# Haskell parser generator
A Shift-Reduce haskell parser and lexer generator from yaml-like syntax

## Usage
This project must be run on a `gmr` file, in the format specified [here](#gmr-format)
### Compiled
The compiled script can be run taking one argument, the path to the `gmr` file. A `hs` file of the same name will be generated in the same place as the `gmr` file.
`./parsergenerator "myParser.gmr"`

### Interpreted
The following functions have been defined in the `ParserGenerator` module.
- `runParserGenerator :: String -> IO ()` taking path and building the `hs` file
- `generateParser :: String -> String -> Result String`, turning a `gmr` file contents and module name into the parser code

### Generated file
The output file of this parser generator exports a `runParser` function, and the `Result` module. The type signature is as follows:
`runParser :: String -> Result output` where output is the type of the first rule's reduction.

## gmr format
### Prelim info
The following types are used in the `gmr` file:
- CodeBlock - A curly bracket enclosed section of haskell code, as follows:
  ```
  {
    test :: Int -> Int
    test = (2*)
  }
  ```
  `{ x + 2 }`
- Directive - A lowercase string prefixed by a `%`, such as `%token`, `%left`, `%empty`.
- Strings - These are enclosed by single or double quotes, they have no distinction.
- Comments - Single line comments in `gmr` files look like `# this`. Multiline comments `#[ look like this ]#`

### Overall structure
The structure of the `gmr` file is as follows, with each section explained further down
1. %exports directive - This optional directive takes a CodeBlock of names to be exported from the generated parser
2. %precode directive - This optional directive takes a CodeBlock of haskell code to be copied to the start of the parser
3. [Scanner specification](#scanner-specificiation) - List of scanner directives (can be 0) such as `%operators "+" "-"`.
4. [Optional Token patterns](#token-patterns) - Map from terminal names to haskell patterns to match. Used for custom terminals when using the `%extraparser` directive with `TokenCustom`
5. [Optional Precedence and associativity definitions](#precedence-and-associativity) - Identical stucture to YAML or Happy, list of levels defining associativity and tokens.
6. [Rules](#rules) - List of productions defined as Nonterminals mapping to lists of terminals/nonterminals and a codeblock result for the reduction.

### Scanner Specificiation
This is a list of directives defining behaviours of the scanner.
- `%operators op1 op2 ...` - List of space separated string literal operators, e.g. `%operators "::" "|" "*" "+" "?"`
- `%keywords kwd1 kwd2 ...` - List of space separated string literal keywords, e.g. `%keywords "for" "in" "if" "then"`
- `%lineComments commentStr` - Defines the single line comment starting string, e.g. `%linecomments "#"`
- `%blockcomments openStr closeStr` - Defines the opening and closing strings for block comments, e.g. `%blockcomments "#[" "]#"`
- `%separateidentitycase` - Flag that causes camelCase and PaskalCase identifiers to be recognised separately.
- `%keepwhitespaces` - Flag that causes the scanner to include whitespace tokens.
- `%keepcomments` - Flag that causes the scanner to include comment tokens, separate for single line and block comments.
- `%parsermap` - This can be used to modify the monadic parser stack used by the scanner.  
  Expects a function of type `[Parser TokenType] -> [Parser TokenType]`. More information on how to do this [here](#using-parsermap)

### Token Patterns
This defines any extra terminals you wish to use in your grammar. All standard terminals are already defined for you, and are listed at the end of this section.  
You should use this directive if you make use of the `%parsermap` directive and create any `TokenCustom` tokens.  
The structure for the `%token` directive is as follows:
```
%token name1   { haskellPattern1 }
       "name2" { haskellPattern2 }
```
Formally defined as a list of identifiers or string literals followed by a CodeBlock.  
When handling reductions, the terminals value will be equal to the entire pattern within the CodeBlock, unless the `$$` tag is used. When this is used, the value will be whatever is in place of the tag. For example, the default definition for `stringLit` is `{ TokenStrLit $$ }`, meaning the value will be the contents of the `TokenStrLit` constructor, rather than the entire `TokenType` object.  
Each terminal may only be defined once, however if you use a name that already exists in the default tokens, your defintion will overwrite it.  
The default token definitions are as follows:
- Operators, every operator has their respective string literal defined. Meaning, with a scanner specification of `%operators "+" "-"`, the terminals `"+"` and `"-"` are defined.
- Keywords, similar to operators, all keywords have their respective lower identifier defined as keywords.
- Brackets, `( ) { } [ ]` brackets are defined as string literals of themselves.
- "identifier" - "TokenIdentifier $$"
- "upperIdentifier" - "TokenUpperIdentifier $$"
- "stringLit" - "TokenStringLit $$"
- "integerLit" - "TokenIntLit $$"
- "floatLit" - "TokenFloatLit $$"
- "blockComment" - "TokenBlockComment $$"
- "lineComment" - "TokenLineComment $$"
- "whitespace" - "TokenWhitespace $$"

### Precedence and Associativity
This is an optional list of precedence levels, each defining an associativity and assigned tokens.  
The higher up the level is in the list, the high its precedence.  
The 3 associativity types are `%left`, `%right` and `%nonassoc`  
A simple precedence example is as follows:
```
%nonassoc ">" "<"
%left "+" "-"
%left "*" "/"
```

### Rules
A `gmr` file requires at least one Rule, defined in the following format:
```
MyRule :: token1 token2 ... tokenN { reductionHaskellCode }
        | token1                   { reductionHaskellCode2 }
```
Each rule defines 1 or more productions with respective reduction code, separated by pipes. When a production is matched, it will be replaced with the code in the CodeBlock after it, therefore all CodeBlocks for a given rule must have the same type.  
Each token in the rule will be assigned to `v1, v2, ... vN` in the CodeBlock, for example:
```
# v1 will be the result of the first ExprPrimary
# v2 will be a TokenOperator "*"
# v3 will be the second ExprPrimary
Expr :: ExprPrimary '*' ExprPrimary { ( v1, v2, v3 ) }
```
For any terminals in the rule definition, `psN` variable will be defined along-side `vN`, this will be the [ParseState](#parsestate-type) of that terminal, containing scanner information about where that token was in the original string.  
```
# Note that ps1 and ps3 do not exist here, as Expr is a nonterminal.
# This is the case even if Expr simply reduced to a terminal
# psN vars only exist for immediate terminals in the rule
MyRule :: Expr "+" Expr { ( v1, ( v2, ps2 ), v3 ) }
```
A rule token is defined as a terminal or nonterminal, followed by an optional modifier. A terminal can be either a lower case identifier, or a string literal.  
A nonterminal, or rule, is defined as a PaskelCase identifier.  
There are 3 modifiers supported:
- `?` - this modifier makes a token optional, changing the vN type from `a` to `Maybe a`
  ```
  # Simple assignment production with optional type
  MyRule :: identifier? identifier '=' Expr { assignment{ type=(fromMaybe "Int" v1), name=v2, val=v4 } }
  ```
- `*`, this modifier matches 0 or more of a token, change the type from `a` to `[a]`.
- `+`, this modifier is the same as `*`, but matches 1 or more of a token.
Both `*` and `+` support a separator token, by following the modifier by a token in brackets. This token can be a terminal or non terminal, but the values of this will be lost.
```
# An array production matching a list of Expr's seperated by commas
Array :: "[" Expr*(",") "]" { array{ vals=v2 } }
```
  
The precedence/associativity of a rule is copied from the last terminal in the production, as specified [here](#precedence-and-associativity).  
If you wish for the rule to take the precedence of a different token, you can use the `%prec` directive at the end of the tokens, but before the CodeBlock, followed by the token to copy from.
```
# This rule will copy the precedence and assocativity of the "*" terminal, rather than the "+".
# Note that the "*" terminal is not part of this production.
MyRule :: identifier "+" OtherRule %prec "*" { ( v1, v2 ) }
```
  
The first rule in the list will be the top of the syntax tree.

## Extra notes
The following two types are defined within [this file](https://github.com/samuelWilliams99/haskell_parser_generator/blob/main/parserrequirements.hs)
### Result type
The `Result` data type (defined [here](https://github.com/samuelWilliams99/haskell_parser_generator/blob/main/parserrequirements.hs#L154)) is very similar to the `Maybe` type, but includes an error string in the failure constructor. It contains identical instances for `Functor`, `Applicative` and `Monad`

### ParseState type
The `ParseState` data type (defined [here](https://github.com/samuelWilliams99/haskell_parser_generator/blob/main/parserrequirements.hs#L102)) contains the following information about the scanner's state at any given time.
- Line number (line)
- Column number (column)
- Input string position (pos)
- Full input code (code)
- Current remaining input (rest)
The function `showPos` takes a ParseState and converts it to a human readable input string position, intended to be appended to the end of an error string.  
```
> "Unexpected token: TokenOperator \"*\" at " ++ showPos someParseState
Unexpected token: TokenOperator "*" at Line 4, Column 15
    %left "+" "-" *
                  ^
```

### gmr Parser
The parser for this parser generator was generated by itself, and it's full definition is found in [cfgparser.gmr](https://github.com/samuelWilliams99/haskell_parser_generator/blob/main/cfgparser.gmr)

### Using ParserMap
The parser map works by modifying a list of `TokenType` `Parsers`, the default definition of this list is:
1. String literals
2. Block comments
3. Line comment
4. Operators
5. Brackets
6. Keywords
7. Identifiers
8. Int literals
9. Float literals

Since these Parsers are all expected to return a TokenType, there is a `TokenCustom String String` constructor in TokenType. It is expected that the first string is a type, and the second is data. This is used for the gmr parser for CodeBlocks and Directives, where parsers for both these types were made, then simply consed to the parser list.  
For more information on `Parsers`, the [Parsing](https://github.com/samuelWilliams99/haskell_parser_generator/blob/main/parserrequirements.hs#L176) module defines several examples, and was modified from the library discussed in [this video](https://www.youtube.com/watch?v=dDtZLm7HIJs&ab_channel=Computerphile), by Graham Hutton.  
