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
- `generattorParser :: String -> String -> Result String`, turning a gmr file contents and module name into the parser code

## GMR format
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
- Comments - Single line comments in gmr files look like `# this`. Multiline comments `#[ look like this ]#`

### Overall structure
The structure of the gmr file is as follows, with each section explained further down
1. Optional CodeBlock - Placed directly at the start of the generated file
2. [Scanner specification](#scanner-specificiation) - List of scanner directives (can be 0) such as `%operators "+" "-"`.
3. [Optional Token patterns](#token-patterns) - Map from terminal names to haskell patterns to match. Used for custom terminals when using the `%extraparser` directive with `TokenCustom`
4. [Optional Precedence and associativity definitions](#precedence-and-associativity) - Identical stucture to Yaml or Happy, list of levels defining associativity and tokens.
5. [Rules](#rules) - List of productions defined as Nonterminals mapping to lists of terminals/nonterminals and a codeblock result for the reduction.

### Scanner Specificiation
This is a list of directives defining behaviours of the scanner.
- `%operators op1 op2 ...` - List of space separated string literal operators, e.g. `%operators "::" "|" "*" "+" "?"`
- `%keywords kwd1 kwd2 ...` - List of space separated string literal keywords, e.g. `%keywords "for" "in" "if" "then"`
- `%lineComments commentStr` - Defines the single line comment starting string, e.g. `%linecomments "#"`
- `%blockcomments openStr closeStr` - Defines the opening and closing strings for block comments, e.g. `%blockcomments "#[" "]#"`
- `%separateidentitycase` - Flag that causes camelCase and PaskalCase identifiers to be recognised separately.
- `%keepwhitespaces` - Flag that causes the scanner to include whitespace tokens.
- `%keepcomments` - Flag that causes the scanner to include comment tokens, separate for single line and block comments.
- `%parsermap` - This can be used to modify the monadic parser stack used by the scanner. Expects a function of type `[Parser TokenType] -> [Parser TokenType]`. More information on how to do this [here](#using-parsermap)

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
TODO

## Extra notes
### Result type
[This file](https://github.com/samuelWilliams99/haskell_parser_generator/blob/main/result.hs) defines the `Result` data type, which is very similar to the `Maybe` type, but includes an error string in the failure constructor. It contains identical instances for `Functor`, `Applicative` and `Monad`

### GMR Parser
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
Since these Parsers are all expected to return a TokenType, there is a `TokenCustom String String` constructor in TokenType. It is expected that the first string is a type, and the second is data. This is used for the GMR parser for CodeBlocks and Directives, where parsers for both these types were made, then simply consed to the parser list.  
For more information on `Parsers`, the [Parsing](https://github.com/samuelWilliams99/haskell_parser_generator/blob/main/parsing.hs) module defines several examples, and was modified from the library discussed in [this video](https://www.youtube.com/watch?v=dDtZLm7HIJs&ab_channel=Computerphile), by Graham Hutton.  
