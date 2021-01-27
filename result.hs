module Result where
-- Maybe like monad

data Result a = Result a | Error String

instance Show a => Show (Result a) where
    show (Result a)  = show a
    show (Error str) = "Error: " ++ str

-- Instances up to Monad
instance Functor Result where
    -- fmap :: (a -> b) -> Result a -> Result b
    fmap f r = case r of
        Result a -> Result $ f a
        Error e  -> Error e

instance Applicative Result where
    -- pure :: a -> Result a
    pure v = Result v

    -- <*> :: Result (a -> b) -> Result a -> Result b
    pg <*> px = case pg of
        Result g -> fmap g px
        Error e  -> Error e

instance Monad Result where
    -- (>>=) :: Result a -> (a -> Result b) -> Result b
    p >>= f = case p of
        Result a -> f a
        Error e  -> Error e
