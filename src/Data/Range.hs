module Data.Range
  ( Range(..)
  , member
  , parseRange
  , parseIntegerRange
  , parseIntegerRange'
  ) where

-- base
import Control.Applicative ( (<|>) )
import Data.Bifunctor ( first )
import Data.Void ( Void )

-- megaparsec
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

-- parser-combinators
import Control.Applicative.Combinators ( sepBy )



data Range a
  = Point !a
  | Range !a !a
  | Union ![Range a]
  deriving (Eq, Show)

member :: Ord a => a -> Range a -> Bool
member x (Point y) = x == y
member x (Range l r) = l <= x && x <= r
member x (Union rs) = any (member x) rs




type Parser = MP.Parsec Void String

itemPoint :: Parser a -> Parser (Range a)
itemPoint p = Point <$> p

itemRange :: Parser a -> Parser (Range a)
itemRange p = Range <$> p <*> (MP.char '-' *> p)

item :: Parser a -> Parser (Range a)
item p = MP.try (itemRange p) <|> itemPoint p

-- Without backtracking, but harder to read:
-- item = do
--   l <- int
--   ((Range l <$> (MP.char '-' *> int)) <|> (return (Point l)))

items :: Parser a -> Parser (Range a)
items p = Union <$> (item p `sepBy` MP.char ',')

parseRange :: Parser a -> String -> Either String (Range a)
parseRange p = first MP.errorBundlePretty . MP.parse (items p <* MP.eof) ""


integer :: Parser Integer
integer = MPL.decimal

parseIntegerRange :: String -> Either String (Range Integer)
parseIntegerRange = parseRange integer

parseIntegerRange' :: (Integer -> a) -> String -> Either String (Range a)
parseIntegerRange' f = parseRange (f <$> integer)
