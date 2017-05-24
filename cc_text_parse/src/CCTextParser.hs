module CCTextParser where

import Text.ParserCombinators.Parsec
import Data.Functor 
import Numeric

-- output type
data Value = IString String 
           | INumber Double
           | IBool Bool
             deriving (Eq, Ord, Show)

type KVPair = [(String, Value)]
data ItemValue = ID String
               | Type String
               | Audio { attrs :: KVPair, content :: String }
               | Text { attrs :: KVPair, content :: String }
               | Pic { attrs :: KVPair, content :: String }
               | TR String
               | Opts [ItemValue]
                 deriving (Eq, Ord, Show)
-- lexer and parser

p_type :: CharParser () ItemValue
p_type = Type <$> (between (spaces *> char '[' <* spaces)
                           (spaces *> char ']') 
                           (string "TYPE" *> (many1 space) *> (many1 (noneOf " ]"))))

p_pic :: CharParser () ItemValue
p_pic = Pic <$> (string "Pic" *> p_params) <*> p_content

p_audio :: CharParser () ItemValue
p_audio = Audio <$> (string "Audio" *> p_params) <*> p_content

p_tr :: CharParser () ItemValue
p_tr = TR <$> p_content
  
p_params :: CharParser () KVPair
p_params = (between (char '(' <* spaces)
                   (spaces *> string "):")
                   ((p_field <* spaces) `sepBy` (char ',' <* spaces))) <|> (char ':' $> [])
  where p_field = (,) <$> (p_name <* char '=' <* spaces) <*> p_value


p_content :: CharParser () String
p_content = manyTill anyChar (lookAhead eoItem)

p_name :: CharParser () String
p_name = many1 alphaNum

p_value :: CharParser () Value
p_value = choice $ try <$> [ IBool <$> p_bool
                 , INumber <$> p_number
                 , IString <$> p_string_value]

p_bool :: CharParser () Bool
p_bool = (try (True <$ (string "true" <* notFollowedBy (noneOf " ,)")))) <|> False <$ (string "false" <* notFollowedBy (oneOf " ,)"))
-- p_bool = True <$ string "true" <|> False <$ string "false"

p_string_value :: CharParser () String
p_string_value = spaces *> (many1 alphaNum) <* spaces

p_number :: CharParser () Double 
p_number = do 
  s <- getInput
  case readSigned readFloat s of 
       [(n, s')] -> n <$ setInput s' 
       _ -> fail "Not a numer"

eoItem = choice $ try <$> [ string "Pic("
                , string "Pic:"
                , string "Audio:"
                , string "Audio("
                , string "Text:"
                , string "Text("
                , string "[TYPE"
                , string "TR:"
                , string "Opts:"
                , eof $> "EOF"]

