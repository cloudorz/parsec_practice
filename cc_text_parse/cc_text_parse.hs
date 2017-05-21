import Text.ParserCombinators.Parsec

-- output type
data Value = IString String 
           | IInt Int
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
p_type = Type <$> (between (spaces *> char '[' <* spaces) (spaces *> char ']') (string "TYPE" *> (many1 space) *> (many1 (noneOf " ]"))))
