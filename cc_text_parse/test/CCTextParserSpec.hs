module CCTextParserSpec where

import Test.Hspec
import CCTextParser
import Text.ParserCombinators.Parsec

spec :: Spec
spec = do
  describe "CC Text Parser" $ do
    describe "parse bool" $ do 
      it "can parse 'true' " $ do
        parse p_bool "(unknown)" "true\n" `shouldBe` return True

      it "can parse 'false' " $ do 
        parse p_bool "(unknown)" "false\n" `shouldBe` return False

      it "can parse 'true ' " $ do 
        parse p_bool "(unknown)" "true " `shouldBe` return True

      it "can parse 'true,' " $ do 
        parse p_bool "(unknown)" "true," `shouldBe` return True

      it "can parse 'true)' " $ do 
        parse p_bool "(unknown)" "true)" `shouldBe` return True

      it "can parse 'false ' " $ do 
        parse p_bool "(unknown)" "false " `shouldBe` return False

      it "can parse 'false,' " $ do 
        parse p_bool "(unknown)" "false," `shouldBe` return False

      it "can parse 'false)' " $ do 
        parse p_bool "(unknown)" "false)" `shouldBe` return False

      it "can't parse 'falsetrue' to False" $ do 
        parse p_bool "(unknown)" "falsetrue" `shouldNotBe` return False

      it "can't parse 'truefalse' to True" $ do 
        parse p_bool "(unknown)" "truefalse" `shouldNotBe` return True

      context "when parse 'true,' success" $ do 
        it "did not eat tail char ','" $ do 
          parse (p_bool *> anyChar) "(unknown)" "true," `shouldBe` return ','

      context "when parse 'falsetrue' failed" $ do 
        it "did not eat any letters" $ do 
          parse (p_bool <|> (("falsetrue" ==) <$> (many anyChar))) "(unknown)" "falsetrue" `shouldBe` return True
    
      context "when parse 'truefalse' failed" $ do 
        it "did not eat any letters" $ do 
          parse (p_bool <|> (("truefalse" ==) <$> (many anyChar))) "(unknown)" "truefalse" `shouldBe` return True

      context "when 'true' is at the end of file" $ do
        it "can parse to True" $ do 
          pending

    describe "parse params key" $ do 
      it "can parse 'abcABC' " $ do 
        pending

      it "can parse 'abc123' " $ do 
        pending

      it "should throw an error" $ do 
        pending

