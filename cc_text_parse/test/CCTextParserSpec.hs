module CCTextParserSpec where

import Test.Hspec
import CCTextParser
import Text.ParserCombinators.Parsec

spec :: Spec
spec = do
  describe "CC Text Parser" $ do
    describe "parse bool" $ do 
      it "can parse 'true' " $ do
        parse p_bool "(unknown)" "true" `shouldBe` return True

      it "can parse 'false' " $ do 
        parse p_bool "(unknown)" "false" `shouldBe` return False

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
        parse p_bool "(unknown)" "falsetrue" `shouldSatisfy` ((/=) (return False))

      it "can't parse 'truefalse' to True" $ do 
        parse p_bool "(unknown)" "truefalse" `shouldSatisfy` ((/=) (return True))
    
    describe "parse params key" $ do 
      it "can parse 'abcABC' " $ do 
        pending

      it "can parse 'abc123' " $ do 
        pending

      it "should throw an error" $ do 
        pending
