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
        pending

      it "can parse 'true,' " $ do 
        pending

      it "can parse 'true)' " $ do 
        pending

      it "can parse 'false ' " $ do 
        pending

      it "can parse 'false,' " $ do 
        pending

      it "can parse 'false)' " $ do 
        pending

      context "when parse 'falsetrue' " $ do
        it "should throw an error" $ do 
          pending

      context "when parse 'truefalse' " $ do
        it "should throw an error" $ do 
          pending
    
    describe "parse params key" $ do 
      it "can parse 'abcABC' " $ do 
        pending

      it "can parse 'abc123' " $ do 
        pending

      it "should throw an error" $ do 
        pending
