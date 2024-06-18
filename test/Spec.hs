import Parser (parseExpr)
import Test.Hspec
import Text.Parsec (parse)
import Types

main :: IO ()
main = hspec $ do
  describe "Parser.parseExpr" $ do
    it "parses a variable" $ do
      parse parseExpr "" "x" `shouldBe` Right (Var "x")

    it "parses a lambda expression" $ do
      parse parseExpr "" "\\x -> x" `shouldBe` Right (Lam "x" (Var "x"))

    it "parses a let expression" $ do
      parse parseExpr "" "let x = 1 in x" `shouldBe` Right (Let ("x", Lit (LitInt 1)) (Var "x"))

    it "parses a tuple" $ do
      parse parseExpr "" "(x, x)" `shouldBe` Right (Tuple (Var "x") (Var "x"))

    it "parses a case expression" $ do
      parse parseExpr "" "case True of {True -> 1; False -> 2}" `shouldBe` Right (Case (Lit (LitBool True)) [(PLit (LitBool True), Lit (LitInt 1)), (PLit (LitBool False), Lit (LitInt 2))])
