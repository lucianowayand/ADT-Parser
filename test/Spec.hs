import Parser (parseExpr)
import Test.Hspec
import Text.Parsec (parse)
import Types

main :: IO ()
main = hspec $ do
  describe "Parser.parseExpr" $ do
    it "parses a lambda expression" $ do
      parse parseExpr "" "\\x. x" `shouldBe` Right (Lam "x" (Var "x"))

    it "parses application" $ do
      parse parseExpr "" "x x" `shouldBe` Right (App (Var "x") (Var "x"))

    it "parses a variable" $ do
      parse parseExpr "" "x" `shouldBe` Right (Var "x")

    it "parses a let expression" $ do
      parse parseExpr "" "let x = 1 in x" `shouldBe` Right (Let ("x", Lit (LitInt 1)) (Var "x"))

    it "parses a case expression" $ do
      parse parseExpr "" "case True of {True -> 1; False -> 2}" `shouldBe` Right (Case (Lit (LitBool True)) [(PLit (LitBool True), Lit (LitInt 1)), (PLit (LitBool False), Lit (LitInt 2))])

    it "parses a if expression" $ do
      parse parseExpr "" "if x then 1 else 2" `shouldBe` Right (If (Var "x") (Lit (LitInt 1)) (Lit (LitInt 2)))

    it "parses a tuple" $ do
      parse parseExpr "" "(x, x)" `shouldBe` Right (Tuple (Var "x") (Var "x"))

    it "parses a parens expression" $ do
      parse parseExpr "" "(x)" `shouldBe` Right (Var "x")

    it "parses a literal integer" $ do
      parse parseExpr "" "1" `shouldBe` Right (Lit (LitInt 1))

    it "parses a literal boolean" $ do
      parse parseExpr "" "True" `shouldBe` Right (Lit (LitBool True))