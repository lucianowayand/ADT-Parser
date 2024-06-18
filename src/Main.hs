module Main where

import Text.Parsec (parse)
import Parser (parseExpr)
import Inferencer (infer, TypeError(..))
import Types

main :: IO ()
main = do
    let input = "(x, x)"
    case parse parseExpr "" input of
        Left err -> print err
        Right expr -> do
            print expr
            let inferredType = infer iniCont expr
            print inferredType
