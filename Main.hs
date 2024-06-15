module Main where

import Text.Parsec (parse)
import Parser (parseExpr)
import Inferencer (infer, TypeError(..))
import Types

main :: IO ()
main = do
    let input = "\\x. let y = 1 in if y then True else False"
    case parse parseExpr "" input of
        Left err -> print err
        Right expr -> do
            print expr
            let inferredType = infer iniCont expr
            print inferredType
