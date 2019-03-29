module Main (main) where

import qualified Test.Data.ByteString.Base32     as Base32
import           Test.Tasty
import           Test.Tasty.Ingredients.FailFast

main :: IO ()
main = do
    defaultMainWithIngredients (map failFast defaultIngredients) $ testGroup "All"
        [ Base32.tests ]
