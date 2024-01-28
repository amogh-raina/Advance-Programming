import Types
import Parser (parseExp)
import Calculator (evaluate, expectation)

import Data.List (intercalate)
import System.Exit (die)
import System.Environment (getArgs)
import Text.Printf
import Data.Ratio

pretty :: Bag -> String
pretty (Bag [n]) = show n
pretty (Bag l) = "(" ++ intercalate "," (map show l) ++ ")"

stats :: PD (Either RunError Bag) -> String
stats d =
  let l = runPD d
      m = foldl1 lcm [denominator p | (p,_) <- l]
      shl (p,v) =
        printf " %*d/%d (%7.3f%%): %s\n" 
               (length (show m))
               (numerator (p * fromIntegral m))
               m
               (fromRational p * 100 :: Double)
               (case v of Left s -> show s; Right b -> pretty b)
  in concatMap shl l

expVal :: PD (Either a Bag) -> String
expVal d =
  let t = do a <- d
             case a of
               Right (Bag [n]) -> return (Just (fromIntegral n :: Double))
               _ -> return Nothing
      (sp,v) = expectation t
      fps = if sp == 1 then ""
            else printf " (with failure probability %3.1f%%)"
                        (fromRational (1-sp) * 100 :: Double)
  in if sp == 0 then "N/A" else printf "%8.6f" v ++ fps


main :: IO ()
main = do
  as <- getArgs
  a <- case as of
         [a] -> return a
         _ -> die $ "Usage: droll \"expression\""
  exp <- case parseExp a of
           Right e -> return e
           Left err -> die $ "Parsing error: " ++ err
  let d = evaluate exp
  putStr $ "Result distribution:\n" ++ stats d
  putStrLn $ "Expected value: " ++ expVal d
