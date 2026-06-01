module Main where

import Derivation.InferExamples
  ( ExpectOutcome(..)
  , InferUnit(..)
  , inferUnits
  , iuExpect
  , iuLabel
  , iuSource
  , iuTrace
  , outcomeTrace
  , runUnit
  )
import Derivation.Trace (InferenceOutcome(..), InferenceResult(..), inferPseudoFunctionSource)
import Parser (parseExp)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), assertFailure, testCase)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "Infer display"
        ( smokeTests
            ++ pseudoFunctionTests
            ++ map unitTest inferUnits
        )
    )

smokeTests :: [TestTree]
smokeTests =
  [ testCase "inferUnits is non-empty" $
      not (null inferUnits) @?= True
  , testCase "all unit sources parse" $
      mapM_ assertParses inferUnits
  ]

pseudoFunctionTests :: [TestTree]
pseudoFunctionTests =
  [ testCase "choose pseudo function infers through web trace path" $
      case inferPseudoFunctionSource chooseSource of
        Left err -> assertFailure ("unexpected parse error: " ++ show err)
        Right (OutcomeInferErr _ err _) ->
          assertFailure ("unexpected inference error: " ++ show err)
        Right (OutcomeOk result) ->
          ("Bool ->" `isInfixOf` show (irScheme result)) @?= True
  ]

chooseSource :: String
chooseSource =
  unlines
    [ "function y = choose(flag, a, b)"
    , "  if flag"
    , "    y = a"
    , "  else"
    , "    y = b"
    , "  end"
    , "end"
    ]

assertParses :: InferUnit -> Assertion
assertParses unit =
  case parseExp (iuSource unit) of
    Left err -> assertFailure ("parse failed for " ++ iuLabel unit ++ ": " ++ show err)
    Right _ -> pure ()

unitTest :: InferUnit -> TestTree
unitTest unit =
  testCase (iuLabel unit ++ " [" ++ iuSource unit ++ "]") $
    assertUnit unit

assertUnit :: InferUnit -> Assertion
assertUnit unit =
  case runUnit unit of
    Left err ->
      assertFailure ("unexpected parse error for " ++ iuLabel unit ++ ": " ++ show err)
    Right outcome -> do
      assertExpect (iuExpect unit) outcome
      assertTraceNeedles (iuTrace unit) (outcomeTrace outcome)

assertExpect :: ExpectOutcome -> InferenceOutcome -> Assertion
assertExpect (ExpectScheme needle) (OutcomeOk result) =
  (needle `isInfixOf` show (irScheme result)) @?= True
assertExpect (ExpectScheme _) _ =
  assertFailure "expected successful inference with matching scheme"
assertExpect (ExpectInferErr needle) (OutcomeInferErr _ err _events) =
  (needle `isInfixOf` show err) @?= True
assertExpect (ExpectInferErr _) _ =
  assertFailure "expected inference error with matching message"

assertTraceNeedles :: [String] -> String -> Assertion
assertTraceNeedles needles traceText =
  mapM_ (assertNeedle traceText) needles

assertNeedle :: String -> String -> Assertion
assertNeedle traceText needle =
  unless (needle `isInfixOf` traceText) $
    assertFailure ("trace missing " ++ show needle ++ " in:\n" ++ traceText)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = go haystack
  where
    go [] = needle == []
    go str@(_ : xs)
      | needle `isPrefixOf` str = True
      | otherwise = go xs

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

unless :: Bool -> IO () -> IO ()
unless condition action =
  if condition
    then pure ()
    else action
