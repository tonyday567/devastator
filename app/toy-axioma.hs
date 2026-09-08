{-# LANGUAGE OverloadedStrings #-}

-- | First devastator axioma: scalar ODE, swappable nonlinearity, two
-- certificates, three verdicts.
module Main (main) where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped, stamped)
import Devastator.Cert (maxValueCert, trivialCert)
import Devastator.Tape (frameMeeting, readMeeting)
import Devastator.Toy (ToyBody (..), integrate, nullOp, trueOp)
import Devastator.Verdict (Verdict (..), verdictAbs, verdictRel)
import System.Exit (exitFailure)

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitFailure

approx :: Double -> Double -> Double -> Bool
approx tol a b = abs (a - b) < tol * (1 + abs a + abs b)

bodyValues :: [Stamped ToyBody] -> [ToyBody]
bodyValues = map (body . stamped)

main :: IO ()
main = do
  putStrLn "devastator-toy-axioma: scalar ODE nonlinearity-swap filter"

  let trueLog = integrate trueOp
      nullLog = integrate nullOp
  putStrLn ("  true log length: " ++ show (length trueLog))
  putStrLn ("  null log length: " ++ show (length nullLog))

  putStrLn "ODE sanity"
  let finalTrue = tbValue . last $ bodyValues trueLog
      maxTrue = maximum (map abs . map tbValue $ bodyValues trueLog)
      maxNull = maximum (map abs . map tbValue $ bodyValues nullLog)
  assert "true final u approx exp(-0.9)" $ approx 2e-3 finalTrue (exp (-0.9))
  assert "true max |u| stays near 1" $ maxTrue < 1.01
  assert "null max |u| blows up past 5" $ maxNull > 5.0

  putStrLn "round-trip"
  let trueBack = readMeeting (frameMeeting trueLog)
      nullBack = readMeeting (frameMeeting nullLog)
  assert "true round-trip bodies match" $ bodiesMatch trueBack trueLog
  assert "null round-trip bodies match" $ bodiesMatch nullBack nullLog

  putStrLn "certificates"
  let vTrivTrue = trivialCert trueLog
      vTrivNull = trivialCert nullLog
      vMaxTrue = maxValueCert trueLog
      vMaxNull = maxValueCert nullLog
  putStrLn ("  trivial true/null: " ++ show vTrivTrue ++ " / " ++ show vTrivNull)
  putStrLn ("  max true/null: " ++ show vMaxTrue ++ " / " ++ show vMaxNull)
  assert "trivialCert is SKELETON" $
    verdictAbs 1e-6 vTrivTrue vTrivNull == Skeleton
  assert "maxValueCert is SEPARATING" $
    verdictRel 1e-3 vMaxTrue vMaxNull == Separating

  putStrLn "ALL PASS"
  where
    bodiesMatch back orig =
      length back == length orig
        && all bodyMatch (zip back orig)
    bodyMatch (s1, s2) =
      let b1 = body (stamped s1)
          b2 = body (stamped s2)
       in approx 1e-12 (tbTime b1) (tbTime b2)
            && approx 1e-12 (tbValue b1) (tbValue b2)
            && approx 1e-12 (tbFlux b1) (tbFlux b2)
