{-# LANGUAGE OverloadedStrings #-}

-- | Local framing for devastator tapes.
--
-- 'Circuit.Agent.Framing' once exported the 'Jsonl' stream newtype; upstream
-- moved to the 'Log' and @[Text]@ images, so the devastator keeps its own.
-- A tape is one framed post per line, and 'uncons' peels a parsed, stamped
-- line at a time. Pure integration tapes carry no real clock, so they stamp
-- every post at 'epoch' — the successor of the old empty timestamp.
module Devastator.Framing
  ( Jsonl (..),
    uncons,
    epoch,
  )
where

import Circuit.Agent.Framing (Stamped, These (..), unframeStored)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Time.Clock (secondsToDiffTime)

-- | JSON Lines image of a tape: one framed post per line.
newtype Jsonl = Jsonl {unJsonl :: [Text]}
  deriving (Eq, Show)

-- | Peel the first parseable line, skipping any that do not frame.
uncons :: Jsonl -> These (Stamped Text) Jsonl
uncons (Jsonl []) = That (Jsonl [])
uncons (Jsonl (l : ls)) = case unframeStored l of
  Just sp
    | null ls -> This sp
    | otherwise -> These sp (Jsonl ls)
  Nothing -> uncons (Jsonl ls)

-- | Fixed stamp time for pure integration tapes.
epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
