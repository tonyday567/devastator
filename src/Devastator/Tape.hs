{-# LANGUAGE OverloadedStrings #-}

-- | The generic devastator tape codec.
--
-- Every devastator tape is JSON Lines of stamped posts whose bodies
-- serialise through the 'Framed' class — one framing discipline for all
-- systems, with the per-body codecs living beside the bodies themselves.
module Devastator.Tape
  ( frameTape,
    readTape,
  )
where

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (Stamped, These (..), frameStored)
import Circuit.Traced (Unfold (..), (:.) (..))
import Devastator.Framing (Jsonl (..), uncons)
import Devastator.Run (Framed (..))

-- | Convert a stamped run into a JSONL tape.
frameTape :: (Framed b) => [Stamped b] -> Jsonl
frameTape = Jsonl . map (frameStored . fmap encodePostBody)
  where
    encodePostBody p = p {body = frameBody (body p)}

-- | Read a JSONL tape back into a stamped run.
readTape :: (Framed b) => Jsonl -> [Stamped b]
readTape = unfold gen
  where
    gen j = Nest $ case uncons j of
      These sp rest -> Just (rest, fmap decodePostBody sp)
      This sp -> Just (Jsonl [], fmap decodePostBody sp)
      That _ -> Nothing
    decodePostBody p = case unframeBody (body p) of
      Just b -> p {body = b}
      Nothing -> error "readTape: malformed body"
