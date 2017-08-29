{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ParionsFDJ.Parse.Outcome
  (
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified HBet.Types as TY
import qualified ParionsFDJ.JSON.Outcome as OC
import ParionsFDJ.Parse.Parsable (Parsable(..))

instance Parsable Text Double where
  parseData t = do
    let sane = sanitize t
    (r, _) <- TR.double sane
    Right r
    where
      sanitize :: Text -> Text
      sanitize =
        T.map
          (\c ->
             if c == ','
               then '.'
               else c)

instance Parsable Text TY.WinOrDraw where
  parseData t =
    case t of
      "1" -> Right TY.W1
      "N" -> Right TY.Draw
      "2" -> Right TY.W2
      _ -> Left $ "Unknown outcome for win or draw: " ++ show t
