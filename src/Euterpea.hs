{-# OPTIONS -XFlexibleInstances #-}
{-# OPTIONS -XTypeSynonymInstances #-}

module Euterpea
  ( module Euterpea.Music,
    module Euterpea.IO.Audio,
    module Euterpea.IO.MIDI,
    module Control.Arrow,
    -- This next line is from Codec.Midi
    exportFile,
    importFile,
  )
where

import Codec.Midi (exportFile, importFile)
import Control.Arrow
import Euterpea.IO.Audio
import Euterpea.IO.MIDI
import Euterpea.Music

-- import Control.Arrow.Operations
