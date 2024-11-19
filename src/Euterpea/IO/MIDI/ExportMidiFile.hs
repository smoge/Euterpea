module Euterpea.IO.MIDI.ExportMidiFile (exportMidiFile) where

import Codec.Midi
import qualified Data.ByteString as Byte
import Data.Char
import Numeric

makeFile :: Midi -> Byte.ByteString
makeFile (Midi ft td trs) =
  let ticksPerQn =
        case td of
          TicksPerBeat x -> x
          TicksPerSecond x y ->
            error
              ( "(makeFile) Don't know how "
                  ++ "to handle TicksPerSecond yet."
              )
      header = makeHeader ft (length trs) ticksPerQn
      body = map makeTrack trs
   in Byte.concat (header : body)

midiHeaderConst :: Byte.ByteString
midiHeaderConst =
  Byte.pack [0x4D, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06]

type TrackCount = Int

type TicksPerQN = Int

makeHeader :: FileType -> TrackCount -> TicksPerQN -> Byte.ByteString
makeHeader ft numTracks ticksPerQn =
  let ft' = case ft of
        SingleTrack -> [0x00, 0x00]
        MultiTrack -> [0x00, 0x01]
        MultiPattern ->
          error
            ( "(makeHeader) Don't know "
                ++ "how to handle multi-pattern yet."
            )
      numTracks' = padByte 2 numTracks
      ticksPerQn' = padByte 2 ticksPerQn
   in if numTracks > 16
        then
          error
            ( "(makeHeader) Don't know how to "
                ++ "handle >16 tracks!"
            )
        else Byte.concat [midiHeaderConst, Byte.pack ft', numTracks', ticksPerQn']

padByte :: (Integral a) => Int -> a -> Byte.ByteString
padByte byteCount i =
  let b = Byte.pack [fromIntegral i]
      n = Byte.length b
      padding = Byte.pack $ replicate (byteCount - n) 0x00
   in if n < byteCount then Byte.concat [padding, b] else b

makeTrack :: Track Ticks -> Byte.ByteString
makeTrack t =
  let body = makeTrackBody t
      header = makeTrackHeader body
   in Byte.concat [header, body]

trackHeaderConst :: Byte.ByteString
trackHeaderConst = Byte.pack [0x4D, 0x54, 0x72, 0x6B]

makeTrackHeader :: Byte.ByteString -> Byte.ByteString
makeTrackHeader tbody =
  let len = Byte.length tbody
      f =
        Byte.pack
          . map (fromIntegral . binStrToNum . reverse)
          . breakBinStrs 8
          . pad (8 * 4) '0'
          . numToBinStr
   in Byte.concat [trackHeaderConst, f len]

makeTrackBody :: Track Ticks -> Byte.ByteString
makeTrackBody [] = endOfTrack -- end marker, very important!
makeTrackBody ((ticks, msg) : rest) =
  let b = msgToBytes msg
      b' = [to7Bits ticks, msgToBytes msg, makeTrackBody rest]
   in if Byte.length b > 0
        then Byte.concat b'
        else makeTrackBody rest

endOfTrack :: Byte.ByteString
endOfTrack = Byte.concat [to7Bits (96 :: Integer), Byte.pack [0xFF, 0x2F, 0x00]]

to7Bits :: (Integral a) => a -> Byte.ByteString
to7Bits =
  Byte.pack
    . map (fromIntegral . binStrToNum . reverse)
    . fixBinStrs
    . map (padTo 7 . reverse)
    . reverse
    . breakBinStrs 7
    . reverse
    . padTo 7
    . numToBinStr

padTo :: Int -> String -> String
padTo i xs = if length xs `mod` i == 0 then xs else padTo i ('0' : xs)

breakBinStrs :: Int -> String -> [String]
breakBinStrs i s = if length s <= i then [s] else take i s : breakBinStrs i (drop i s)

numToBinStr :: (Integral a) => a -> String
numToBinStr i = showIntAtBase 2 intToDigit i ""

binStrToNum :: String -> Int
binStrToNum [] = 0
binStrToNum ('0' : xs) = 2 * binStrToNum xs
binStrToNum ('1' : xs) = 1 + 2 * binStrToNum xs
binStrToNum _ = error "bad data."

fixBinStrs :: [String] -> [String]
fixBinStrs xs =
  let n = length xs
      bits = replicate (n - 1) '1' ++ "0"
   in Prelude.zipWith (:) bits xs

pad :: Int -> a -> [a] -> [a]
pad b x xs = if length xs >= b then xs else pad b x (x : xs)

msgToBytes :: Message -> Byte.ByteString
msgToBytes (NoteOn c k v) =
  Byte.concat [Byte.pack [0x90 + fromIntegral c], padByte 1 k, padByte 1 v]
msgToBytes (NoteOff c k v) =
  Byte.concat [Byte.pack [0x80 + fromIntegral c], padByte 1 k, padByte 1 v]
msgToBytes (ProgramChange c p) =
  Byte.concat [Byte.pack [0xC0 + fromIntegral c], padByte 1 p]
msgToBytes (ControlChange c n v) =
  Byte.concat [Byte.pack [0xB0 + fromIntegral c], padByte 1 n, padByte 1 v]
msgToBytes (TempoChange t) =
  -- META EVENT, HAS NO CHANNEL NUMBER
  Byte.concat [Byte.pack [0xFF, 0x51, 0x03], fixTempo t]
msgToBytes x =
  error
    ( "(msgToBytes) Message type not currently "
        ++ "supported: "
        ++ show x
    )

fixTempo :: Tempo -> Byte.ByteString
fixTempo =
  Byte.pack
    . map (fromIntegral . binStrToNum . reverse)
    . breakBinStrs 8
    . pad (4 * 6) '0'
    . numToBinStr

exportMidiFile :: FilePath -> Midi -> IO ()
exportMidiFile fn = Byte.writeFile fn . makeFile
