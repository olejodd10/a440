module Main where
import Sound.PortMidi
import Data.Maybe
import Control.Concurrent

-- https://hackage.haskell.org/package/PortMidi-0.1.6.1/docs/Sound-PortMidi.html#v:PMStream
-- https://www.midi.org/specifications-old/item/table-2-expanded-messages-list-status-bytes

-- Note that channel is 1-indexed!
noteOn :: Int -> Int -> Int -> PMMsg
noteOn channel key velocity = PMMsg { status = fromIntegral $ 143 + channel, data1 = fromIntegral key, data2 = fromIntegral velocity }

noteOff :: Int -> Int -> Int -> PMMsg
noteOff channel key velocity = PMMsg { status = fromIntegral $ 127 + channel, data1 = fromIntegral key, data2 = fromIntegral velocity }

a440On :: PMMsg
a440On = noteOn 1 69 127

a440Off :: PMMsg
a440Off = noteOff 1 69 127

connect :: IO (Either PMError PMStream)
connect = do
    deviceId <- fmap (fromMaybe 0) getDefaultOutputDeviceID
    openOutput deviceId 0 -- last argument is latency. Timestamps on events are ignored if 0. Otherwise it is used to determine when events happen

main :: IO ()
main = do
    conn <- connect
    case conn of 
        Left error -> putStr "Error connecting to MIDI output device: " >> print error
        Right stream -> do 
            writeShort stream PMEvent {message = encodeMsg a440On, timestamp = 0}
            threadDelay 3000000
            writeShort stream PMEvent {message = encodeMsg a440Off, timestamp = 0} 
            threadDelay 500000 -- give time for noteOff to end the note smoothly
            close stream
            return ()
    return ()

