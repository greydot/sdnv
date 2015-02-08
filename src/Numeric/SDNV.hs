{-# Language MultiWayIf #-}
{-# Language ScopedTypeVariables #-}
module Numeric.SDNV (SDNV
                    ,encodeSDNV
                    ,decodeSDNV
                    ,safeDecodeSDNV
                    ,isValidSDNV
                    ) where

import Control.Applicative
import Data.Binary
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Short (ShortByteString, pack, unpack, toShort, fromShort)
import Data.List (maximum)

newtype SDNV a = SDNV ShortByteString deriving (Eq)

instance Binary (SDNV a) where
    put (SDNV bs) = mapM_ put $ unpack bs
    get = do
        f <- get
        SDNV . pack . (f:) <$> if
            | f < 0x80 -> pure []
            | f > 0x80 -> let g v
                                | v >= 0x80 = (v:) <$> (g =<< get)
                                | otherwise = pure (v:[])
                          in get >>= g
            | otherwise -> fail "Invalid SDNV"

encodeSDNV :: (Bits a, Integral a) => a -> SDNV a
encodeSDNV = SDNV . pack . encode []
    where
        encode vs 0 = vs
        encode vs n = let v = fromIntegral (n .&. 0x7F) .|. (if null vs
                                                             then 0x00
                                                             else 0x80)
                          nv = n `shiftR` 7
                      in encode (v:vs) nv

decodeSDNV :: (Bits a, Integral a) => SDNV a -> a
decodeSDNV (SDNV bs) = foldl decode 0 . unpack $ bs
    where
        decode r v = let dv = fromIntegral (v .&. 0x7F)
                     in (r `shiftL` 7) .|. dv

safeDecodeSDNV :: (Bits a, Integral a) => SDNV a -> Maybe a
safeDecodeSDNV s
    | isValidSDNV s = return $ decodeSDNV s
    | otherwise = Nothing

isValidSDNV :: forall a. (Bits a, Integral a) => SDNV a -> Bool
isValidSDNV (SDNV bs) = let s = fromShort bs
                        in validateData s && validateSize (B.head s) (B.length s)
    where
        validateData s = case B.length s of
                            1 -> validateLast (B.last s)
                            2 -> validateFirst (B.head s) && validateLast (B.last s)
                            _ -> validateFirst (B.head s) && validateLast (B.last s) && B.all validateMiddle (B.tail . B.init $ s)
        validateFirst = (0x80 <)
        validateMiddle = (0x80 <=)
        validateLast = (< 0x80)
        -- This check is not applicable to variable length types such as Integer,
        -- thus it defaults to True if bitSizeMaybe returns Nothing.
        validateSize h l = let hl = 1 + maximum (filter (testBit h) [0..6])     -- Head length in bits
                               tl = (l - 1) * 7                                 -- Tail length in bits
                               z = zeroBits :: a
                           in maybe True (hl + tl <=) (bitSizeMaybe z)
