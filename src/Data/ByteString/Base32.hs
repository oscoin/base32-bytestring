-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Efficient encoding and decoding of base32 encoded bytestring
--   according to RFC 4648. <http://tools.ietf.org/html/rfc4648>
--
--   This module recommended to be imported as
--   @import Data.ByteString.Base32 as Base32@ to avoid name clashes
--   with @Data.Binary@ or @Data.ByteString.Base64@ modules.
--
{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Base32
       ( Base32
       , encode
       , decode
       , decodeLenient

       , encTable
       , decTable
       ) where

import           Data.ByteString                 as BS
import           Data.ByteString.Base32.Internal
import qualified Data.ByteString.Char8           as C8


-- | Base32 encoded bytestring.
type Base32 = ByteString

encTable :: EncTable
encTable = C8.pack "ybndrfg8ejkmcpqxot1uwisza345h769"

-- | Encode an arbitrary bytestring into (upper case) base32 form.
encode :: ByteString -> Base32
encode = unpack5 encTable


-- Taken from <here https://gist.github.com/maaku/8996338#file-common-cpp-L14>.
decTable :: ByteString
decTable = BS.pack [
      invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx,    18
    , invIx,    25,    26,    27,    30,    29,     7,    31, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx,    24,     1,    12,     3,     8
    ,     5,     6,    28,    21,     9,    10, invIx,    11,     2,    16
    ,    13,    14,     4,    22,    17,    19, invIx,    20,    15,     0
    ,    23, invIx, invIx, invIx, invIx, invIx, invIx,    24,     1,    12
    ,     3,     8,     5,     6,    28,    21,     9,    10, invIx,    11
    ,     2,    16,    13,    14,     4,    22,    17,    19, invIx,    20
    ,    15,     0,    23, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx, invIx
    , invIx, invIx, invIx, invIx, invIx, invIx
    ]
{-# INLINE decTable #-}

-- | Decode a base32 encoded bytestring. This functions is
-- case-insensitive and do not require correct padding.
decode :: Base32 -> Either String ByteString
decode = pack5 decTable

-- | The same as 'decode' but with additional leniency: decodeLenient
-- will skip non-alphabet characters.
decodeLenient :: Base32 -> Either String ByteString
decodeLenient = pack5Lenient decTable
