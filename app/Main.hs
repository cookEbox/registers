module Main where

import qualified Data.ByteString    as B
import           Data.Word          (Word8)
import           System.Environment (getArgs)

data Byte = B  Bool Bool Bool Bool Bool Bool Bool Bool
          | B2 Bool Bool Bool Bool Bool Bool Bool Bool
               Bool Bool Bool Bool Bool Bool Bool Bool
          deriving (Show, Eq)

instance Semigroup Byte where
  B a b c d e f g h <> B i j k l m n o p
         = B2 a b c d e f g h i j k l m n o p
  a <> _ = a

byteStringToWords :: B.ByteString -> [Word8]
byteStringToWords = B.unpack

wordToBinaryList :: [Bool] -> Word8 -> [Bool]
wordToBinaryList acc w  | w > 0 = wordToBinaryList (acc <> [bin]) $ div w 2
                        | w == 0 = reverse acc
                        | otherwise = acc
                        where
                          bin = rem w 2 == 1

boolsToByte :: [Bool] -> Either String Byte
boolsToByte [b1, b2, b3, b4, b5, b6, b7, b8]
              = Right $ B b1 b2 b3 b4 b5 b6 b7 b8
boolsToByte _ = Left "This is not an 8 bit binary number"

bytesToByte2 :: [Either String Byte] -> Either String Byte
bytesToByte2 [Right fbyte, Right sbyte] = Right $ fbyte <> sbyte
bytesToByte2 [Left msg, _]              = Left msg
bytesToByte2 [_, Left msg]              = Left msg
bytesToByte2 _                          = Left "Too many elements"

bytesToBytes2 :: [Either String Byte] -> [Either String Byte] 
bytesToBytes2 bytes@(x:y:zs) | even $ length bytes = bytesToByte2 [x, y] : bytesToBytes2 zs
                             | otherwise           = [Left "This is not a list of 16 bits"]
bytesToBytes2 x              = x

bytes2ToOperation :: Byte -> Either String (String, Byte)
bytes2ToOperation bytes@(B2 True False False False True False _ _ _ _ _ _ _ _ _ _)
                    = Right ("mov", bytes)
bytes2ToOperation _ = Left "Unkown operator"

bytes2ToRegisters :: (String, Byte) -> Either String (String, Byte)
bytes2ToRegisters (str, bytes@(B2 _ _ _ _ _ _ d w True True a1 a2 a3 b1 b2 b3))
  | d && w    = Right (str <> " " <> wideReg a1 a2 a3 <> ", " <> wideReg b1 b2 b3, bytes)
  | d         = Right (str <> " " <> thinReg a1 a2 a3 <> ", " <> thinReg b1 b2 b3, bytes)
  |      w    = Right (str <> " " <> wideReg b1 b2 b3 <> ", " <> wideReg a1 a2 a3, bytes)
  | otherwise = Right (str <> " " <> thinReg b1 b2 b3 <> ", " <> thinReg a1 a2 a3, bytes)
    where
      wideReg False False False = "ax"
      wideReg False False True  = "cx"
      wideReg False True  False = "dx"
      wideReg False True  True  = "bx"
      wideReg True  False False = "sp"
      wideReg True  False True  = "bp"
      wideReg True  True  False = "si"
      wideReg True  True  True  = "di"
      thinReg False False False = "al"
      thinReg False False True  = "cl"
      thinReg False True  False = "dl"
      thinReg False True  True  = "bl"
      thinReg True  False False = "ah"
      thinReg True  False True  = "ch"
      thinReg True  True  False = "dh"
      thinReg True  True  True  = "bh"
bytes2ToRegisters _ = Left "Not 2 bytes"

stringASM :: Either String (String, Byte) -> Either String String
stringASM (Right (asm, _)) = Right $ asm <> "\n"
stringASM (Left msg)       = Left msg

registers :: Either String Byte -> Either String String
registers (Right byte) = stringASM $ bytes2ToRegisters =<< bytes2ToOperation byte
registers (Left msg)   = Left msg

main :: IO ()
main = do
  file     <- fmap head getArgs
  contents <- B.readFile file
  let op =  foldr (\x acc -> (<>) <$> x <*> acc) (Right "") 
         $  registers
        <$> bytesToBytes2
         (  boolsToByte
         .  wordToBinaryList  []
        <$> byteStringToWords contents
         )
  case op of
    Right msg -> putStrLn $ "bits 16 \n\n" <> msg
    Left  err -> print err
