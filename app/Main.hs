module Main where

import qualified Data.ByteString as B
import Data.Word (Word8)

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
boolsToByte [b1, b2, b3, b4, b5, b6, b7, b8] = Right $ B b1 b2 b3 b4 b5 b6 b7 b8
boolsToByte _ = Left "This is not an 8 bit binary number"

bytesToByte2 :: [Either String Byte] -> Either String Byte 
bytesToByte2 [Right fbyte, Right sbyte] = Right $ fbyte <> sbyte 
bytesToByte2 [Left msg, _] = Left msg 
bytesToByte2 [_, Left msg] = Left msg
bytesToByte2 _ = Left "Too many elements"

bytes2ToOperation :: Byte -> Either String (String, Byte)
bytes2ToOperation bytes@(B2 True False False False True False _ _ _ _ _ _ _ _ _ _) = Right ("mov", bytes)
bytes2ToOperation _ = Left "Unkown operator"

bytes2ToRegisters :: (String, Byte) -> Either String (String, Byte)
bytes2ToRegisters (str, bytes@(B2 _ _ _ _ _ _ d w True True a1 a2 a3 b1 b2 b3)) 
  | d && w = Right (str <> " " <> wideReg a1 a2 a3 <> ", " <> wideReg b1 b2 b3, bytes)
  | d      = Right (str <> " " <> thinReg a1 a2 a3 <> ", " <> thinReg b1 b2 b3, bytes)
  |      w = Right (str <> " " <> wideReg b1 b2 b3 <> ", " <> wideReg a1 a2 a3, bytes)
  | otherwise = Right (str <> " " <> thinReg b1 b2 b3 <> ", " <> thinReg a1 a2 a3, bytes)
bytes2ToRegisters _ = Left "Not 2 bytes"

wideReg :: Bool -> Bool -> Bool -> String
wideReg False False False = "ax"
wideReg False False True = "cx"
wideReg False True False = "dx"
wideReg False True True = "bx"
wideReg True False False = "sp"
wideReg True False True = "bp"
wideReg True True False = "si"
wideReg True True True = "di"

thinReg :: Bool -> Bool -> Bool -> String
thinReg False False False = "al"
thinReg False False True = "cl"
thinReg False True False = "dl"
thinReg False True True = "bl"
thinReg True False False = "lh"
thinReg True False True = "bh"
thinReg True True False = "sh"
thinReg True True True = "dh"

printASM :: Either String (String, Byte) -> IO ()
printASM (Left err) = print err
printASM (Right (asm, _)) = putStrLn $ "bits 16\n\n" <> asm

main :: IO ()
main = do 
  contents <- B.readFile "../listing37" 
  let op =   bytesToByte2 
         $   boolsToByte 
         .   wordToBinaryList [] 
         <$> byteStringToWords contents
  case op of 
    Right bytes -> printASM $ bytes2ToRegisters =<< bytes2ToOperation bytes
    Left err -> print err
