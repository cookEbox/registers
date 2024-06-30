module Main where

import qualified Data.ByteString    as B
import           Data.Word          (Word8)
import           System.Environment (getArgs)

data Binary = I | O deriving (Show, Eq)

data Byte = B  Binary Binary Binary Binary Binary Binary Binary Binary
          | B2 Binary Binary Binary Binary Binary Binary Binary Binary
               Binary Binary Binary Binary Binary Binary Binary Binary
          | B3 Binary Binary Binary Binary Binary Binary Binary Binary
               Binary Binary Binary Binary Binary Binary Binary Binary
               Binary Binary Binary Binary Binary Binary Binary Binary
          | B4 Binary Binary Binary Binary Binary Binary Binary Binary
               Binary Binary Binary Binary Binary Binary Binary Binary
               Binary Binary Binary Binary Binary Binary Binary Binary
               Binary Binary Binary Binary Binary Binary Binary Binary
          deriving (Show, Eq)

instance Semigroup Byte where
  B a b c d e f g h <> B i j k l m n o p
         = B2 a b c d e f g h i j k l m n o p
  B2 a b c d e f g h i j k l m n o p <> B q r s t u v w x 
         = B3 a b c d e f g h i j k l m n o p q r s t u v w x
  B a b c d e f g h <> B2 i j k l m n o p q r s t u v w x 
         = B3 a b c d e f g h i j k l m n o p q r s t u v w x
  B3 a b c d e f g h i j k l m n o p q r s t u v w x <> B q2 r2 s2 t2 u2 v2 w2 x2 
         = B4 a b c d e f g h i j k l m n o p q r s t u v w x q2 r2 s2 t2 u2 v2 w2 x2 
  B a1 b1 c1 d1 e1 f1 g1 h1 <> B3 a b c d e f g h i j k l m n o p q r s t u v w x 
         = B4 a1 b1 c1 d1 e1 f1 g1 h1 a b c d e f g h i j k l m n o p q r s t u v w x
  a <> _ = a

byteStringToWords :: B.ByteString -> [Word8]
byteStringToWords = B.unpack

wordToBinaryList :: [Binary] -> Word8 -> [Binary]
wordToBinaryList acc w  | w > 0 = wordToBinaryList (acc <> [bin]) $ div w 2
                        | w == 0 = reverse acc
                        | otherwise = acc
                        where
                          bin = if rem w 2 == 1 then I else O

boolsToByte :: [Binary] -> Either String Byte
boolsToByte [b1, b2, b3, b4, b5, b6, b7, b8]
              = Right $ B b1 b2 b3 b4 b5 b6 b7 b8
boolsToByte _ = Left "This is not an 8 bit binary number"

-- replace with foldr as in main
bytesToByte :: [Either String Byte] -> Either String Byte
bytesToByte = foldr1 (\x acc -> (<>) <$> x <*> acc)

bytesToByte2 :: [Either String Byte] -> Either String Byte
bytesToByte2 [Right fbyte, Right sbyte] = Right $ fbyte <> sbyte
bytesToByte2 [Left msg, _]              = Left msg
bytesToByte2 [_, Left msg]              = Left msg
bytesToByte2 _                          = Left "Too many elements"

-- replace with something that checks the second element for if it needs the 3rd and 4th
bytesToBytes234 :: [Either String Byte] -> [Either String Byte] 
bytesToBytes234 [_]                                      = [Left "This has an incorrect number of bytes"]
bytesToBytes234 [_, Right (B O O _ _ _ I I O)]           = [Left "For a 00* you need at least one extra byte"]
bytesToBytes234 [_, Right (B I O _ _ _ _ _ _),_]         = [Left "For a 10 you need at least two extra bytes"]
bytesToBytes234 [_, Right (B O I _ _ _ _ _ _)]           = [Left "For a 01 you need at least one extra byte"]
bytesToBytes234 (_: Left msg : _ )                       = [Left msg]
bytesToBytes234 (a:b@(Right (B I I _ _ _ _ _ _)):cs)     = bytesToByte [a, b]       : bytesToBytes234 cs
bytesToBytes234 (a@(Right (B _ _ _ _ I _ _ _)):
                 b@(Right (B O O _ _ _ I I O)):c:ds)     = bytesToByte [a, b, c]    : bytesToBytes234 ds
bytesToBytes234 (a:b@(Right (B O O _ _ _ _ _ _)):cs)     = bytesToByte [a, b]       : bytesToBytes234 cs
bytesToBytes234 (a:b@(Right (B O I _ _ _ _ _ _)):c:ds)   = bytesToByte [a, b, c]    : bytesToBytes234 ds
bytesToBytes234 (a:b@(Right (B I O _ _ _ _ _ _)):c:d:es) = bytesToByte [a, b, c, d] : bytesToBytes234 es
bytesToBytes234 x                                        = x

bytesToBytes2 :: [Either String Byte] -> [Either String Byte] 
bytesToBytes2 bytes@(x:y:zs) | even $ length bytes = bytesToByte2 [x, y] : bytesToBytes2 zs
                             | otherwise           = [Left "This is not a list of 16 bits"]
bytesToBytes2 x              = x

-- replace with opcode that checks first 6 elements of B2 B3 B4
bytes234ToOpCode :: Byte -> Either String (String, Byte)
bytes234ToOpCode bytes@(B2 I O I I _ _ _ _ _ _ _ _ _ _ _ _) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@(B3 I O I I _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@(B2 I O O O I O _ _ _ _ _ _ _ _ _ _) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@(B3 I O O O I O _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@(B4 I O O O I O _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) 
                   = Right ("mov", bytes)
bytes234ToOpCode _ = Left "Unkown operator"

bytes2ToOperation :: Byte -> Either String (String, Byte)
bytes2ToOperation bytes@(B2 I O O O I O _ _ _ _ _ _ _ _ _ _)
                    = Right ("mov", bytes)
bytes2ToOperation _ = Left "Unkown operator"

-- generalise to any number of bytes
-- need a helper function to turn Byte to word8
-- bytesToRegMem :: (String, Byte) -> Either String (String, Byte)
-- bytesToRegMem (str, bytes@(B3 )) 

bytes2ToRegisters :: (String, Byte) -> Either String (String, Byte)
bytes2ToRegisters (str, bytes@(B2 _ _ _ _ _ _ d w I I a1 a2 a3 b1 b2 b3))
  | db && wb  = Right (str <> " " <> wideReg a1 a2 a3 <> ", " <> wideReg b1 b2 b3, bytes)
  | db        = Right (str <> " " <> thinReg a1 a2 a3 <> ", " <> thinReg b1 b2 b3, bytes)
  |       wb  = Right (str <> " " <> wideReg b1 b2 b3 <> ", " <> wideReg a1 a2 a3, bytes)
  | otherwise = Right (str <> " " <> thinReg b1 b2 b3 <> ", " <> thinReg a1 a2 a3, bytes)
    where
      db = d == I
      wb = w == I
      wideReg O O O = "ax"
      wideReg O O I = "cx"
      wideReg O I O = "dx"
      wideReg O I I = "bx"
      wideReg I O O = "sp"
      wideReg I O I = "bp"
      wideReg I I O = "si"
      wideReg I I I = "di"
      thinReg O O O = "al"
      thinReg O O I = "cl"
      thinReg O I O = "dl"
      thinReg O I I = "bl"
      thinReg I O O = "ah"
      thinReg I O I = "ch"
      thinReg I I O = "dh"
      thinReg I I I = "bh"
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
        <$> bytesToBytes234
         (  boolsToByte
         .  wordToBinaryList  []
        <$> byteStringToWords contents
         )
  case op of
    Right msg -> putStrLn $ "bits 16 \n\n" <> msg
    Left  err -> print err
