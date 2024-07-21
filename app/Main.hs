module Main where

import qualified Data.ByteString    as B
import           Data.Word          (Word8, Word16)
import           System.Environment (getArgs)

data Bin = I | O deriving (Show, Eq)

data Byte = B  Bin Bin Bin Bin Bin Bin Bin Bin
          | B2 Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
          | B3 Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
          | B4 Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
          | B5 Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
          | B6 Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
               Bin Bin Bin Bin Bin Bin Bin Bin
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
  B a2 b2 c2 d2 e2 f2 g2 h2 
    <> B4 a1 b1 c1 d1 e1 f1 g1 h1 a b c d e f g h i j k l m n o p q r s t u v w x
    = B5 a2 b2 c2 d2 e2 f2 g2 h2 a1 b1 c1 d1 e1 f1 g1 h1 
         a b c d e f g h i j k l m n o p q r s t u v w x
  B2 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 
    <> B3 a b c d e f g h i j k l m n o p q r s t u v w x
    = B5 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 
         a b c d e f g h i j k l m n o p q r s t u v w x
  B3 a b c d e f g h i j k l m n o p q r s t u v w x 
    <> B2 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 
    = B5 a b c d e f g h i j k l m n o p q r s t u v w x 
         a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1
  B a3 b3 c3 d3 e3 f3 g3 h3 
    <> B5 a2 b2 c2 d2 e2 f2 g2 h2 a1 b1 c1 d1 e1 f1 g1 h1 
          a b c d e f g h i j k l m n o p q r s t u v w x
    = B6 a3 b3 c3 d3 e3 f3 g3 h3 a2 b2 c2 d2 e2 f2 g2 h2 a1 b1 c1 d1 e1 f1 g1 h1 
         a b c d e f g h i j k l m n o p q r s t u v w x
  B2 a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 k3 l3 m3 n3 o3 p3 
    <> B4 a1 b1 c1 d1 e1 f1 g1 h1 a b c d e f g h i j k l m n o p q r s t u v w x
    = B6 a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 k3 l3 m3 n3 o3 p3 a1 b1 c1 d1 e1 f1 g1 h1 
         a b c d e f g h i j k l m n o p q r s t u v w x
  B4 a1 b1 c1 d1 e1 f1 g1 h1 a b c d e f g h i j k l m n o p q r s t u v w x 
    <> B2 a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 k3 l3 m3 n3 o3 p3
    = B6 a1 b1 c1 d1 e1 f1 g1 h1 a b c d e f g h i j k l m n o p q r s t u v w x 
         a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 k3 l3 m3 n3 o3 p3 
  B3 a b c d e f g h i j k l m n o p q r s t u v w x 
    <> B3 a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 
    = B6 a b c d e f g h i j k l m n o p q r s t u v w x 
         a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 
  a <> _ = a

byteStringToWords :: B.ByteString -> [Word8]
byteStringToWords = B.unpack

wordToBinList :: [Bin] -> Word8 -> [Bin]
wordToBinList acc w  | w > 0 = wordToBinList (acc <> [bin]) $ div w 2
                        | w == 0 = reverse acc
                        | otherwise = acc
                        where
                          bin = if rem w 2 == 1 then I else O

boolsToByte :: [Bin] -> Either String Byte
boolsToByte byte 
  | length byte < 8 = eightBinToByte $ replicate (8 - length byte) O <> byte
  | otherwise       = eightBinToByte byte

eightBinToByte :: [Bin] -> Either String Byte
eightBinToByte [b1, b2, b3, b4, b5, b6, b7, b8]
                 = Right $ B b1 b2 b3 b4 b5 b6 b7 b8
eightBinToByte _ = Left "This is not an 8 bit binary number"

bytesToByte :: [Either String Byte] -> Either String Byte
bytesToByte = foldr1 (\x acc -> (<>) <$> x <*> acc)

bytesToBytes234 :: [Either String Byte] -> [Either String Byte] 
bytesToBytes234 [_]                                          
                  = [Left "This has an incorrect number of bytes"]
bytesToBytes234 [_, Right (B O I _ _ _ _ _ _)]               
                  = [Left "For a 01 you need at least one extra byte"]
bytesToBytes234 (Left msg : _ )                              
                  = [Left msg]
bytesToBytes234 (_: Left msg : _ )                           
                  = [Left msg]
bytesToBytes234 (a@(Right (B I O I I O _ _ _)):b:cs)         
                  = bytesToByte [a, b]       : bytesToBytes234 cs
bytesToBytes234 (a@(Right (B I O I I I _ _ _)):b:c:ds)       
                  = bytesToByte [a, c, b]    : bytesToBytes234 ds
bytesToBytes234 (a@(Right (B I O I O O O _ O)):b:cs)         
                  = bytesToByte [a, b]       : bytesToBytes234 cs
bytesToBytes234 (a@(Right (B I O I O O O _ I)):b:c:ds)       
                  = bytesToByte [a, c, b]    : bytesToBytes234 ds
bytesToBytes234 (a@(Right (B I I O O O I I O)):
                 b@(Right (B O I O O O _ _ _)):c:d:es)       
                  = bytesToByte [a, b, c, d] : bytesToBytes234 es
bytesToBytes234 (a@(Right (B I I O O O I I I)):
                 b@(Right (B O I O O O _ _ _)):c:d:e:fs)     
                  = bytesToByte [a, b, c, e, d] : bytesToBytes234 fs
bytesToBytes234 (a@(Right (B I I O O O I I I)):
                 b@(Right (B O I O O O I I O)):c:d:e:f:gs)   
                  = bytesToByte [a, b, c, d, f, e] : bytesToBytes234 gs
bytesToBytes234 (a@(Right (B I I O O O I I I)):
                 b@(Right (B I O O O O _ _ _)):c:d:e:f:gs)   
                  = bytesToByte [a, b, c, d, f, e] : bytesToBytes234 gs
bytesToBytes234 (a@(Right (B I I O O O I I I)):
                 b@(Right (B I O O O O I I O)):c:d:e:f:g:hs) 
                  = bytesToByte [a, b, c, d, e, g, f] : bytesToBytes234 hs
bytesToBytes234 (a@(Right (B I I O O O I I O)):
                 b@(Right (B O O O O O I I O)):c:d:e:fs)     
                  = bytesToByte [a, b, c, d, e] : bytesToBytes234 fs
bytesToBytes234 (a@(Right (B I I O O O I I I)):
                 b@(Right (B O O O O O I I O)):c:d:e:f:gs)   
                  = bytesToByte [a, b, c, d, f, e] : bytesToBytes234 gs
bytesToBytes234 (a@(Right (B I I O O O I I O)):
                 b@(Right (B _ _ O O O _ _ _)):c:ds)         
                  = bytesToByte [a, b, c] : bytesToBytes234 ds
bytesToBytes234 (a@(Right (B I I O O O I I I)):
                 b@(Right (B _ _ O O O _ _ _)):c:d:es)       
                  = bytesToByte [a, b, d, c] : bytesToBytes234 es
bytesToBytes234 (a:b@(Right (B I I _ _ _ _ _ _)):cs)         
                  = bytesToByte [a, b]       : bytesToBytes234 cs
bytesToBytes234 (a@(Right (B _ _ _ _ _ _ _ I)):
                 b@(Right (B O O _ _ _ I I O)):c:d:es)       
                  = bytesToByte [a, b, d, c] : bytesToBytes234 es
bytesToBytes234 (a:b@(Right (B O O _ _ _ _ _ _)):cs)         
                  = bytesToByte [a, b]       : bytesToBytes234 cs
bytesToBytes234 (a:b@(Right (B O I _ _ _ _ _ _)):c:ds)       
                  = bytesToByte [a, b, c]    : bytesToBytes234 ds
bytesToBytes234 (a:b@(Right (B I O _ _ _ _ _ _)):c:d:es)     
                  = bytesToByte [a, b, d, c] : bytesToBytes234 es
bytesToBytes234 x = x

bytes234ToOpCode :: Byte -> Either String (String, Byte)
bytes234ToOpCode bytes@( B2 I O I _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B3 I O I _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B2 I O O O I O _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B3 I O O O I O _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B4 I O O O I O _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B5 I O O O I O _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B6 I O O O I O _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B2 I I O O O I _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B3 I I O O O I _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B4 I I O O O I _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B5 I I O O O I _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode bytes@( B6 I I O O O I _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ 
                            _ _ _ _ _ _ _ _ ) 
                   = Right ("mov", bytes)
bytes234ToOpCode _ = Left "Unkown operator"

toWord8 :: Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin 
        -> Word8
toWord8 a b c d e f g h 
  = sum $ zipWith (\x y -> if z x then y else 0) 
    [a,b,c,d,e,f,g,h] (reverse $ ((2^)::Word8 -> Word8) <$> [0..7]) 

signedToWord8 :: Bin 
              -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin 
              -> String
signedToWord8 s a b c d e f g h 
  | worded == 0  = ""
  | otherwise    = signed 
    where worded = toWord8 a b c d e f g h
          signed = if z s then " + " <> show worded 
                          else " - " <> show (abs $ 2^(8 :: Word8) - worded)

toWord16 :: Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin 
         -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin 
         -> Word16
toWord16 a b c d e f g h i j k l m n o p 
  = sum $ zipWith (\x y -> if z x then y else 0) 
    [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] 
    (reverse $ ((2^)::Word16 -> Word16) <$> [0..15])


signedToWord16 :: Bin 
               -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin 
               -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin -> Bin 
               -> String
signedToWord16 s a b c d e f g h i j k l m n o p 
  | worded == 0  = ""
  | otherwise    = signed 
    where worded = toWord16 a b c d e f g h i j k l m n o p 
          signed = if z s then " + " <> show worded 
                          else " - " <> show (abs $ 2^(16 :: Word16) - worded)

bytesToRegMem :: (String, Byte) -> Either String (String, Byte)
bytesToRegMem (str, bytes@(B2 I O I I O a1 a2 a3 b1 b2 b3 b4 b5 b6 b7 b8))
               = Right ( str <> " " <> thinReg a1 a2 a3
                             <> ", " <> show (toWord8 b1 b2 b3 b4 b5 b6 b7 b8)
                             , bytes
                       )
bytesToRegMem (str, bytes@(B3 I O I I I a1 a2 a3 b1 b2 b3 b4 b5 b6 b7 b8 
                                                 c1 c2 c3 c4 c5 c6 c7 c8))
               = Right ( str <> " " <> wideReg a1 a2 a3
                             <> ", " <> show (toWord16 b1 b2 b3 b4 b5 b6 b7 b8 
                                                       c1 c2 c3 c4 c5 c6 c7 c8)
                             , bytes
                       )
bytesToRegMem (str, bytes@(B2 I O I O O O d I b1 b2 b3 b4 b5 b6 b7 b8))
  | z d        = Right ( str <> " ax, ["
                      <> show (toWord8 b1 b2 b3 b4 b5 b6 b7 b8)
                      <> "]"
                      , bytes
                )
  | otherwise  = Right ( str <> " ["
                            <> show (toWord8 b1 b2 b3 b4 b5 b6 b7 b8)
                            <> "], ax" 
                            , bytes
                      )
bytesToRegMem (str, bytes@(B3 I O I O O O d I b1 b2 b3 b4 b5 b6 b7 b8 
                                              c1 c2 c3 c4 c5 c6 c7 c8))
  | not (z d)  = Right ( str <> " ax, ["
                      <> show (toWord16 b1 b2 b3 b4 b5 b6 b7 b8 
                                        c1 c2 c3 c4 c5 c6 c7 c8)
                      <> "]"
                      , bytes
                )
  | otherwise  = Right ( str <> " ["
                            <> show (toWord16 b1 b2 b3 b4 b5 b6 b7 b8 
                                              c1 c2 c3 c4 c5 c6 c7 c8)
                            <> "], ax"
                            , bytes
                      )
bytesToRegMem (str, bytes@(B3 I I O O O I I _ O O _ _ _ b1 b2 b3 
                                                  c1 c2 c3 c4 c5 c6 c7 c8))
               = Right ( str <> " [" <> ooMem   b1 b2 b3 <> "]" 
                             <> ", byte " 
                             <> show (toWord8 c1 c2 c3 c4 c5 c6 c7 c8)
                             , bytes
                       )
bytesToRegMem (str, bytes@(B3 I I O O O I I _ I I _ _ _ b1 b2 b3 
                                                  c1 c2 c3 c4 c5 c6 c7 c8))
               = Right ( str <> " [" <> ooMem   b1 b2 b3 <> "]" 
                             <> ", byte " 
                             <> show (toWord8 c1 c2 c3 c4 c5 c6 c7 c8)
                             , bytes
                       )
bytesToRegMem (str, bytes@(B4 I I O O O I I _ O I _ _ _ I I O
                                                  a1 a2 a3 a4 a5 a6 a7 a8
                                                  b1 b2 b3 b4 b5 b6 b7 b8
                                                  ))
               = Right ( str <> " [" <> ooMem I I O <> " + " 
                             <> show (toWord8 a1 a2 a3 a4 a5 a6 a7 a8)
                             <> "]" <> ", word " 
                             <> show (toWord8 b1 b2 b3 b4 b5 b6 b7 b8 )
                             , bytes
                       )
bytesToRegMem (str, bytes@(B5 I I O O O I I _ O I _ _ _ I I O
                                                  a1 a2 a3 a4 a5 a6 a7 a8
                                                  b1 b2 b3 b4 b5 b6 b7 b8
                                                  c1 c2 c3 c4 c5 c6 c7 c8
                                                  ))
               = Right ( str <> " [" <> ooMem I I O <> " + " 
                             <> show (toWord8 a1 a2 a3 a4 a5 a6 a7 a8)
                             <> "]" <> ", word " 
                             <> show (toWord16 b1 b2 b3 b4 b5 b6 b7 b8 c1 c2 c3 c4 c5 c6 c7 c8)
                             , bytes
                       )
bytesToRegMem (str, bytes@(B5 I I O O O I I _ O O _ _ _ I I O
                                                  a1 a2 a3 a4 a5 a6 a7 a8
                                                  b1 b2 b3 b4 b5 b6 b7 b8
                                                  c1 c2 c3 c4 c5 c6 c7 c8
                                                  ))
               = Right ( str <> " [" <> ooMem I I O <> " + " 
                             <> show (toWord16 a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8)
                             <> "]" <> ", word " 
                             <> show (toWord8 c1 c2 c3 c4 c5 c6 c7 c8)
                             , bytes
                       )
bytesToRegMem (str, bytes@(B6 I I O O O I I _ O O _ _ _ I I O
                                                  a1 a2 a3 a4 a5 a6 a7 a8
                                                  b1 b2 b3 b4 b5 b6 b7 b8
                                                  c1 c2 c3 c4 c5 c6 c7 c8
                                                  d1 d2 d3 d4 d5 d6 d7 d8
                                                  ))
               = Right ( str <> " [" <> ooMem I I O <> " + " 
                             <> show (toWord16 a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8)
                             <> "]" <> ", word " 
                             <> show (toWord16 c1 c2 c3 c4 c5 c6 c7 c8 d1 d2 d3 d4 d5 d6 d7 d8)
                             , bytes
                       )
bytesToRegMem (str, bytes@(B5 I I O O O I I _ I O _ _ _ r1 r2 r3
                                                  a1 a2 a3 a4 a5 a6 a7 a8
                                                  b1 b2 b3 b4 b5 b6 b7 b8
                                                  c1 c2 c3 c4 c5 c6 c7 c8
                                                  ))
               = Right ( str <> " [" <> ooMem r1 r2 r3 <> " + " 
                             <> show (toWord16 a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8)
                             <> "]" <> ", word " 
                             <> show (toWord8 c1 c2 c3 c4 c5 c6 c7 c8)
                             , bytes
                       )
bytesToRegMem (str, bytes@(B6 I I O O O I I _ I O _ _ _ r1 r2 r3
                                                  a1 a2 a3 a4 a5 a6 a7 a8
                                                  b1 b2 b3 b4 b5 b6 b7 b8
                                                  c1 c2 c3 c4 c5 c6 c7 c8
                                                  d1 d2 d3 d4 d5 d6 d7 d8
                                                  ))
               = Right ( str <> " [" <> ooMem r1 r2 r3 <> " + " 
                             <> show (toWord16 a1 a2 a3 a4 a5 a6 a7 a8 b1 b2 b3 b4 b5 b6 b7 b8)
                             <> "]" <> ", word " 
                             <> show (toWord16 c1 c2 c3 c4 c5 c6 c7 c8 d1 d2 d3 d4 d5 d6 d7 d8)
                             , bytes
                       )
bytesToRegMem (str, bytes@(B4 _ _ _ _ _ _ I _ O O a1 a2 a3 I  O  O
                                                  c1 c2 c3 c4 c5 c6 c7 c8 
                                                  d1 d2 d3 d4 d5 d6 d7 d8
                          ))
               = Right ( str <> " " <> wideReg a1 a2 a3 <> ", "
                             <> "[" 
                             <> show (toWord16 c1 c2 c3 c4 c5 c6 c7 c8 
                                               d1 d2 d3 d4 d5 d6 d7 d8)
                             <> "]"
                             , bytes
                       ) 
bytesToRegMem (str, bytes@(B4 _ _ _ _ _ _ O _ O O I  O  O  b1 b2 b3
                                                  c1 c2 c3 c4 c5 c6 c7 c8 
                                                  d1 d2 d3 d4 d5 d6 d7 d8
                          ))
               = Right ( str <> " " <> "["
                             <> show (toWord16 c1 c2 c3 c4 c5 c6 c7 c8 
                                               d1 d2 d3 d4 d5 d6 d7 d8)
                             <> "]"
                             <> ", " <> wideReg b1 b2 b3 
                             , bytes
                       ) 
bytesToRegMem (str, bytes@(B4 _ _ _ _ _ _ d I O O a1 a2 a3 I I O
                                                  c1 c2 c3 c4 c5 c6 c7 c8 
                                                  d1 d2 d3 d4 d5 d6 d7 d8
                          ))
  | z d        = Right ( str <> " " <> wideReg a1 a2 a3 <> ", "
                             <> "[" 
                             <> show (toWord16 c1 c2 c3 c4 c5 c6 c7 c8 
                                               d1 d2 d3 d4 d5 d6 d7 d8)
                             <> "]"
                             , bytes
                       )
  | otherwise  = Right ( str <> " " <> "["
                             <> show (toWord16 c1 c2 c3 c4 c5 c6 c7 c8 
                                               d1 d2 d3 d4 d5 d6 d7 d8)
                             <> "]" <> ", " <> wideReg a1 a2 a3
                             , bytes
                       )
bytesToRegMem (str, bytes@(B4 _ _ _ _ _ s d w I O a1 a2 a3 b1 b2 b3 
                                                  c1 c2 c3 c4 c5 c6 c7 c8 
                                                  d1 d2 d3 d4 d5 d6 d7 d8
                          ))
  | z d && z w = Right ( str <> " " <> wideReg a1 a2 a3 <> ", "
                             <> "[" <> ooMem   b1 b2 b3 
                             <> signedToWord16 s c1 c2 c3 c4 c5 c6 c7 c8 
                                                 d1 d2 d3 d4 d5 d6 d7 d8
                             <> "]"
                             , bytes
                       ) 
  | z d        = Right ( str <> " " <> thinReg a1 a2 a3 <> ", "
                             <> "[" <> ooMem   b1 b2 b3 
                             <> signedToWord16 s c1 c2 c3 c4 c5 c6 c7 c8 
                                                 d1 d2 d3 d4 d5 d6 d7 d8
                             <> "]"
                             , bytes
                       )
  |        z w = Right ( str <> " " <> "[" <> ooMem   b1 b2 b3
                             <> signedToWord16 s c1 c2 c3 c4 c5 c6 c7 c8 
                                                 d1 d2 d3 d4 d5 d6 d7 d8
                             <> "]" <> ", " 
                             <> wideReg a1 a2 a3
                             , bytes
                       )
  | otherwise  = Right ( str <> " " <> "[" <> ooMem   b1 b2 b3
                             <> signedToWord16 s c1 c2 c3 c4 c5 c6 c7 c8 
                                                 d1 d2 d3 d4 d5 d6 d7 d8
                             <> "]" <> ", " <> thinReg a1 a2 a3
                             , bytes
                       )
bytesToRegMem (str, bytes@(B3 _ _ _ _ _ s d w O I a1 a2 a3 b1 b2 b3 
                                                  c1 c2 c3 c4 c5 c6 c7 c8))
  | z d && z w = Right ( str <> " " <> wideReg a1 a2 a3 <> ", "
                             <> "[" <> ooMem   b1 b2 b3 
                             <> signedToWord8 s c1 c2 c3 c4 c5 c6 c7 c8 
                             <> "]"
                             , bytes
                       ) 
  | z d        = Right ( str <> " " <> thinReg a1 a2 a3 <> ", "
                             <> "[" <> ooMem   b1 b2 b3 
                             <> signedToWord8 s c1 c2 c3 c4 c5 c6 c7 c8 
                             <> "]"
                             , bytes
                       )
  |        z w = Right ( str <> " " <> "[" <> ooMem   b1 b2 b3
                             <> signedToWord8 s c1 c2 c3 c4 c5 c6 c7 c8 
                             <> "]" <> ", " 
                             <> wideReg a1 a2 a3
                             , bytes
                       )
  | otherwise  = Right ( str <> " " <> "[" <> ooMem   b1 b2 b3
                             <> signedToWord8 s c1 c2 c3 c4 c5 c6 c7 c8 
                             <> "]" <> ", " <> thinReg a1 a2 a3
                             , bytes
                       )
bytesToRegMem (str, bytes@(B2 _ _ _ _ _ _ d w O O a1 a2 a3 b1 b2 b3))
  | z d && z w = Right (str <> " "    <> wideReg a1 a2 a3
                            <> ", ["  <> ooMem   b1 b2 b3  <> "]", bytes)
  | z d        = Right (str <> " "    <> thinReg a1 a2 a3
                            <> ", ["  <> ooMem   b1 b2 b3  <> "]", bytes)
  |        z w = Right (str <> " ["   <> ooMem   b1 b2 b3
                            <> "], "  <> wideReg a1 a2 a3, bytes)
  | otherwise  = Right (str <> " ["   <> ooMem   b1 b2 b3
                            <> "], "  <> thinReg a1 a2 a3, bytes)
bytesToRegMem (str, bytes@(B2 _ _ _ _ _ _ d w I I a1 a2 a3 b1 b2 b3))
  | z d && z w = Right (str <> " "  <> wideReg a1 a2 a3
                            <> ", " <> wideReg b1 b2 b3, bytes)
  | z d        = Right (str <> " "  <> thinReg a1 a2 a3
                            <> ", " <> thinReg b1 b2 b3, bytes)
  |        z w = Right (str <> " "  <> wideReg b1 b2 b3
                            <> ", " <> wideReg a1 a2 a3, bytes)
  | otherwise  = Right (str <> " "  <> thinReg b1 b2 b3
                            <> ", " <> thinReg a1 a2 a3, bytes)
bytesToRegMem (_, bytes) 
               = Left $ "This is not a recognized pattern of bytes\n" 
                      <> show bytes

z :: Bin -> Bool
z d = d == I

ooMem, wideReg, thinReg :: Bin -> Bin -> Bin -> String
ooMem   O O O = "bx + si"
ooMem   O O I = "bx + di"
ooMem   O I O = "bp + si"
ooMem   O I I = "bp + di"
ooMem   I O O = "si"
ooMem   I O I = "di"
ooMem   I I O = "bp"
ooMem   I I I = "bx"
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

stringASM :: Either String (String, Byte) -> Either String String
stringASM (Right (asm, _)) = Right $ asm <> "\n"
stringASM (Left msg)       = Left msg

registers :: Either String Byte -> Either String String
registers (Right byte) = stringASM $ bytesToRegMem =<< bytes234ToOpCode byte
registers (Left msg)   = Left msg

main :: IO ()
main = do
  file     <- fmap head getArgs
  contents <- B.readFile file
  putStrLn $ concat $ (<>"\n") <$> concatMap show <$> bytesToBytes234 (boolsToByte . wordToBinList [] <$> byteStringToWords contents)
  let op =  foldr (\x acc -> (<>) <$> x <*> acc) (Right "") 
         $  registers
        <$> bytesToBytes234
         (  boolsToByte
         .  wordToBinList  []
        <$> byteStringToWords contents
         )
  case op of
    Right msg -> putStrLn $ "bits 16 \n\n" <> msg
    Left  err -> putStrLn err
