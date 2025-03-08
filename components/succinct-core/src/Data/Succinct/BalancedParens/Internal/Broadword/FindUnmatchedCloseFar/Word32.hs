module Data.Succinct.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word32
  ( findUnmatchedCloseFar
  ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Broadword.Word32
import HaskellWorks.Data.Int.Narrow
import HaskellWorks.Data.Int.Widen

muk1 :: Word32
muk1 = 0x33333333
{-# INLINE muk1 #-}

muk2 :: Word32
muk2 = 0x0f0f0f0f
{-# INLINE muk2 #-}

muk3 :: Word32
muk3 = 0x00ff00ff
{-# INLINE muk3 #-}

muk4 :: Word32
muk4 = 0x0000ffff
{-# INLINE muk4 #-}

-- | Find the position of the first unmatch parenthesis.
--
-- This is the broadword implementation of 'Data.Succinct.BalancedParens.Internal.Slow.Word32.findCloseFor'.
--
-- See [Broadword Implementation of Parenthesis Queries](https://arxiv.org/pdf/1301.5468.pdf), Sebastiano Vigna, 2013
findUnmatchedCloseFar :: Word64 -> Word64 -> Word32 -> Word64
findUnmatchedCloseFar c p w =
  --  Keys:
  --    * k1: Level of sub-words of size 2 = 1 .<. 1
  --    * k2: Level of sub-words of size 4 = 1 .<. 2
  --    * k3: Level of sub-words of size 8 = 1 .<. 3
  --    * o: Open count
  --    * c: Close count
  --    * e: Excess
  --    * L: Left half of sub-word
  --    * R: Right half of sub-word
  --    * b: deficit at position whole-word mask
  --    * m: deficit at position sub-word mask
  --    * pa: position accumulator
  --    * sa: shift accumulator
  --    * f: far sub-block close parens count
  let x     = w .>. p                                                                           in
  let wsz   = 32 :: Int32                                                                       in
  let k1    = 1                                                                                 in
  let k2    = 2                                                                                 in
  let k3    = 3                                                                                 in
  let k4    = 4                                                                                 in
  let k5    = 5                                                                                 in
  let mask1 = (1 .<. (1 .<. k1)) - 1                                                            in
  let mask2 = (1 .<. (1 .<. k2)) - 1                                                            in
  let mask3 = (1 .<. (1 .<. k3)) - 1                                                            in
  let mask4 = (1 .<. (1 .<. k4)) - 1                                                            in
  let mask5 = (1 .<. (1 .<. k5)) - 1                                                            in
  let t64k1 = 1 .<. k1 :: Word64                                                                in
  let t64k2 = 1 .<. k2 :: Word64                                                                in
  let t64k3 = 1 .<. k3 :: Word64                                                                in
  let t64k4 = 1 .<. k4 :: Word64                                                                in
  let t8k1  = 1 .<. k1 :: Word32                                                                in
  let t8k2  = 1 .<. k2 :: Word32                                                                in
  let t8k3  = 1 .<. k3 :: Word32                                                                in
  let t8k4  = 1 .<. k4 :: Word32                                                                in
  let t8k5  = 1 .<. k5 :: Word32                                                                in

  let b0    =      x .&. 0x55555555                                                             in
  let b1    =    ( x .&. 0xaaaaaaaa) .>. 1                                                      in
  let ll    =   (b0  .^. b1  ) .&. b1                                                           in
  let ok1   = ( (b0  .&. b1  )                 .<. 1) .|. ll                                    in
  let ck1   = (((b0  .|. b1  ) .^. 0x55555555) .<. 1) .|. ll                                    in

  let eok1 =   ok1 .&.  muk1                                                                    in
  let eck1 =  (ck1 .&. (muk1 .<. t64k1)) .>. t64k1                                              in
  let ok2L =  (ok1 .&. (muk1 .<. t64k1)) .>. t64k1                                              in
  let ok2R = kBitDiffPos 4 eok1 eck1                                                            in
  let ok2  = ok2L + ok2R                                                                        in
  let ck2  =  (ck1 .&.  muk1) + kBitDiffPos 4 eck1 eok1                                         in

  let eok2 =   ok2 .&.  muk2                                                                    in
  let eck2 =  (ck2 .&. (muk2 .<. t64k2)) .>. t64k2                                              in
  let ok3L = ((ok2 .&. (muk2 .<. t64k2)) .>. t64k2)                                             in
  let ok3R = kBitDiffPos 8 eok2 eck2                                                            in
  let ok3  = ok3L + ok3R                                                                        in
  let ck3  =  (ck2 .&.  muk2) + kBitDiffPos 8 eck2 eok2                                         in

  let eok3 =   ok3 .&.  muk3                                                                    in
  let eck3 =  (ck3 .&. (muk3 .<. t64k3)) .>. t64k3                                              in
  let ok4L = ((ok3 .&. (muk3 .<. t64k3)) .>. t64k3)                                             in
  let ok4R = kBitDiffPos 16 eok3 eck3                                                           in
  let ok4  = ok4L + ok4R                                                                        in
  let ck4  =  (ck3 .&.  muk3) + kBitDiffPos 16 eck3 eok3                                        in

  let eok4 =   ok4 .&.  muk4                                                                    in
  let eck4 =  (ck4 .&. (muk4 .<. t64k4)) .>. t64k4                                              in
  let ok5L = ((ok4 .&. (muk4 .<. t64k4)) .>. t64k4)                                             in
  let ok5R = kBitDiffPos 32 eok4 eck4                                                           in
  let ok5  = ok5L + ok5R                                                                        in
  let ck5  =  (ck4 .&.  muk4) + kBitDiffPos 32 eck4 eok4                                        in

  let pak5  = c                                                                                 in
  let sak5  = 0                                                                                 in

  let hk5   = 0x00200020 .&. comp (0xffffffff .>. fromIntegral sak5)                            in
  let fk5   = ((ck5 .>. fromIntegral sak5) .|. hk5) .&. mask5                                   in
  let bk5   = ((narrow pak5 - fk5) .>. fromIntegral (wsz - 1)) - 1                              in
  let mk5   = bk5 .&. mask5                                                                     in
  let pbk5  = pak5 - widen ((ck5 .>. fromIntegral sak5) .&. mk5)                                in
  let pck5  = pbk5 + widen ((ok5 .>. fromIntegral sak5) .&. mk5)                                in
  let sbk5  = sak5 + widen (t8k5 .&. bk5)                                                       in

  let pak4  = pck5                                                                              in
  let sak4  = sbk5                                                                              in

  let hk4   = 0x00100010 .&. comp (0xffffffff .>. fromIntegral sak4)                            in
  let fk4   = ((ck4 .>. fromIntegral sak4) .|. hk4) .&. mask4                                   in
  let bk4   = ((narrow pak4 - fk4) .>. fromIntegral (wsz - 1)) - 1                              in
  let mk4   = bk4 .&. mask4                                                                     in
  let pbk4  = pak4 - widen (((ck4 .>. fromIntegral sak4) .|. hk4) .&. mk4)                      in
  let pck4  = pbk4 + widen ( (ok4 .>. fromIntegral sak4)          .&. mk4)                      in
  let sbk4  = sak4 + widen (t8k4 .&. bk4)                                                       in

  let pak3  = pck4                                                                              in
  let sak3  = sbk4                                                                              in

  let hk3   = 0x08080808 .&. comp (0xffffffff .>. fromIntegral sak3)                            in
  let fk3   = ((ck3 .>. fromIntegral sak3) .|. hk3) .&. mask3                                   in
  let bk3   = ((narrow pak3 - fk3) .>. fromIntegral (wsz - 1)) - 1                              in
  let mk3   = bk3 .&. mask3                                                                     in
  let pbk3  = pak3 - widen (((ck3 .>. fromIntegral sak3) .|. hk3) .&. mk3)                      in
  let pck3  = pbk3 + widen ( (ok3 .>. fromIntegral sak3)          .&. mk3)                      in
  let sbk3  = sak3 + widen (t8k3 .&. bk3)                                                       in

  let pak2  = pck3                                                                              in
  let sak2  = sbk3                                                                              in

  let hk2   = 0x44444444 .&. comp (0xffffffff .>. fromIntegral sak2)                            in
  let fk2   = ((ck2 .>. fromIntegral sak2) .|. hk2) .&. mask2                                   in
  let bk2   = ((narrow pak2 - fk2) .>. fromIntegral (wsz - 1)) - 1                              in
  let mk2   = bk2 .&. mask2                                                                     in
  let pbk2  = pak2 - widen (((ck2 .>. fromIntegral sak2) .|. hk2) .&. mk2)                      in
  let pck2  = pbk2 + widen ( (ok2 .>. fromIntegral sak2)          .&. mk2)                      in
  let sbk2  = sak2 + widen (t8k2 .&. bk2)                                                       in

  let pak1  = pck2                                                                              in
  let sak1  = sbk2                                                                              in

  let hk1   = 0xaaaaaaaa .&. comp (0xffffffff .>. fromIntegral sak1)                            in
  let fk1   = ((ck1 .>. fromIntegral sak1) .|. hk1) .&. mask1                                   in
  let bk1   = ((narrow pak1 - fk1) .>. fromIntegral (wsz - 1)) - 1                              in
  let mk1   = bk1  .&. mask1                                                                    in
  let pbk1  = pak1 - widen (((ck1 .>. fromIntegral sak1) .|. hk1) .&. mk1)                      in
  let pck1  = pbk1 + widen ( (ok1 .>. fromIntegral sak1)          .&. mk1)                      in
  let sbk1  = sak1 + widen (t8k1 .&. bk1)                                                       in

  let rrr   = sbk1 + pck1 + (((widen x .>. fromIntegral sbk1) .&. ((pck1 .<. 1) .|. 1)) .<. 1)  in

  rrr + p
{-# INLINE findUnmatchedCloseFar #-}
