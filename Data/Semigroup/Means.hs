module Data.Semigroup.Means where

import Data.Semigroup

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Arithmetic mean/
data AM a = AM {
        amWeight :: {-# UNPACK #-} !Int
    ,   amSum    :: a
    } deriving (Show, Eq)

-- | 'AM' with weight 1.
am :: Num a => a -> AM a
am = AM 1

-- | 'AM' with weight(weight is 'Int' with 1 as unit).
am' :: Num a => Int -> a -> AM a
am' w v = AM w (fromIntegral w * v)

instance Num a => Semigroup (AM a) where
    AM c1 s1 <> AM c2 s2 = AM (c1 + c2) (s1 + s2)

getAM :: Fractional a => AM a -> a
getAM (AM c s) = s / fromIntegral c

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Geometric mean/
data GM a = GM {
        gmWeight  :: {-# UNPACK #-} !Int
    ,   gmProduct :: a
    } deriving (Show, Eq)

gm :: Num a => a -> GM a
gm = GM 1

instance Num a => Semigroup (GM a) where
    GM c1 s1 <> GM c2 s2 = GM (c1 + c2) (s1 * s2)

getGM :: Floating a => GM a -> a
getGM (GM c p) = p ** (1 / fromIntegral c)

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Harmonic mean/
data HM a = HM {
        hmWeight  :: {-# UNPACK #-} !Int
    ,   hmSum     :: a
    } deriving (Show, Eq)

hm :: Fractional a => a -> HM a
hm x = HM 1 (1 / x)

instance Num a => Semigroup (HM a) where
    HM c1 s1 <> HM c2 s2 = HM (c1 + c2) (s1 + s2)

getHM :: Fractional a => HM a -> a
getHM (HM c s) = fromIntegral c / s

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Quadratic mean/
data QM a = QM {
        qmWeight  :: {-# UNPACK #-} !Int
    ,   qmSum     :: a
    } deriving (Show, Eq)

qm :: Fractional a => a -> QM a
qm x = QM 1 (x ^ 2)

instance Num a => Semigroup (QM a) where
    QM c1 s1 <> QM c2 s2 = QM (c1 + c2) (s1 + s2)

getQM :: Floating a => QM a -> a
getQM (QM c s) = sqrt (s / fromIntegral c)

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Cubic mean/
data CM a = CM {
        cmWeight  :: {-# UNPACK #-} !Int
    ,   cmSum     :: a
    } deriving (Show, Eq)

cm :: Fractional a => a -> CM a
cm x = CM 1 (x ^ 3)

instance Num a => Semigroup (CM a) where
    CM c1 s1 <> CM c2 s2 = CM (c1 + c2) (s1 + s2)

getCM :: Floating a => CM a -> a
getCM (CM c s) = (s / fromIntegral c) ** (1/3)

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Midrange mean/
data MM a = MM {
        mmMin  :: a
    ,   mmMax  :: a
    } deriving (Show, Eq)

mm :: a -> MM a
mm x = MM x x

instance Ord a => Semigroup (MM a) where
    MM min1 max1 <> MM min2 max2 =
        let min' = min min1 min2
            max' = max max1 max2
        in MM min' max'

getMM :: Fractional a => MM a -> a
getMM (MM min max) = (max + min) / 2

--------------------------------------------------------------------------------
