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
{-# INLINABLE am #-}

-- | 'AM' with weight(weight is 'Int' with 1 as unit).
am' :: Num a => Int -> a -> AM a
am' w v = AM w (fromIntegral w * v)
{-# INLINABLE am' #-}

instance Num a => Semigroup (AM a) where
    AM c1 s1 <> AM c2 s2 = AM (c1 + c2) (s1 + s2)
    {-# INLINE (<>) #-}

getAM :: Fractional a => AM a -> a
getAM (AM c s) = s / fromIntegral c
{-# INLINABLE getAM #-}

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Geometric mean/
data GM a = GM {
        gmWeight  :: {-# UNPACK #-} !Int
    ,   gmProduct :: a
    } deriving (Show, Eq)

gm :: Num a => a -> GM a
gm = GM 1
{-# INLINABLE gm #-}

instance Num a => Semigroup (GM a) where
    GM c1 s1 <> GM c2 s2 = GM (c1 + c2) (s1 * s2)
    {-# INLINE (<>) #-}

getGM :: Floating a => GM a -> a
getGM (GM c p) = p ** (1 / fromIntegral c)
{-# INLINABLE getGM #-}

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Harmonic mean/
data HM a = HM {
        hmWeight  :: {-# UNPACK #-} !Int
    ,   hmSum     :: a
    } deriving (Show, Eq)

hm :: Fractional a => a -> HM a
hm x = HM 1 (1 / x)
{-# INLINABLE hm #-}

instance Num a => Semigroup (HM a) where
    HM c1 s1 <> HM c2 s2 = HM (c1 + c2) (s1 + s2)
    {-# INLINE (<>) #-}

getHM :: Fractional a => HM a -> a
getHM (HM c s) = fromIntegral c / s
{-# INLINABLE getHM #-}

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Quadratic mean/
data QM a = QM {
        qmWeight  :: {-# UNPACK #-} !Int
    ,   qmSum     :: a
    } deriving (Show, Eq)

qm :: Fractional a => a -> QM a
qm x = QM 1 (x ^ (2 :: Int))
{-# INLINABLE qm #-}

instance Num a => Semigroup (QM a) where
    QM c1 s1 <> QM c2 s2 = QM (c1 + c2) (s1 + s2)
    {-# INLINE (<>) #-}

getQM :: Floating a => QM a -> a
getQM (QM c s) = sqrt (s / fromIntegral c)
{-# INLINABLE getQM #-}

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Cubic mean/
data CM a = CM {
        cmWeight  :: {-# UNPACK #-} !Int
    ,   cmSum     :: a
    } deriving (Show, Eq)

cm :: Fractional a => a -> CM a
cm x = CM 1 (x ^ (3 :: Int))
{-# INLINABLE cm #-}

instance Num a => Semigroup (CM a) where
    CM c1 s1 <> CM c2 s2 = CM (c1 + c2) (s1 + s2)
    {-# INLINE (<>) #-}

getCM :: Floating a => CM a -> a
getCM (CM c s) = (s / fromIntegral c) ** (1/3)
{-# INLINABLE getCM #-}

--------------------------------------------------------------------------------

-- | semigroup for accumalting /Midrange mean/
data MM a = MM {
        mmMin  :: a
    ,   mmMax  :: a
    } deriving (Show, Eq)

mm :: a -> MM a
mm x = MM x x
{-# INLINABLE mm #-}

instance Ord a => Semigroup (MM a) where
    MM min1 max1 <> MM min2 max2 =
        let min' = min min1 min2
            max' = max max1 max2
        in MM min' max'
    {-# INLINE (<>) #-}

getMM :: Fractional a => MM a -> a
getMM (MM min' max') = (max' + min') / 2
{-# INLINABLE getMM #-}

--------------------------------------------------------------------------------
