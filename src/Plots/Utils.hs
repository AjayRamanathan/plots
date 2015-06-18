{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Plots.Utils
    ( projection
    , autoSteps
    , linearMap
    , linearMapBound
    , hasNaN
    , text'
    ) where


import Control.Lens.Internal.Fold
import Data.Foldable
import Control.Lens
import Control.Lens.Internal
import Data.Profunctor.Unsafe
import Data.Monoid.Recommend
import Linear
import Data.Ord (comparing)
import Data.Function

import Diagrams.Prelude
import Graphics.SVGFonts.ReadFont
import Diagrams.Backend.Cairo.Text

import Data.List
import Plots.Types

-- | @enumFromToN a b n@ calculates a list from @a@ to @b@ in @n@ steps.
enumFromToN :: Fractional n => n -> n -> Int -> [n]
enumFromToN a b n = step n a
  where
    step !i !x | i < 1     = [x]
               | otherwise = x : step (i - 1) (x + diff)
    diff = (b - a) / fromIntegral n

------------------------------------------------------------------------
-- Lens
------------------------------------------------------------------------

-- Index an optic, starting from 1.
oneindexing :: Indexable Int p => ((a -> Indexing f b) -> s -> Indexing f t) -> p a (f b) -> s -> f t
oneindexing l iafb s = snd $ runIndexing (l (\a -> Indexing (\i -> i `seq` (i + 1, indexed iafb i a))) s) 1
{-# INLINE oneindexing #-}

oneeach :: Each s t a b => IndexedTraversal Int s t a b
oneeach = conjoined each (indexing each)
{-# INLINE oneeach #-}

onefolded :: Foldable f => IndexedFold Int (f a) a
onefolded = conjoined folded' (indexing folded')
{-# INLINE onefolded #-}

folded' :: Foldable f => Fold (f a) a
folded' f = coerce . getFolding . foldMap (Folding #. f)
{-# INLINE folded' #-}

------------------------------------------------------------------------
-- Recommend
------------------------------------------------------------------------

liftRecommend :: (a -> a -> a) -> Recommend a -> Recommend a -> Recommend a
liftRecommend _ (Commit a) (Recommend _)    = Commit a
liftRecommend _ (Recommend _) (Commit b)    = Commit b
liftRecommend f (Recommend a) (Recommend b) = Recommend (f a b)
liftRecommend f (Commit a) (Commit b)       = Commit (f a b)

-- recommend :: Lens' (Recommend a) a
-- recommend = lens getRecommend setRecommend
--   where
--     setRecommend (Recommend _) a = Recommend a
--     setRecommend (Commit _   ) a = Commit a

-- _Recommend :: Prism' (Recommend a) a
-- _Recommend = prism' Recommend getRec
--   where
--     getRec (Recommend a) = Just a
--     getRec _             = Nothing

-- _Commit :: Prism' (Recommend a) a
-- _Commit = prism' Commit getCommit
--   where
--     getCommit (Commit a) = Just a
--     getCommit _          = Nothing

fromCommit :: a -> Recommend a -> a
fromCommit _ (Commit a) = a
fromCommit a _          = a

------------------------------------------------------------------------
-- Diagrams
------------------------------------------------------------------------

pathFromVertices :: (Metric v, OrderedField n) => [Point v n] -> Path v n
pathFromVertices = fromVertices

-- -- | The @themeEntry@ lens goes though recommend, so @set themeEntry myTheme
-- --   myPlot@ won't give a committed theme entry (so theme from axis will
-- --   override). Use commitTheme to make sure theme is committed.
-- commitTheme :: HasPlotProperties a => ThemeEntry (B a) (N a) -> a -> a
-- commitTheme = set plotThemeEntry . Commit
--
-- -- | Make the current theme a committed theme. See @commitTheme@.
-- commitCurrentTheme :: HasPlotProperties a => a -> a
-- commitCurrentTheme = over plotThemeEntry makeCommitted
--   where
--     makeCommitted (Recommend a) = Commit a
--     makeCommitted c             = c

-- | project a 3d point to 2d
---------------------------------------------------------------------------
--projection :: (Double, Double, Double)  -- ^ position of camera
--           -> (Double, Double, Double)  -- ^ orientation of camera
--           -> (Double, Double, Double)  -- ^ viewer's position
--           -> (Double, Double, Double)  -- ^ 3d point to be projected
 --          -> (Double, Double)
--projection (cx',cy',cz') (θx,θy,θz) (ex,ey,ez) (ax,ay,az) = (bx, by)
--  where
--    bx = ez / dz * dx - ex
--    by = ez / dz * dy - ey
--    dx = cy * (sz * y + cz * x) - sy * z
--    dy = sx * (cy * z + sy * (sz * y + cz * x)) + cx * (cz * y - sz * x)
--    dz = cx * (cy * z + sy * (sz * y + cz * x)) - sx * (cz * y - sz * x)
--    x = ax - cx'
--    z = az - cz'
 --   sx = sin θx
 --   sy = sin θy
--    sz = sin θz
--    cx = cos θx
--    cy = cos θy
--    cz = cos θz
-----------------------------------------------------------------------------------------------
chooseStep :: RealFloat a => a -> (a,a) -> Rational
chooseStep nsteps (x1,x2) = minimumBy (comparing proximity) stepVals
  where
    delta = x2 - x1
    mult  = 10 ^^ ((floor $ logBase 10 $ delta / nsteps)::Integer)
    stepVals = map (mult*) [0.1,0.2,0.25,0.5,1.0,2.0,2.5,5.0,10,20,25,50]
    proximity x = abs $ delta / realToFrac x - nsteps
{-# INLINE chooseStep #-}

-- | Given a target number of values, and a list of input points,
--   find evenly spaced values from the set {1*X, 2*X, 2.5*X, 5*X} (where
--   X is some power of ten) that evenly cover the input points.
autoSteps :: Int -> (Double, Double) -> (Rational, Rational, Rational)
autoSteps nSteps (minV, maxV) = (min', max', step)
  where
    r@(minV', maxV')  | minV == maxV = (minV-0.5,minV+0.5)
                      | otherwise    = (minV, maxV)
    step = chooseStep (fromIntegral nSteps) r
    min' = fromIntegral (floor   $ realToFrac minV' / step :: Integer) * step
    max' = fromIntegral (ceiling $ realToFrac maxV' / step :: Integer) * step

linearMap :: (Double, Double) -> (Double, Double) -> PointMap Double
linearMap (l, u) (l', u') = PointMap mapFn (l, u)
  where
    mapFn x | x < l || x > u = Nothing
            | otherwise = Just $ (x - l) / (u - l) * (u' - l') + l'
{-# INLINE linearMap #-}

linearMapBound :: (Double, Double) -> (Double, Double) -> PointMap Double
linearMapBound (l, u) (l', u') = PointMap mapFn (l, u)
  where
    mapFn x | x < l = Just l' 
            | x > u = Just u'
            | otherwise = Just $ (x - l) / (u - l) * (u' - l') + l'
{-# INLINE linearMapBound #-}

hasNaN :: [(Double, Double)] -> Bool
hasNaN = any (uncurry ((||) `on` isNaN))

-- slants for fonts.
data FontSlant = FontSlantNormal  -- ^ normal.
               | FontSlantItalic  -- ^ italic.
               | FontSlantOblique -- ^ oblique.
               deriving (Show, Eq, Ord)

-- default.
instance Default FontSlant where
  def = FontSlantNormal

-- bold normal.
data FontBN = FontBNNormal -- normal
           | FontBNBold   -- bold
           deriving (Show, Eq, Ord)

-- def
instance Default S where
  def = FontBNNormal

-- font data type
data FontStyle = FontStyle {
      _font_name   :: String, -- font to use
      _font_size   :: Double, -- size
      _font_slant  :: FontSlant,
      _font_bn     :: FontBN,
      _font_color  :: AlphaColour Double -- color
} deriving (Show, Eq)

-- def
instance Default FontStyle where
  def = FontStyle 
    { _font_name   = "sans-serif"
    , _font_size   = 10
    , _font_slant  = def
    , _font_bn = def
    , _font_color  = opaque black
    }

-- horizontal text anchor HTA-VTA
data HorizontalTA = HTA_Left 
                  | HTA_Centre 
                  | HTA_Right 
                  deriving (Show, Eq, Ord)

-- vertical
data VerticalTA =  VTA_Top 
                 | VTA_Centre 
                 | VTA_Bottom 
                 | VTA_BaseLine 
                 deriving (Show, Eq, Ord)

-- | Draw a line of text that is aligned at a different anchor point.
--   See 'drawText'.
drawTextA :: HorizontalTA -> VerticalTA -> Point -> String -> DiaR2
drawTextA hta vta = drawTextR hta vta 0

{- 
drawTextA hta vta p txt =
  drawTextR hta vta 0 p txt 
  >> withLineStyle (solidLine 1 (opaque red)) 
     (textDrawRect hta vta p txt
       >>= \rect -> alignStrokePath (rectPath rect) >>= strokePath)
-}

drawTextR :: HorizontalTA -> VerticalTA -> Double -> Point -> String -> DiaR2
drawTextR hta vta angle p s =
  withTranslation p $
    withRotation theta $ do
      ts <- textSize s
      drawText (adjustText hta vta ts) s
  where
    theta = angle*pi/180.0

drawTextsR :: HorizontalTA -> VerticalTA -> Double -> Point -> String -> DiaR2
drawTextsR hta vta angle p s = case num of
      0 -> return ()
      1 -> drawTextR hta vta angle p s
      _ -> 
        withTranslation p $
          withRotation theta $ do
            tss <- mapM textSize ss
            let ts = head tss
            let maxh   = maximum (map textSizeYBearing tss)
                gap    = maxh / 2
                totalHeight = fromIntegral num*maxh +
                              (fromIntegral num-1)*gap
                ys = take num (unfoldr (\y-> Just (y, y-gap-maxh))
                                       (yinit vta ts totalHeight))
                xs = map (adjustTextX hta) tss
            sequence_ (zipWith3 drawT xs ys ss)
    where
      ss   = lines s
      num  = length ss

      drawT x y = drawText (Point x y)
      theta = angle*pi/180.0

      yinit VTA_Top      ts _      = textSizeAscent ts
      yinit VTA_BaseLine _  _      = 0
      yinit VTA_Centre   ts height = height / 2 + textSizeAscent ts
      yinit VTA_Bottom   ts height = height + textSizeAscent ts

adjustText :: HorizontalTA -> VerticalTA -> TextSize -> Point
adjustText hta vta ts = Point (adjustTextX hta ts) (adjustTextY vta ts)

adjustTextX :: HorizontalTAr -> TextSize -> Double
adjustTextX HTA_Left   _  = 0
adjustTextX HTA_Centre ts = - (textSizeWidth ts / 2)
adjustTextX HTA_Right  ts = - textSizeWidth ts

adjustTextY :: VerticalTA -> TextSize -> Double
adjustTextY VTA_Top      ts = textSizeAscent ts
adjustTextY VTA_Centre   ts = - textSizeYBearing ts / 2
adjustTextY VTA_BaseLine _  = 0
adjustTextY VTA_Bottom   ts = - textSizeDescent ts

{-
textDrawRect :: HorizontalTA -> VerticalTA -> Point -> String -> DiaR2
textDrawRect hta vta (Point x y) s = do
  ts <- textSize s
  let (w,h,dh) = (textSizeWidth ts, textSizeHeight ts, textSizeDescent ts)
      lx = adjustTextX hta ts
      ly = adjustTextY vta ts
      (x',y') = (x + lx, y + ly + dh)
      p1 = Point x' (y' - h)
      p2 = Point (x' + w) y'
  return $ Rect p1 p2

textDimension :: String -> (Double, Double)
textDimension s = do
  ts <- textSize s
  return (textSizeWidth ts, textSizeHeight ts)
-}

-- linecap
data LineCap = LineCapButt   -- straight.
             | LineCapRound  -- rounded
             | LineCapSquare -- square.
             deriving (Show, Eq, Ord)

-- linestyle
data LineStyle = LineStyle 
  { _line_width  :: Double  -- size
  , _line_color  :: AlphaColour Double -- colour
  , _line_dashes :: [Double]-- dash
  , _line_cap    :: LineCap -- line
  } deriving (Show, Eq)

-- def
instance Default LineStyle where
  def = LineStyle 
    { _line_width  = 1
    , _line_color  = opaque black
    , _line_dashes = []
    , _line_cap    = LineCapButt
    }

-- text -----------------------------------------------------------------
--data TextSize = TextSize 
--  { textSizeWidth    :: Double -- width
--  , textSizeAscent   :: Double -- ascent
-- , textSizeDescent  :: Double -- decent
--  , textSizeYBearing :: Double -- y
--  , textSizeHeight   :: Double -- h
--  } deriving (Show, Eq)

-- -----------------------------------------------------------------------

--text' :: Double -> String -> DiaR2
--text' size str = stroke (textSVG' (TextOpts str lin2 INSIDE_WH HADV False size size)) # fc black # lwL 0

--text' :: Double -> String -> DiaR2
--text' size str = textVisualBounded (fontSize (Local size) mempty) str # fontSize (Local size)

text' :: Double -> String -> DiaR2
text' size str = text str # fontSizeL size
