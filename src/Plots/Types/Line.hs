{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
-- Orphans: Plotable (Path V2 n)

module Plots.Types.Line     
    ( line
    , LineOpts
    , lineshape
    ) where

import Control.Lens     hiding (transform, ( # ), lmap)
import Data.Foldable    as F
-- import Data.Typeable
import Diagrams.Prelude
-- import Diagrams.LinearMap
-- import Diagrams.ThreeD.Types

import Diagrams.Coordinates.Isomorphic

import Plots.Themes
import Plots.Types
import Plots.Utils (hasNaN)

data LineOpts = LineOpts
    { _lineshape :: Char
    }

makeLenses ''LineOpts

instance Default LineOpts where
    def = LineOpts
        { _lineshape = 'o'
        }

-- figureout a way to add strokesize, alpha and size; add scales

mkTrail :: (PointLike v n p, OrderedField n, Foldable f) => f p -> Located (Trail v n)
mkTrail = mkTrailOf folded

mkTrailOf :: (PointLike v n p, OrderedField n) => Fold s p -> s -> Located (Trail v n)
mkTrailOf f ps = fromVertices $ toListOf (f . unpointLike) ps

mkPathOf :: (PointLike v n p, OrderedField n) => Fold s t -> Fold t p -> s -> Path v n
mkPathOf f1 f2 as = Path $ map (mkTrailOf f2) (toListOf f1 as)

mkPath :: (PointLike v n p, OrderedField n, Foldable f, Foldable g) => g (f p) -> Path v n
mkPath = mkPathOf folded folded

instance (TypeableFloat n, Renderable (Path V2 n) b) => Plotable (Path V2 n) b where
  renderPlotable s path pp
    = stroke path
        # transform (s^.specTrans)
        # applyLineStyle pp

  defLegendPic _ pp
    = (p2 (-10,0) ~~ p2 (10,0))
        # applyLineStyle pp

line :: (PlotData m1 a1, PlotData m2 a2) => m1 a1 -> m2 a2 -> LineOpts -> PlotFn
line xs ys opt mapX mapY | hasNaN xy = error "Line: Found NaN"
                            | otherwise = [l]
  where
    l = lwO 1 . fromVertices . map p2 . mapMaybe (runMap pMap) $ xy
    xy = zip (getValues xs) $ getValues ys
    pMap = compose mapX mapY

------------------------------------------------------------------------
-- Sample
------------------------------------------------------------------------

-- kernalDensity :: Int -> Fold s n -> s -> LinePlot V2 n
-- kernalDensity n f as =


