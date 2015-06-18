{-# LANGUAGE TemplateHaskell #-}
{-
module Plots.Types.Pie(
) where


data PieChart = PieChart {
   _data             :: [PieItem],
   _colors           :: [AlphaColour Double],
   _label_style      :: FontStyle,
   _pie_label_line_style :: LineStyle, 
   _pie_start_angle      :: Double

}


instance Default PieChart where
  def = PieChart 
    { _pie_data             = []
    , _pie_colors           = defaultColorSeq
    , _pie_label_style      = def
    , _pie_label_line_style = solidLine 1 $ opaque black
    , _pie_start_angle      = 0
    }
-}

