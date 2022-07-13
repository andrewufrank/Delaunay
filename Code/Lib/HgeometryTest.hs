-----------------------------------------------------------------------------
--
-- Module      :   a more elaborate sub
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib.HgeometryTest  
     where

import UniformBase
-- import Algorithms.Geometry.DelaunayTriangulation.Naive

import Data.Geometry

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- module Demo.Delaunay where

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Algorithms.Geometry.EuclideanMST
import           Control.Lens
import           Data.Data
import           Data.Ext
-- import           Geometry
-- import           Geometry.PlanarSubdivision
-- import           Geometry.PlanarSubdivision.Draw
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           Data.Semigroup
-- import           Data.Tree.Draw
-- import           Ipe
-- import           Ipe.Color
-- import           Options.Applicative

----------------------------the delaunay example https://raw.githubusercontent.com/noinia/hgeometry/master/hgeometry-examples/src/Demo/Delaunay.hs

type R = RealNumber 5

data Options = Options { _inPath    :: FilePath
                       , _outFile   :: FilePath
                       }
               deriving Data

-- options :: ParserInfo Options
-- options = info (helper <*> parser)
--                (  progDesc "Compute the Delaunay Triangulation of the points in the input file."
--                <> header   "Delaunay"
--                )
--   where
--     parser = Options
--           <$> strOption (help "Input file (in ipe7 xml format)"
--                          <> short 'i'
--                         )
--           <*> strOption (help "Output File (in ipe7 xml format)"
--                          <> short 'o'
--                         )

mainAF :: IO ()
mainAF  = do 
    pts <- readAllFrom @(Point 2 R) inFile
    let pts' = NonEmpty.fromList pts
        dt   = delaunayTriangulation $ pts' 
    return ()


-- mainWith                          :: Options -> IO ()
-- mainWith (Options inFile outFile) = do
--   pts <- readAllFrom @(Point 2 R) inFile
--   let pts' = NonEmpty.fromList pts
--       dt   = toPlanarSubdivision @DTWorld . delaunayTriangulation $ pts'
--       emst = euclideanMST pts'
--       out  = [ iO $ drawPlanarSubdivisionWith drawVtx drawEdge (drawInternalFace dt) drawOuterFace dt
--                   ! attr SLayer "delaunayTriangulation"
--              , iO $ drawTree' emst ! attr SLayer "emst"
--              ]
--       outputFile = singlePageFromContent out
--   outputFile' <- addStyleSheetFrom "../hgeometry-ipe/resources/opacities.isy" outputFile
--   writeIpeFile outFile outputFile'

-- -- | The world in which the delaunay triangulation "lives"
-- data DTWorld

-- -- | Draw vertices using their default representation; disk marks. For
-- -- the rest we keep their original attributes.
-- drawVtx                         :: IpeOut' Maybe (VertexId' s, VertexData r (IpeAttributes IpeSymbol r)) IpeSymbol r
-- drawVtx (_vi, VertexData p ats) = Just $ defIO p ! ats

-- -- | Draw edges using normal line segments
-- drawEdge              :: IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  Path r
-- drawEdge (_d, s :+ _) = Just $ defIO s

-- -- | Internal faces are filled polygons.
-- drawInternalFace                 :: PlanarSubdivision s v e f r
--                                  -> IpeOut' Maybe (FaceId' s,   SomePolygon v r :+ f)    Path r
-- drawInternalFace s (fi, pg :+ _) = Just $ defIO pg ! attr SFill lightcyan

-- -- | Draw the outer face (in some box)
-- drawOuterFace :: (Ord r, Num r) => IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f) Path r
-- drawOuterFace (_, pg :+ _) = Just $ defIO pg ! attr SOpacity "10%"
--                                              ! attr SFill lightgray

  -- const Nothing


hgeometry:: IO ()
hgeometry = do 
    putIOwords ["hgeometry"]

    return ()