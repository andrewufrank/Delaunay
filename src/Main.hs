-----------------------------------------------------------------------------
--
-- Module      :   a test  
------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main     where      -- must have Main (main) or Main where

 
-- import           Lib.Delaunay
import Lib.HgeometryTest

main :: IO ()
main =  do  -- with tests in other modules
    -- dirMain
    -- openMain
    hgeometry
    return ()

