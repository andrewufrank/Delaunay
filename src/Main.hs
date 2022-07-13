-----------------------------------------------------------------------------
--
-- Module      :   a test  
------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main     where      -- must have Main (main) or Main where

 
-- import           Lib.Delaunay
-- import Lib.HgeometryTest
import Lib.TryDelaunay

main :: IO ()
main =  do  -- with tests in other modules
    -- dirMain
    -- openMain
    -- hgeometry
    mainDel1
    return ()

