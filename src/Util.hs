module Util (randomNumber) where

import System.Random
import System.IO.Unsafe

randomNumber :: Double
randomNumber = unsafePerformIO randomIO
