module Util where

import System.Random

safeHead :: [a] -> Maybe a
safeHead as = if not (null as) then Just (head as) else Nothing

makeRoomId :: RandomGen g => g -> (String, g)
makeRoomId g = foldl folder ("", g) [0 .. 3]
  where
    folder (roomId, g) _ =
      let (c, g') = randomR ('a', 'z') g
       in (roomId ++ [c], g')