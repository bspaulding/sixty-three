module Util where

safeHead :: [a] -> Maybe a
safeHead as = if not (null as) then Just (head as) else Nothing
