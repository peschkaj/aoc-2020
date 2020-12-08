module Day8 where

import qualified Data.Set as Set
import qualified Data.Vector as V

data Instruction = Acc Int
                 | Jmp Int
                 | Nop Int
  deriving (Show, Eq, Ord)

parse :: String -> [Instruction]
parse = map transInstr . lines
  where
    num = read . dropWhile (== '+')
    transInstr line = case words line of
      ["acc", n] -> Acc $ num n
      ["jmp", n] -> Jmp $ num n
      ["nop", n] -> Nop $ num n


part1 :: String -> Int
part1 input = go 0 0 Set.empty
   where
     program = V.fromList $ parse input
     l = V.length program
     go :: Int -> Int -> Set.Set Int -> Int
     go ip acc ips
       | Set.member ip  ips = acc
       | ip < 0 || ip >= l  = go (mod ip l) acc ips
       | otherwise          = case (program V.! ip) of
           Acc n -> go (succ ip) (acc + n) (Set.insert ip ips)
           Jmp n -> go (ip + n)  acc       (Set.insert ip ips)
           Nop _ -> go (succ ip) acc       (Set.insert ip ips)

part2 :: String -> Int
part2 input = go [(0, Set.empty, False, 0)]
  where
    program = V.fromList $ parse input
    l = V.length program

    go ((ip, ips, ch, acc):rest)
      | ip `Set.member` ips = go rest
      | ip == l && ch       = acc
      | ip < 0 || ip >= l   = go ((ip`mod`l, ips, ch, acc) : rest)
      | otherwise           =
          let ips' = Set.insert ip ips
          in  case program V.! ip of
                  Acc n      -> go ((succ ip, ips', ch, acc+n):rest)
                  Jmp n | ch -> go ((n +  ip, ips', ch, acc  ):rest)
                  Jmp n      -> go ((n +  ip, ips', ch, acc  ):(succ ip, ips', True, acc):rest)
                  Nop _ | ch -> go ((succ ip, ips', ch, acc  ):rest)
                  Nop n      -> go ((succ ip, ips', ch, acc  ):(n +  ip, ips', True, acc):rest)
