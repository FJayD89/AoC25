module Day1 where

toRot :: String -> Int
toRot ('R':d) = read d
toRot ('L':d) = (-1) * read d 



main = do
  txt <- readFile "./1.txt"
  
  print $ scanl up (0,50) $ map toRot $ lines txt
 
up (c, s) n = (c', s')
 where    
  to0 = md (s * signum n)
  s' = md (n + s)
  c' = c + dv (abs n + to0)
      
md k = mod k 100
dv k = div k 100
  
  
