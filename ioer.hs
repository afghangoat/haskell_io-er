import Data.Char  
import Data.Array.IO
import Prelude hiding (map) 

func1 :: Integer -> Integer -> Integer  --takes 2 integer values and returns one integer
func1 x y = x*x + y*y

increment :: Integer -> Integer
increment x =x+1

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n newVal xs =
  take n xs ++ [newVal] ++ drop (n + 1) xs

eveno :: Int -> Bool
eveno x = if x `rem` 2 == 0 
   then True 
else False 

fact :: Integer -> Integer 
fact n | n == 0 = 1 
       | n /= 0 = n * fact (n-1) 

map :: (a -> b) -> [a] -> [b]  --is list empty: null x
map _ [] = [] 
map func (x : abc) = func x : map func abc 

readInt :: String -> Int  --Example: readInt "12"
readInt = read

isNum:: Char-> Bool 
isNum x = if x == '0' ||x == '1' ||x == '2' ||x == '3' ||x == '4' ||x == '5' ||x == '6' ||x == '7' ||x == '8'||x == '9'
   then True 
else False 

-- show returns string 
main = do 
  
  memory <- newArray (0, 9) 0 :: IO (IOArray Int Int)
  
  putStrLn "This is a very basic language, you have 10 memory slots, \nyou can set the slots to 1 using the <number> operator,\nand print out the memory using the 'A' operator.\n To reset the cells to 0, use the 'B' operator. Example code: 1234AB"
  putStrLn "Input the code for interpretation to STDIN!" 
  
  input_lang <- getLine
  
  let modified = concatMap (\y -> [y]) input_lang
  
  mapM_ (\y -> 
    if isNum y 
            then writeArray memory (digitToInt y) 1
        else if y == 'A' 
            then do
                updatedMemory <- getElems memory
                print updatedMemory
    else if y=='B'
      then do
        mapM_ (\i -> writeArray memory i 0) [0..9]
    else return ()
	) input_lang
