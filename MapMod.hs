--Gregory Cremins
--Map.hs

module MapMod(generateMap, Map, isContinuous)where
import System.Random
import Data.List
import Data.Maybe



--the Map type is a list of lists of integers
type Map = [[Int]]


--generates a test map calls fillIn to fill in its list with lists of integers length 3, to represent the three other rooms the room is connected to. The last two elements are part of the loop and the first is a randomly generated element different from the other two.
generateMap:: RandomGen g => Int -> g -> (Map, g)
generateMap si gen 
	| si < 12 = error "That is too small of a map"
	|otherwise = ((fillIn (genPath [1..(si)] (snd (next gen))) 1 (createMap si si [[]]) gen), gen)
	

--create map creates a blank map
createMap:: Int -> Int -> [[Int]] -> Map
createMap 0 s xs = xs
createMap num s xs = createMap (num - 1) s ([]:xs)


--checkRepeats check if each element in a list appears. So [1,2,3] would be true [1,2,2] would be false because 3 does not appear.
checkRepeats:: [Int] -> Int -> Int -> Bool
checkRepeats xs index s
	|(index) == (s + 1) = True
	|((elem index xs) == True) = checkRepeats xs (index + 1) s
	|otherwise = False


--genPath generates a continuous path using the checkRepeats method to make sure each element in the list appears. That way each room is represented once.	
--genPath:: [Int] -> Int -> Int -> [Int]
--genPath xs rand s
--	|((checkRepeats xs 1 s) == True) && ((checkRandom xs 1) == True) = xs
--	|otherwise = genPath (take s(randomRs (1, s) (mkStdGen rand))) (fst(random (mkStdGen rand))) s


genPath:: RandomGen g => [Int] -> g -> [Int]
genPath shuffled gen
	| (isContinuous shuffled [] (head shuffled))  = shuffled
	| otherwise = genPath (fst(shuffle shuffled (snd (next gen)))) (snd (next gen))
	


--genThreeList creates a list of 3 integers using a starting value that must be in, and 2 more unique values.	
genThreeList:: RandomGen g => [Int] -> [Int] -> g -> [Int]
genThreeList rtnLst remaining gen = (pickRandom remaining (genNum (length remaining) gen)):rtnLst


--fillin takes a conituous path, a map, a size, and a random number generator and returns a filled in map using the generateThreeList. 
fillIn:: RandomGen g => [Int] -> Int -> Map -> g -> Map
fillIn path index target gen
	|(length path) == index = [(genThreeList [(incrementTail path index), ((fromJust (elemIndex index  path)) + 1)] (filter (\x -> (x /= index) && (x /= (incrementTail path index)) && (x /=((fromJust (elemIndex index  path)) + 1))) path)  gen)]
	|otherwise = [(genThreeList [(incrementTail path index), ((fromJust (elemIndex index  path)) + 1)] (filter (\x -> (x /= index) && (x /= (incrementTail path index)) && (x /=((fromJust (elemIndex index  path)) + 1))) path)  gen)] ++ (fillIn path (index + 1) target gen) 

--showMap shows a map
showMap:: Map -> IO ()
showMap m = putStrLn (showIt m)
	where showIt m 
		|(length m) == 0 = ""
		|(rem (length (tail m)) 3) == 0 = (show (head m)) ++  " " ++ "\n"  ++ (showIt (tail m))
		|otherwise = (show(head m)) ++  ", " ++ (showIt (tail m))

--checkRanom makes sure that each element in a list is not equivalent to its index
checkRandom:: [Int] -> Int -> Bool
checkRandom xs index
	|(length xs) == 0 = True
	|(index == (head xs)) = False
	|otherwise = checkRandom (tail xs) (index + 1)

--pickRandom picks a random integer from a list	
pickRandom:: [Int] -> Int -> Int
pickRandom xs index
	|(length xs) == ((length xs) - ((length xs) - index)) = (head xs)
	|(length xs) == 0 = error "had a problem"
	|otherwise = pickRandom (tail xs) index

--genNum generates a random number	
genNum:: RandomGen g => Int -> g -> Int
genNum a g = fst(randomR (1, a) g)

--incrementTail interates a list from the tail to get the element at the required index
incrementTail:: [Int] -> Int -> Int
incrementTail xs index
	|(length xs) == index = last xs
	|otherwise = incrementTail (init xs) index
	
isContinuous:: [Int]-> [Int] -> Int -> Bool
isContinuous lst used target
	| (length used) == (length lst) = True
	| (elem target used) = False
	|otherwise = isContinuous lst (target:used) (incrementTail lst target)
	
-- basic Fisher-Yates algorithm
shuffle :: RandomGen g => [a] -> g -> ([a],g)
shuffle [] gin = ([],gin)
shuffle [x] gin = ([x],gin)
shuffle xs gin = (x:newtail,gout)
  where (i,gnxt) = randomR (0, length $ tail xs) gin
        (xs1,x:xs2) = splitAt i xs
      	(newtail,gout) = (shuffle (xs1++xs2) gnxt)