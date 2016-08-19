--Greg Cremins
--Game.hs
import System.Random
import MapMod

--                   	DATA TYPES
data Player = Player {arrows:: Int, adjacentRooms:: [Int], getR::Int}
data Wumpus = Wumpus {adjRooms:: [Int], getRoom:: Int}
data Game = Game {getMap:: Map, getPlayer:: Player, getWumpus:: Wumpus, isOver:: Int}

--              	GAME LOOP FUNCTIONS
	
setUpGame:: RandomGen g => Int -> g -> Game
setUpGame size gen = makeGame (fst(generateMap size gen)) (makePlayer 5 (fst(generateMap size gen)) [1..size] gen) gen

--makeGame makes a new game	
makeGame:: RandomGen g => Map -> Player -> g -> Game
makeGame m p gen  = Game m p (makeWumpus m (filter (\x -> x /= (getR p)) [1..(length m)]) gen) 0

--game loop to make sure the wumpus does not eat the player when a shot is fired
miniGameLoop:: RandomGen g => Game -> g -> IO()
miniGameLoop game gen = do
			if ((isOver game) > 0) then putStrLn (endText game)
			else gameLoop game gen

--game loop to continue input and output
gameLoop:: RandomGen g => Game -> g -> IO ()
gameLoop game gen  = do
			ioCheckSenses game gen
			putStrLn "Would you like to move (m) or shoot (s)?"
			--putStrLn ("The wumpus is in room " ++ (show (getRoom (getWumpus game))) ++ "." ) FOR DEBUGGING
			action <- getLine
			putStrLn "Three rooms lie ahead of you, pick either 1, 2, or 3."
			rawRoom <- getLine
			room <- return $ (read rawRoom :: Int)
			
			if(action == "m") then if((move game (getPlayer game) room) == True) then gameLoop (makeMove game room) (snd(next gen))
						else putStrLn (endText (makeMove game room))
			else if (action == "s" && (canShoot (getPlayer game))) then if ((getRoom (getWumpus game)) == (incrementTail (adjacentRooms(getPlayer game)) room)) then putStrLn(endText(takeShot game (incrementTail (adjacentRooms(getPlayer game)) room) gen))
						else miniGameLoop (takeShot game (incrementTail (adjacentRooms(getPlayer game)) room) gen) (snd(next gen))
			else if (action == "s" && (canShoot(getPlayer game)) == False) then putStrLn "You are out of arrows. \nGame over."
			else error "not a valid command"
	
	
--program to check senses using pre made functions
ioCheckSenses::RandomGen g => Game -> g -> IO()
ioCheckSenses game gen = do
			putStrLn(checkSenses (adjacentRooms (getPlayer game)) (getWumpus game))
			putStrLn ("You have " ++ (getArrows game) ++ " arrows.")
			putStrLn (printRoomString game)
	
--getArrows returns the number of arrows the player has left
getArrows:: Game -> String
getArrows g
	|(arrows (getPlayer g)) < 0 = "0"
	|otherwise = show (arrows (getPlayer g))
	
	
--main function to run the game
main:: IO()
main = do
	putStrLn "Give me the size of the game board."
	rawSize <- getLine
	size <- return $ (read rawSize :: Int)
	seed <- rollForWorld
	gameLoop (setUpGame size (mkStdGen seed)) (mkStdGen seed)
	putStrLn "Thank you for playing!"

--generates a random int for the seed value
rollForWorld :: IO Int
rollForWorld = getStdRandom (randomR (1,1000))



--     			PRINTING FUNCTIONS

--end game text generator
endText:: Game -> String
endText g
	|((isOver g) == 1) = "You have shot the wumpus. You win!"
	|((isOver g) == 2) = "OMN NOM NOM NOM. The Wumpus ate you."
	|otherwise = error "PROBLEMS"

--Prints the room number 
printRoomString:: Game -> String
printRoomString gen = "You are in room " ++ (show(getR (getPlayer gen))) ++ "."



--			WUMPUS FUNCTIONS
--function to move the wumpus and return a new gamestate
moveWumpus:: RandomGen g => Game -> g -> Game
moveWumpus ga gen  
	|(getRoom (makeWumpus (getMap ga) (filter (\x -> x /= (getRoom (getWumpus ga))) [1..(length (getMap ga))]) gen)) == (getR (getPlayer ga)) =  Game (getMap ga) (getPlayer ga) (makeWumpus (getMap ga) (filter (\x -> x /= (getRoom (getWumpus ga))) [1..(length (getMap ga))]) gen) 2
	|otherwise =  Game (getMap ga) (getPlayer ga) (makeWumpus (getMap ga) (filter (\x -> x /= (getRoom (getWumpus ga))) [1..(length (getMap ga))]) gen) (isOver ga)

--checkWump returns whether there is a wumpus in the next room that the player is interacting.
containsWump:: Int -> Wumpus -> Bool
containsWump checkDirection wump = checkDirection == (getRoom wump)
	

--makes a wumpus
makeWumpus:: RandomGen g => Map -> [Int] -> g -> Wumpus
makeWumpus m rooms g = Wumpus (getAdjRooms (chooseRandom rooms g) m) (chooseRandom rooms g)


--			PLAYER FUNCTIONS


checkSenses:: [Int] -> Wumpus -> String
checkSenses rooms w
	| (length rooms) <= 0 = ""
	| (getRoom w) == (head rooms) = "Eww! Something stinks!"
	| otherwise = checkSenses (tail rooms) w
	
	
--makes a player
makePlayer:: RandomGen g => Int -> Map -> [Int] -> g -> Player
makePlayer arrows m rooms g = Player arrows (getAdjRooms (chooseRandom rooms g) m) (chooseRandom rooms g)


--checks if a player can shoot
canShoot:: Player -> Bool
canShoot p = ((arrows p) > 0)


--checks if the player moved into a valid place not occupied by the wumpus. If so it returns true. Otherwise, it returns false.
move:: Game -> Player -> Int -> Bool
move g p target
	|(containsWump (incrementTail (adjacentRooms p) target) (getWumpus g)) == True = False
	|otherwise = True


--makeMove makes the player move
makeMove:: Game -> Int -> Game
makeMove g target
	|(move g (getPlayer g) target) == True = Game (getMap g) (Player (arrows (getPlayer g)) (getAdjRooms (incrementTail (adjacentRooms (getPlayer g)) target) (getMap g)) (incrementTail (adjacentRooms (getPlayer g)) target)) (getWumpus g) (isOver g)
	|otherwise = Game (getMap g) (getPlayer g) (getWumpus g) 2



--shoot shoots a bow from the player to a selected room
shoot:: Game -> Player -> Int -> (Player, Bool)
shoot g p target 
	|(containsWump target (getWumpus g) ) == True = ((Player ((arrows p) - 1) (adjacentRooms p) (getR p)), True)
	|otherwise = ((Player ((arrows p) - 1) (adjacentRooms p) (getR p)), False)


takeShot::RandomGen g =>  Game -> Int -> g ->  Game
takeShot g target gen
	|snd(shoot g (getPlayer g) target) == True = Game (getMap g) (fst(shoot g (getPlayer g) target)) (getWumpus g) 1
	|otherwise = moveWumpus (Game (getMap g) (fst (shoot g (getPlayer g) target)) (getWumpus g) (isOver g)) gen
	
	


chooseRandom:: RandomGen g => [Int] -> g -> Int
chooseRandom  remainRooms gen 
	|(fst(randomR (1, (length remainRooms)) gen)) == (length remainRooms) = last remainRooms
	|(fst(randomR (1, (length remainRooms)) gen)) >= (length remainRooms) = error "THERE WAS PROBLEM"
	|otherwise = chooseRandom (init remainRooms) gen



--			HELPER FUNCTIONS

	
--incrementTail interates a list from the tail to get the element at the required index
incrementTail:: [Int] -> Int -> Int
incrementTail xs index
	|(length xs) == index = last xs
	| index > 3 = error "Not a valid selection"
	|otherwise = incrementTail (init xs) index
	
--getAdjRooms returns the adjacent rooms to a given space	
getAdjRooms:: Int -> Map -> [Int]
getAdjRooms target m 
	| (length m) < target  = error "We had a problem in getAdjRooms."
	| (length m) == target = last m
	| otherwise = getAdjRooms target (init m)