-- This is NOT mine, it's just a test of ZelteHonor's code

module Bed.Chess where

import           Control.Exception   (SomeException, try)
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           System.Console.ANSI (clearScreen)

type Coor = (Int,Int)

type Board = Map.Map Coor Piece

data Couleur = Blanc | Noir

instance Show Couleur where
  show c = case c of
    Blanc -> "Blanc"
    Noir  -> "Noir"

instance Eq Couleur where
  Blanc == Blanc = True
  Noir  == Noir  = True
  _     == _     = False

data TypePiece =  Pion | Tour | Cavalier | Fou | Reine | Roi

data Piece = Piece { typeP   :: TypePiece
                   , couleur ::  Couleur}

instance Show Piece where
  show p = case p of
    Piece Tour Noir      -> "T_N"
    Piece Cavalier Noir  -> "C_N"
    Piece Fou Noir       -> "F_N"
    Piece Roi Noir       -> "RON"
    Piece Reine Noir     -> "REN"
    Piece Pion Noir      -> "P_N"
    Piece Tour Blanc     -> "T_B"
    Piece Cavalier Blanc -> "C_B"
    Piece Fou Blanc      -> "F_B"
    Piece Roi Blanc      -> "ROB"
    Piece Reine Blanc    -> "REB"
    Piece Pion Blanc     -> "P_B"

instance Eq Piece where
  (==) a b = show a == show b

boardToString :: Board -> String
boardToString board = (addRowNumber . unlines . addLineNumber) chars
    where
      addRowNumber strBoard = " " ++ concatMap ( (\s -> "  " ++ s ++ " ") . show) [(0::Int)..7] ++ "\n" ++ strBoard
      addLineNumber strBoard = zipWith (++) (map ( (++ " ") . show) [(0::Int)..7]) strBoard
      chars = [unwords [maybe " . " show (Map.lookup (x, y) board)
        | x <- [0..7]]
        | y <- [0..7]]

boardDepart :: Board
boardDepart = Map.fromList [((0,0), Piece Tour Noir),
                            ((1,0), Piece Cavalier Noir),
                            ((2,0), Piece Fou Noir),
                            ((3,0), Piece Roi Noir),
                            ((4,0), Piece Reine Noir),
                            ((5,0), Piece Fou Noir),
                            ((6,0), Piece Cavalier Noir),
                            ((7,0), Piece Tour Noir),
                            ((0,1), Piece Pion Noir),
                            ((1,1), Piece Pion Noir),
                            ((2,1), Piece Pion Noir),
                            ((3,1), Piece Pion Noir),
                            ((4,1), Piece Pion Noir),
                            ((5,1), Piece Pion Noir),
                            ((6,1), Piece Pion Noir),
                            ((7,1), Piece Pion Noir),
                            ((0,6), Piece Pion Blanc),
                            ((1,6), Piece Pion Blanc),
                            ((2,6), Piece Pion Blanc),
                            ((3,6), Piece Pion Blanc),
                            ((4,6), Piece Pion Blanc),
                            ((5,6), Piece Pion Blanc),
                            ((6,6), Piece Pion Blanc),
                            ((7,6), Piece Pion Blanc),
                            ((0,7), Piece Tour Blanc),
                            ((1,7), Piece Cavalier Blanc),
                            ((2,7), Piece Fou Blanc),
                            ((3,7), Piece Roi Blanc),
                            ((4,7), Piece Reine Blanc),
                            ((5,7), Piece Fou Blanc),
                            ((6,7), Piece Cavalier Blanc),
                            ((7,7), Piece Tour Blanc)
                            ]

inBoard :: (Int,Int) -> Bool
inBoard (x,y)= xIn && yIn
  where
    xIn = x `elem` [0..7]
    yIn = y `elem` [0..7]

validMovePiece :: Piece -> (Int,Int) -> (Int,Int) -> Bool
validMovePiece p (w,x) (y,z) = let
  (a,b) = (y - w, z - x)
  in case p of
      (Piece Tour _)     -> a == 0 || b == 0
      (Piece Cavalier _) -> abs a + abs b == 3 && abs a /= 3 && abs b /= 3
      (Piece Fou _)      -> abs a == abs b
      (Piece Roi _)      -> abs a + abs b <= 2 && abs a /= 2 && abs b /= 2
      (Piece Reine _)    -> (abs a == abs b) || (a == 0 || b == 0)
      (Piece Pion Blanc) -> a == 0 && b == -1
      (Piece Pion Noir)  -> a == 0 && b == 1

validPrisePiece :: Piece -> (Int,Int) -> (Int,Int) -> Bool
validPrisePiece p (w,x) (y,z) = let
  (a,b) = (y - w, z - x)
  in case p of
    (Piece Pion Blanc) -> (a == 1 || a == -1) && b == -1
    (Piece Pion Noir)  -> (a == 1 || a == -1) && b == 1
    _                  -> validMovePiece p (w,x) (y,z)

validMove :: (Board, Couleur) -> (Int,Int) -> (Int,Int) -> Bool
validMove (board, joueur) o f =    inBoard o && inBoard f
                      && isJust  (Map.lookup o board)
                      && joueur == (couleur . fromJust) (Map.lookup o board)
                      &&  maybe True (joueur /=)
                          (fmap couleur (Map.lookup f board) )
                      && if  isJust (Map.lookup f board) && couleur (fromJust (Map.lookup f board)) /= joueur
                          then validPrisePiece (fromJust (Map.lookup o board)) o f
                          else validMovePiece  (fromJust (Map.lookup o board)) o f
                      && cheminLibre board o f

cheminLibre :: Board -> (Int,Int) -> (Int,Int) -> Bool
cheminLibre board (w,x) (y,z) = (case fromJust (Map.lookup (w,x) board) of
  (Piece Cavalier _) -> True
  (Piece Pion _)     -> True
  (Piece Roi _)      -> True
  (Piece Tour _)     -> ligne_colonne
  (Piece Fou _)      -> diagonnal
  (Piece Reine _)    -> if w == y || x == z then ligne_colonne else diagonnal)
    where
      ligne_colonne= case () of _
                                 | w == y && x > z -> null [(w,n) | n <- [x+1, (x - 1)..z-1] , isJust $ Map.lookup (w,n) board ]
                                 | w == y && x < z -> null [(w,n) | n <- [x+1..z-1] , isJust $ Map.lookup (w,n) board ]
                                 | w > y && x == z -> null [(n,x) | n <- [w+1, (w - 1)..y-1] , isJust $ Map.lookup (n,x) board ]
                                 | w < y && x == z -> null [(n,x) | n <- [w+1..y-1] , isJust $ Map.lookup (n,x) board ]
                                 | otherwise -> False
      diagonnal = let
        a = y - w
        b = z - x
        in case () of _
                       | a == b && a > 0 -> null [ (w+n,x+n) | n <- [1..a] , isJust $ Map.lookup (w+n,x+n) board ]
                       | a == b && a < 0 -> null [ (w+n,x+n) | n <- [-1,-2..a] , isJust $ Map.lookup (w+n,x+n) board ]
                       | a > b -> null [ (w+n,x-n) | n <- [1..a] ,  isJust $ Map.lookup (w+n,x-n) board ]
                       | a < b -> null [ (w-n,x+n) | n <- [1..(-a)] ,  isJust $ Map.lookup (w-n,x+n) board ]
                       | otherwise -> False


changeBoard :: Board -> Coor -> Coor -> Board
changeBoard board ori fin = Map.delete ori (Map.insert fin (board Map.! ori) board)

gameLoop :: Board -> Couleur -> IO ()
gameLoop board joueur = do
  clearScreen
  putStrLn $ boardToString board
  putStrLn $ "Le format est (x,y) \nC'est le tour du joueur " ++ show joueur ++
            "\nEntre position origin"
  tryOrigin <- getLine
  putStrLn "Entre position final"
  tryFinal <- getLine
  origin <- try (readIO tryOrigin) :: IO (Either SomeException (Int,Int))
  final  <- try (readIO tryFinal)  :: IO (Either SomeException (Int,Int))
  case (origin, final) of
    (Left _, _ ) -> gameLoop board joueur
    ( _ ,Left _) -> gameLoop board joueur
    (Right o, Right f) -> if not $ validMove (board, joueur) o f
      then gameLoop board joueur
      else let
        board' = changeBoard board o f
        in case () of _
                        | Piece Roi Noir  `notElem` Map.elems board' -> putStrLn "Blanc a gagne!"
                        | Piece Roi Blanc `notElem` Map.elems board' -> putStrLn "Noir a gagne!"
                        | otherwise                            ->  if joueur == Blanc
                          then gameLoop board' Noir
                          else gameLoop board' Blanc

main :: IO ()
main = gameLoop boardDepart Blanc
