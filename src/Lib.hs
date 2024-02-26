{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}

module Lib (shakki) where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import qualified Text.Read as Read
import System.Exit ( exitSuccess ) 
import System.IO (hFlush, stdout)

data Player = Black | White deriving (Show, Eq, Ord)
data Piece = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq, Ord)
data OwnedPiece = OwnedPiece Piece Player deriving (Show, Eq, Ord)
data Pos = Pos Int Int deriving (Show, Eq, Ord)
data Board = Board (Map Pos OwnedPiece) deriving (Show, Eq)
data History = History [Board] deriving (Show, Eq)
data Game = Game Board Player History deriving (Show, Eq)
data User = Asking | MoveFrom | MoveTo Pos deriving (Show, Eq)
data State = State Game User deriving (Show, Eq)

-- Kunkin shakkinappulan Unicode-merkki
merkit :: Map OwnedPiece String
merkit = Map.fromList[
    (OwnedPiece Rook White,     [c 9820] <> " "),
    (OwnedPiece Knight White,   [c 9822] <> " "),
    (OwnedPiece Bishop White,   [c 9821] <> " "),
    (OwnedPiece Queen White,    [c 9819] <> " "),
    (OwnedPiece King White,     [c 9818] <> " "),
    (OwnedPiece Pawn White,     [c 9823]),       -- jostain syystä tämä merkki on kaksi kertaa pitempi kuin nuo muut, ainakin fontissa SimSun-ExtB
    (OwnedPiece Rook Black,     [c 9814] <> " "),
    (OwnedPiece Knight Black,   [c 9816] <> " "),
    (OwnedPiece Bishop Black,   [c 9815] <> " "),
    (OwnedPiece Queen Black,    [c 9813] <> " "),
    (OwnedPiece King Black,     [c 9812] <> " "),
    (OwnedPiece Pawn Black,     [c 9817] <> " ")
    ]
    where 
        c = Char.chr

-- Tarkastaa onko Pos laudalla
onBoard :: Pos -> Bool
onBoard (Pos x y) =
    x `elem` [1..8] && y `elem` [1..8]

-- Tehty tälleen, koska yleiskäyttöisyys
getPieceFromBoard :: Board -> Pos -> Maybe OwnedPiece
getPieceFromBoard (Board board) pos =
    Map.lookup pos board

-- Palauttaa laudan sijaintia vastaavan merkin
getPieceChar :: Board -> Pos -> String
getPieceChar (Board board) pos = 
    case piece of
        Nothing -> "  "
        Just a -> merkit Map.! a
    where
        piece = getPieceFromBoard (Board board) pos

kerroin :: Player -> Int
kerroin Black = -1
kerroin White = 1

opp:: Player -> Player
opp Black = White 
opp White = Black

-- Piirtää laudan ja siihen tavittaessa linjat mitä pitkin valittu nappula voi liikkua.
drawBoard :: Board -> [Pos] -> Pos -> IO ()
drawBoard board liikelista pos =   
    putStr ("\n"
        <> "  8 |" <> rivi 8 <> "|\n"
        <> "  7 |" <> rivi 7 <> "|\n"
        <> "  6 |" <> rivi 6 <> "|\n"
        <> "  5 |" <> rivi 5 <> "|\n"
        <> "  4 |" <> rivi 4 <> "|\n"
        <> "  3 |" <> rivi 3 <> "|\n"
        <> "  2 |" <> rivi 2 <> "|\n"
        <> "  1 |" <> rivi 1 <> "|\n"
        <> "      A B C D E F G H \n"
        )
        where
            getPiece :: Int -> Int -> String
            getPiece x y = getPieceChar board (Pos x y)

            getBGnColor :: Int -> Int -> String
            getBGnColor x y
                | Pos x y `elem` liikelista = "\ESC[103m\ESC[31m"
                | Pos x y == pos = "\ESC[103m\ESC[32m"
                | (x + y) `mod` 2 == 1 = "\ESC[100m"
                | otherwise = "\ESC[0m"

            solu :: Int -> Int -> String
            solu x y = getBGnColor x y <> getPiece x y <> "\ESC[0m"

            rivi :: Int -> String
            rivi y =
                solu 1 y <> solu 2 y <> solu 3 y <> solu 4 y <>
                solu 5 y <> solu 6 y <> solu 7 y <> solu 8 y

-- Liikuttaa nappulaa, mikäli siirto on sallittu. Mikäli ei, palautetaan sama lauta mikä funktioon alun perin tulikin.
movePiece :: Pos -> Pos -> Board -> Board
movePiece pos1 pos2 (Board board) =
    case maybePiece of
        Nothing -> Board board
        Just a ->
            let
                board' = Map.delete pos1 board
                board'' = Map.insert pos2 a board'
                in
                    if onBoard pos2
                        then Board board''
                    else Board board
        where
            maybePiece = Map.lookup pos1 board

-- Tähän en passant -käsittely (Sitä ei ole, vaan se on jätetty pelaajan vastuulle.)
enPassant :: Game -> Pos -> Bool
enPassant _g _pos = False

-- TODO: erikoistarkistus
specialHandling :: OwnedPiece -> Pos -> Game -> [Pos] -> [Pos]
specialHandling (OwnedPiece play_rank play_player) (Pos play_x play_y) (Game b_curr p_curr h_curr) positionCandidates = 
    case play_rank of
        Pawn    -> filter handlePawn positionCandidates
        _       -> positionCandidates
        
    where
        handlePawn :: Pos -> Bool
        handlePawn (Pos cand_x cand_y) 
            | cand_x == play_x =
                case getPieceFromBoard b_curr (Pos play_x (play_y + (1 * kerr))) of
                    Just _a -> False
                    Nothing | cand_y == play_y + (1 * kerr) -> True
                            | cand_y == play_y + (2 * kerr) ->
                                case getPieceFromBoard b_curr (Pos play_x cand_y) of
                                    Just _a -> False
                                    Nothing | play_y == 2 && kerr == 1 -> True
                                            | play_y == 7 && kerr == -1 -> True
                                            | otherwise -> False
                            | otherwise -> False

            | (cand_x == (play_x - 1) || cand_x == (play_x + 1)) && cand_y == play_y + (1 * kerr) =
                case getPieceFromBoard b_curr (Pos cand_x cand_y) of
                    Just (OwnedPiece _rank2 player2)
                        | player2 == opp play_player -> True
                        | otherwise -> False
                    Nothing -> enPassant (Game b_curr p_curr h_curr) (Pos play_x play_y)
            | otherwise = False
        kerr = kerroin play_player

-- Päätytarkistus
endHandling :: OwnedPiece -> Pos -> Game -> [Pos] -> [Pos]
endHandling (OwnedPiece _rank player) (Pos _x _y) (Game b _p _h) positionCandidates = 
    filter properEnd positionCandidates
    where 
        properEnd :: Pos -> Bool
        properEnd pos =
            case getPieceFromBoard b pos of
                Nothing -> True
                Just (OwnedPiece _rank2 player2) 
                    | player == player2 -> False
                    | otherwise -> True

-- Polkutarkistus (rekursiolla)
pathHandling :: OwnedPiece -> Pos -> Game -> [Pos] -> [Pos]
pathHandling (OwnedPiece rank _player) (Pos x y) (Game b _p _h) positionCandidates =
    case rank of
        Queen   ->  pathing [] 1 1 x y ++ pathing [] (-1) 1 x y ++ pathing [] 1 (-1) x y ++ pathing [] (-1) (-1) x y ++
                    pathing [] 1 0 x y ++ pathing [] (-1) 0 x y ++ pathing [] 0   1  x y ++ pathing []   0  (-1) x y
        Rook    ->  pathing [] 1 0 x y ++ pathing [] (-1) 0 x y ++ pathing [] 0   1  x y ++ pathing []   0  (-1) x y
        Bishop  ->  pathing [] 1 1 x y ++ pathing [] (-1) 1 x y ++ pathing [] 1 (-1) x y ++ pathing [] (-1) (-1) x y
        _       ->  positionCandidates
    where
        pathing :: [Pos] -> Int -> Int -> Int -> Int -> [Pos]
        pathing lista vect_x vect_y alku_x alku_y =
            if onBoard (Pos (alku_x + vect_x) (alku_y + vect_y))
                then 
                    case getPieceFromBoard b (Pos (alku_x + vect_x) (alku_y + vect_y)) of
                    Nothing ->
                        pathing (Pos (alku_x + vect_x) (alku_y + vect_y) : lista) vect_x vect_y (alku_x + vect_x) (alku_y + vect_y)
                    Just _a -> Pos (alku_x + vect_x) (alku_y + vect_y) : lista
            else lista

-- Määritellään nappuloille sallitut siirrot (alustavasti, vaativammat ovat pelaajan vastuulla)
gimmeMoves :: Game -> Pos -> [Pos]
gimmeMoves (Game b p h) pos =
    case rank of 
        Pawn    -> spec (f [Pos x (y + 1 * kerr), Pos x (y + 2 * kerr),
                            Pos (x + 1) (y + 1* kerr), Pos (x - 1) (y + 1 * kerr)])
        Rook    -> end (f (path []))
        Knight  -> end (f ([Pos (x + i) (y + j) | i <- [-2, 2], j <- [-1, 1]] ++ [Pos (x + i) (y + j) | i <- [-1, 1], j <- [-2, 2]]))
        Bishop  -> end (f (path []))
        Queen   -> end (f (path []))
                
        King    -> spec (end (f [Pos (x + i) (y + j) | i <- [-1..1], j <- [-1..1]]))
    where
        (OwnedPiece rank player) = Maybe.fromJust (getPieceFromBoard b pos)
        (Pos x y) = pos
        kerr = kerroin player
        f = filter onBoard
        path = pathHandling (OwnedPiece rank player) pos g
        end = endHandling (OwnedPiece rank player) pos g
        spec = specialHandling (OwnedPiece rank player) pos g
        g = Game b p h

-- Ottaa yhden askeleen taaksepäin, eli hakee historiasta yhden laudan ja asettaa sen pelilaudaksi.
undo :: Game -> Game
undo (Game b p (History [])) = 
    Game b p (History [])
undo (Game _b p (History (eka : loput))) = 
    Game eka (opp p) (History loput)

-- Vapaan muokkauksen tila, jossa pelaaja voi vaihtaa laudan nappuloita miten tahtoo. Esim. en passant
-- ja tornitus, yms. (Luotetaan herrasmiessääntöihin)
edit :: Game -> IO ()
edit (Game b p (History h)) = do
    drawBoard b [] (Pos 0 0)
    putStrLn ("Turn: " <> show p)
    hFlush stdout
    putStr (
        "\nType 'remove'    to remove a piece." <>
        "\nType 'add'       to add a piece." <>
        "\nType 'move'      to move a piece." <>
        "\nType 'player     to switch player." <>
        "\nType 'back'      to go back." <>
        "\n\nEdit how?> "
        )
    hFlush stdout
    command <- getLine
    case command of
        "remove"    -> do
            putStr "Remove from where? (x y)> "
            hFlush stdout
            input <- getLine
            let
                inputAndNils = input ++ " 0 0"
                inputList = words inputAndNils
                pos = Pos (lue X (inputList !! 0)) (lue Y (inputList !! 1))
                bMap' = Map.delete pos bMap
            edit (Game (Board bMap') p (History (b:h)))
        "add"       -> do
            putStr "Add what piece? (Pawn (default), Rook, Knight, Bishop, Queen, King)> "
            hFlush stdout
            input1 <- getLine
            let
                rank =
                    case input1 of
                        "Rook"      -> Rook
                        "Knight"    -> Knight
                        "Bishop"    -> Bishop
                        "Queen"     -> Queen
                        "King"      -> King
                        _           -> Pawn
            putStr "Add piece to where? (x, y)> "
            hFlush stdout
            input2 <- getLine
            let
                inputAndNils = input2 ++ " 0 0"
                inputList = words inputAndNils
                pos = Pos (lue X (inputList !! 0)) (lue Y (inputList !! 1))
                bMap' = Map.insert pos (OwnedPiece rank p) bMap
            if onBoard pos
                then 
                    edit (Game (Board bMap') p (History (b:h)))
                else 
                    edit (Game b p (History h))

        "player"    -> do
            putStrLn "Switching player."
            hFlush stdout
            edit (Game b (opp p) (History h))
        "move"      -> do
            putStr "Move from where (x y)> "
            hFlush stdout
            posi1 <- getLine
            putStr "Move to where (x y)> "
            hFlush stdout
            posi2 <- getLine
            let
                posi1AndNils = posi1 ++ " 0 0"
                inputList1 = words posi1AndNils
                pos1 = Pos (lue X (inputList1 !! 0)) (lue Y (inputList1 !! 1))
                posi2AndNils = posi2 ++ " 0 0"
                inputList2 = words posi2AndNils
                pos2 = Pos (lue X (inputList2 !! 0)) (lue Y (inputList2 !! 1))
                b' = movePiece pos1 pos2 b
            edit (Game b' p (History (b:h)))
        "back"      -> loop (State (Game b p (History h)) Asking)
        _ -> edit (Game b p (History h))
        where
            (Board bMap) = b

-- Kääntää kirjaimet A-H numeroiksi 1-8
translation :: Map String Int
translation = Map.fromList[("A", 1),("B", 2),("C", 3),("D", 4),("E", 5),("F", 6),("G", 7),("H", 8)]

-- Muuntaa kirjaimen numeroksi jos tarkastellaan X-koordinaattia. Jos tarkastellaan Y-koordinaattia, palautetaan numero.
data Koord = X | Y
lue :: Koord -> String -> Int
lue X jono =
    case Map.lookup (map Char.toUpper jono) translation of
        Nothing -> 0
        Just a -> a
lue Y jono =
    case Read.readMaybe jono of
        Nothing -> 0
        Just a -> a

-- Pelirekursio, josta on kolme eri muotoa.
loop :: State -> IO ()

-- Asking, jossa kysytään mitä pelaaja haluaa tehdä
loop (State (Game b p h) Asking) = do
    drawBoard b [] (Pos 0 0)
    putStrLn (show p <> "'s turn.")
    putStr "What will you do?> "
    hFlush stdout
    command <- getLine
    case command of
        "quit" -> do
            putStrLn "Thank you for playing."
            hFlush stdout
            exitSuccess
        "forfeit" -> do
            putStrLn (show (opp p) <> " has won.")
            putStrLn "Thank you for playing."
            hFlush stdout
            exitSuccess
        "edit" -> do
            edit (Game b p h)
        "help" -> do
            help
        "undo" ->
            loop (State (undo (Game b p h)) Asking)
        "move" -> loop (State (Game b p h) MoveFrom)
        _ -> do
            putStrLn "\ESC[31mInvalid input.\ESC[0m"
    loop (State (Game b p h) Asking)

-- MoveFrom, jossa pelaajalta kysytään mitä nappulaa tämä tahtoo siirtää
loop (State (Game b p h) MoveFrom) = do
    putStr "Move which piece? (x y)> "
    hFlush stdout
    input <- getLine
    let
        inputAndNils = input ++ " 0 0"
        inputList = words inputAndNils
        pos1 = Pos (lue X (inputList !! 0)) (lue Y (inputList !! 1))
        pieceCandidate = getPieceFromBoard b pos1
    case pieceCandidate of
        Nothing -> do
            putStrLn "\ESC[31mInvalid input.\ESC[0m"
            hFlush stdout
            loop (State (Game b p h) Asking)
        Just (OwnedPiece _piece player) 
            | player == p -> loop (State (Game b p h) (MoveTo pos1))
            | otherwise -> do
                putStrLn "\ESC[31mInvalid input.\ESC[0m"
                hFlush stdout
                loop (State (Game b p h) Asking)

-- MoveTo, jossa pelaajalta kysytään mihin hän haluaa nappulansa siirtää
loop (State (Game b p h) (MoveTo pos1)) = do
    drawBoard b moves pos1
    putStr "Move piece where? (x y)> "
    hFlush stdout
    input <- getLine
    let
        inputAndNils = input ++ " 0 0"
        inputList = words inputAndNils
        pos2 = Pos (lue X (inputList !! 0)) (lue Y (inputList !! 1))
        b' = movePiece pos1 pos2 b
    if b == b'
        then do
            putStrLn "\ESC[31mInvalid input.\ESC[0m"
            hFlush stdout
    else if pos2 `elem` moves
            then do
                loop (State (Game b' (opp p) (History (b:hoo))) Asking)
        else do
            putStrLn "\ESC[4m\ESC[31mInvalid input.\ESC[0m"
            hFlush stdout
    loop (State (Game b p h) Asking)
        where
            (History hoo) = h
            moves = gimmeMoves (Game b p h) pos1

-- Printtaa komennot joita pelaaja voi käyttää.
help :: IO()
help =
    putStrLn ("\ESC[96m" 
        <>  "\n --------Chess--------"
        <>  "\nType 'move'       to move a piece."
        <>  "\nType 'undo'       to undo a move."
        <>  "\nType 'edit'       to edit board."
        <>  "\nType 'help'       to show this text."
        <>  "\nType 'forfeit'    to forfeit."
        <>  "\nType 'quit'       to quit." 
        <>  "\ESC[0m"
        )

-- Pelin kännistys
shakki :: IO ()
shakki = do
    help
    hFlush stdout
    let
        board = 
            Board (Map.fromList[
                (p 1 8, op roo b), (p 2 8, op kni b), (p 3 8, op bis b), (p 4 8, op que b),
                (p 5 8, op kin b), (p 6 8, op bis b), (p 7 8, op kni b), (p 8 8, op roo b),
                (p 1 7, op paw b), (p 2 7, op paw b), (p 3 7, op paw b), (p 4 7, op paw b),
                (p 5 7, op paw b), (p 6 7, op paw b), (p 7 7, op paw b), (p 8 7, op paw b),

                (p 1 1, op roo w), (p 2 1, op kni w), (p 3 1, op bis w), (p 4 1, op que w),
                (p 5 1, op kin w), (p 6 1, op bis w), (p 7 1, op kni w), (p 8 1, op roo w),
                (p 1 2, op paw w), (p 2 2, op paw w), (p 3 2, op paw w), (p 4 2, op paw w),
                (p 5 2, op paw w), (p 6 2, op paw w), (p 7 2, op paw w), (p 8 2, op paw w)
                ])

    loop (State (Game board White (History [])) Asking)
    where
        p = Pos
        op = OwnedPiece
        b = Black
        w = White
        paw = Pawn
        roo = Rook
        kni = Knight
        bis = Bishop
        que = Queen
        kin = King