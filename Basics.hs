{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}



module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe
import Data.Bifunctor

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Gateway = Gateway {
    pos :: Position,
    pair :: Position
}

instance Eq Gateway where
    Gateway pos1 pair1 == Gateway pos2 pair2 = pos1 == pos2 && pair1 == pair2

instance Ord Gateway where
    Gateway p1 _ <= Gateway p2 _ = p1 <= p2

data Game = Game {
    l :: Int,
    c :: Int,
    hunter :: Position,
    targets :: [Target],
    obstacles :: [Position],
    gateways :: [Gateway]
} deriving Ord

instance  Eq Game where
    Game l1 c1 h1 t1 o1 g1 == Game l2 c2 h2 t2 o2 g2
        | l1 /= l2 = False
        | c1 /= c2 = False
        | h1 /= h2 = False
        | t1 /= t2 = False
        | g1 /= g2 = False
        | o1 /= o2 = False
        | otherwise  = True



{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString game = Data.List.intercalate "" lst
    where
        lst = [[getEl (i, j) | j <- [0 .. c game]] | i <- [0 .. (l game - 2)]] ++ [replicate (c game) '@']
        getEl p
            | p `elem` obstacles game = '@'
            | p == hunter game = '!'
            | p `elem` map position (targets game) = '*'
            | p `elem` map pos (gateways game) = '#'
            | p `elem` map pair (gateways game) = '#'

            | snd p == c game && fst p /= l game - 1= '\n'
            | snd p == c game - 1 || fst p == 0 || fst p == l game - 1 && snd p /= c game || snd p == 0 = '@'
            | otherwise = ' '

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame n m = Game n m (1, 1) [] [] []

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter (x, y) (Game lin col ht t o g)
    | x < 1 || y < 1 || x >= lin - 1 || y >= col - 1 || (x,y) `elem` o = Game lin col ht t o g
    | otherwise = Game lin col (x, y) t o g

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget b p (Game lin col ht t o g)
    | fst p < 1 || snd p < 1 || fst p >= lin - 1 || snd p >= col - 1 = Game lin col ht t o g
    | otherwise  =  Game lin col ht (t ++ [Target p b]) o g

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (p1,p2) (Game lin col ht t o g)
    | (fst p1 < 1 || snd p1 < 1 || fst p1 >= lin - 1 || snd p1 >= col - 1)
        &&  fst p2 >= 1 &&  snd p2 >= 1 && fst p2 < lin - 1 && snd p2 < col - 1 =
                                            Game lin col ht t o (g ++ [Gateway p2 p2])
    | (fst p2 < 1 || snd p2 < 1 || fst p2 >= lin - 1 || snd p2 >= col - 1)
    &&  fst p1 >= 1 &&  snd p1 >= 1 && fst p1 < lin - 1 && snd p1 < col - 1 =
                                        Game lin col ht t o (g ++ [Gateway p1 p1])
    | fst p1 >= 1 &&  snd p1 >= 1 && fst p1 < lin - 1 && snd p1 < col - 1
    &&  fst p2 >= 1 &&  snd p2 >= 1 && fst p2 < lin - 1 && snd p2 < col - 1 =
                                        Game lin col ht t o (g ++ [Gateway p1 p2])
    | otherwise = Game lin col ht t o g

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle ps (Game lin col ht t o g)  = Game lin col ht t (o ++ [ps]) g

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove (x, y) (Game lin col _ _ o g)
    | (x, y) `elem` o = Nothing
    | gate /= (0, 0) = Just gate
    | x >= 1 &&  y >= 1 && x < lin - 1 && y < col - 1 = Just (x, y)
    | otherwise  = Nothing
    where
        gate = foldl (\acc gt -> if pos gt == (x, y) then pair gt else if pair gt == (x, y) then pos gt else acc) (0, 0) g

{-
    *** TODO ***
    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
go :: Position->Behavior->Behavior
go (px, py) b (x, y) (Game lin col ht t o g)
    | (x, y) `elem` o = Target (x, y) b
    | px >= 1 &&  py >= 1 && px < lin - 1 && py < col - 1 = Target (nx, ny) b
    | otherwise = Target (getPos (x, y)) b
    where
        (nx, ny)
            | (tnx, tny) == (0, 0) = Data.Maybe.fromMaybe (0, 0) (attemptMove (x, y) (Game lin col ht t o g))
            | otherwise = (tnx, tny)
        (tnx, tny) = Data.Maybe.fromMaybe (0, 0) (attemptMove (px, py) (Game lin col ht t o g))
        getPos (xn, yn) = Data.Maybe.fromMaybe (0, 0) (attemptMove (xn, yn) (Game lin col ht t o g))

goEast :: Behavior
goEast (x, y) (Game lin col ht t o g) = go(x, y + 1) goEast (x, y) (Game lin col ht t o g)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest (x, y) (Game lin col ht t o g) = go(x, y - 1) goWest (x, y) (Game lin col ht t o g)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth (x, y) (Game lin col ht t o g) = go(x - 1, y) goNorth (x, y) (Game lin col ht t o g)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth (x, y) (Game lin col ht t o g) = go(x + 1, y) goSouth (x, y) (Game lin col ht t o g)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}


bounce :: Int -> Behavior
bounce dir (x, y) (Game lin col ht t o g)
    | nx == 0 && ny == 0 && dir == 1 = Target (pxn, pyn) (bounce (-1))
    | nx == 0 && ny == 0 && dir == -1 = Target (pxs, pys) (bounce 1)
    |otherwise = Target (nx, ny) (bounce dir)
    where
        (nx, ny)
            | dir == 1 = Data.Maybe.fromMaybe (0, 0) (attemptMove (x + 1, y) (Game lin col ht t o g))
            | otherwise = Data.Maybe.fromMaybe (0, 0) (attemptMove (x - 1, y) (Game lin col ht t o g))
        (pxn, pyn)
            | (npxn, npyn) /= (0,0) = (npxn, npyn)
            | otherwise  = Data.Maybe.fromMaybe (0, 0) (attemptMove (x, y) (Game lin col ht t o g))

        (npxn, npyn) =  Data.Maybe.fromMaybe (0, 0) (attemptMove (x - 1, y) (Game lin col ht t o g))
        (pxs, pys)
            | (npxs, npys) /= (0,0) = (npxs, npys)
            | otherwise  = Data.Maybe.fromMaybe (0, 0) (attemptMove (x, y) (Game lin col ht t o g))

        (npxs, npys) =  Data.Maybe.fromMaybe (0, 0) (attemptMove (x + 1, y) (Game lin col ht t o g))


{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets (Game lin col ht t o g) =
    Game lin col ht (map (\x -> behavior x (position x) (Game lin col ht t o g)) t) o g

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (xh, yh) (Target (xt, yt) _)
    | xt == xh - 1 && yt == yh = True
    | xt == xh + 1 && yt == yh = True
    | xt == xh && yt == yh + 1 = True
    | xt == xh && yt == yh - 1 = True
    | otherwise  = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
advanceGameHelper ::  Bool -> Game -> Game
advanceGameHelper bl (Game lin col ht t o g)
    | bl  = filterTargets $ moveTargets $ Game lin col ht (filter  (not . isTargetKilled ht) t) o g
    | otherwise =  Game lin col ht t o g
    where
        filterTargets (Game lin1 col1 ht1 t1 o1 g1) = Game lin1 col1 ht1 (filter  (not . isTargetKilled ht1) t1) o1 g1

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir bl (Game lin col (x, y) t o g)
    | px >= 1 &&  py >= 1 && px < lin - 1 && py < col - 1 && (px, py) `notElem` o = advanceGameHelper  bl (Game lin col (nx, ny) t o g)
    | otherwise = Game lin col (x, y) t o g
    where
        (px, py) = getDir dir
        getDir North = (x - 1, y)
        getDir South = (x + 1, y)
        getDir West = (x, y - 1)
        getDir East = (x, y + 1)
        (nx, ny)
            | (npx, npy) /= (0,0) = (npx, npy)
            | otherwise  = Data.Maybe.fromMaybe (0, 0) (attemptMove (x, y) (Game lin col (x,y) t o g))
        (npx, npy) = Data.Maybe.fromMaybe (0, 0) (attemptMove (px, py) (Game lin col (x,y) t o g))

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (Game _ _ _ t _ _) = not (null t)

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circleHelper :: Direction -> Position -> Int -> Behavior
circleHelper dir  center radius (x, y) Game {} =  Target (x + px, y + py) (circleHelper newdir center radius)
    where
        newdir 
            | dir == East && y - snd center == radius = South
            | dir == South && x - fst center == radius = West
            | dir == North && fst center - x == radius = East
            | dir == West && snd center - y == radius = North
            | otherwise  = dir
        (px, py) 
            | newdir == East = (0, 1) 
            | newdir == South && newdir /= dir = (1, 1)
            | newdir == South = (1, 0)
            | newdir == West = (0, -1)
            | newdir == North && newdir /= dir = (-1, -1)
            | newdir == North = (-1, 0)
            | otherwise = (0, 0)


circle :: Position -> Int -> Behavior
circle  = circleHelper East

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors g = [(dir, advanceGameState dir False g) | dir <- [North, South, East, West]]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal (Game _ _  (x, y) t _ _ )
        | (x + 1, y) `elem` map position t = True
        | (x - 1, y) `elem` map position t = True
        | (x, y + 1) `elem` map position t = True
        | (x, y - 1) `elem` map position t = True
        | otherwise = False

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h (Game _ _ ht t _ _) = foldl (\acc (Target p _) -> if hEuclidean ht p < acc then hEuclidean ht p else acc) 999999 t

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ (x1 - x2) ^ pow + (y1 - y2) ^ pow
  where
    pow = 2 :: Int

hManhattan :: Position -> Position -> Float
hManhattan (x1, y1) (x2, y2) = sqrt $ fromIntegral $ dx + dy
  where
    dx = abs (x1 - x2)
    dy = abs (y1 - y2)

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)
 
{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors (BonusGame g)  = map (Data.Bifunctor.second BonusGame) (successors g)

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal (BonusGame (Game _  _ (x, y) t _ _ ))
        | (x + 1, y) `elem` map position t = True
        | (x - 1, y) `elem` map position t = True
        | (x, y + 1) `elem` map position t = True
        | (x, y - 1) `elem` map position t = True
        | otherwise = False

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h (BonusGame (Game _ _ ht t _ _)) = foldl (\acc (Target p _) -> if hManhattan ht p < acc then hManhattan ht p else acc) 999999 t