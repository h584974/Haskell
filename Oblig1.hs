-- Oliver O'Loughlin
module Oblig1 where

dictionary = [
        ("bb",["Big Brother"]),
        ("dep",["department"]),
        ("sec", ["Sector"]),
        ("doubleplusgood",["excellent", "fabulous", "fantastic", "best"]),
        ("doubleplusungood", ["terrible", "horrible", "worst"]),
        ("Ingsoc", ["English Socialism"]),
        ("joycamp", ["labour camp"]),
        ("Oldspeak", ["Standard English", "English"]),
        ("oldthink", ["objectivity", "rationalism", "democracy"]),
        ("thinkpol", ["The Thought Police"]),
        ("prolefeed", ["Popular culture", "pop-culture"]),
        ("crimethink", ["liberty", "equality", "privacy", "thoughtcrime"]),
        ("fullwise", ["fully", "completely", "totally"]),
        ("goodthink", ["political orthodoxy", "politically orthodox thought", "orthodox thought"]),
        ("goodwise", ["well"]),
        ("ownlife", ["anti-social tendency", "solitude", "individualism"]),
        ("plusgood", ["very good", "great"]),
        ("plusungood", ["very bad"]),
        ("misprint", ["error", "misprediction"]),
        ("Miniluv", ["The Ministry of Love"]),
        ("Minipax", ["The Ministry of Peace"]),
        ("Minitrue", ["The Ministry of Truth"]),
        ("Miniplenty", ["The Ministry of Plenty"]),
        ("bellyfeel", ["blind, enthusiastic acceptance"]),
        ("doublethink", ["believing two contradictory ideas"]),
        ("duckspeak", ["vocal support of political orthodoxies"]),
        ("un", ["not"]),
        ("peace", ["war"]),
        ("strength", ["ignorance"]),
        -- The next line contains a list of forbidden words that don't have a translation to Newspeak, these should be replaced with '*'s
        ("",["freedom", "revolution", "fun", "diary", "surveillance", "Great Britain", "Winston Smith", "Julia"])
        ]


-- Oppgave 1 ----------------------------------------------------

-- Sammenligner karakterer fra første og andre String, om første går tom
-- før andre og ingen karakterer er ulike er det prefix til den andre
isPrefix :: String -> String -> Bool 
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys


-- Oppgave 2 ----------------------------------------------------

-- Sjekker for tilfeller av tomme strenger og ellers bruker hjelpefunksjon locateA
locate :: String -> String -> [(Int,Int)]
locate s1 s2 = locateR s1 s2 0

locateR :: String -> String -> Int -> [(Int,Int)]
locateR [] _ _ = []
locateR _ [] _ = []
locateR (x:xs) (y:ys) n = if isPrefix (x:xs) (y:ys) then r ++ [(n,n + length (x:xs))] else r where r = locateR (x:xs) ys (n+1)

-- Oppgave 3 ----------------------------------------------------
translate :: String -> String 
translate [] = []
translate word = translateR word 0

translateR word n
        | n >= length dictionary = []
        | exists word (snd (dictionary !! n)) = fst (dictionary !! n)
        | otherwise = translateR word (n+1)

exists :: String -> [String] -> Bool
exists word [] = False
exists word (x:xs) = if word == x then True else exists word xs


-- Oppgave 4 ----------------------------------------------------

-- bruker hjelpefunksjon rep
replace :: [(Int,Int)] -> String -> String 
replace [] s = s
replace (x:xs) [] = []
replace (x:xs) s = rep (-1,-1) (x:xs) s ""

-- Tar for hvert steg å legger til oversettelse og gjenværende i ns, 
-- bruker original String s for å hente ut ordene som skal oversettes
rep :: (Int,Int) -> [(Int,Int)] -> String -> String -> String
rep (a,b) [] s ns = ns ++ takeString (b,length s) s
rep (-1,-1) (x:xs) s ns = rep x xs s (takeString (0,fst x) s ++ validate (takeString x s))
rep (a,b) (x:xs) s ns = rep x xs s (ns ++ takeString (b,fst x) s ++ validate (takeString x s))

-- Oversetter et ord og skriver stjerner hvis det har ingen oversettelse
validate s = if null (translate s) then writeStar (length s) else translate s
writeStar :: Int -> String
writeStar n = take n (repeat '*')

-- Henter ut String fra gitte indekser
takeString (x,y) s = drop x (take y s)


-- Oppgave 5 ----------------------------------------------------

-- Samler resultat og erstatter
toNewspeak :: String -> String 
toNewspeak s = replace (collect1 s dictionary []) s

-- samler resultat fra ytre liste i dictionary
collect1 s [] l = l
collect1 s (x:xs) l = collect1 s xs ((collect2 s (snd x) [])++l)

-- Samler resultat fra indre liste i dictionary
collect2 s [] l = l
collect2 s (x:xs) l = collect2 s xs ((locate x s)++l)


-- Oppgave 6 ----------------------------------------------------
analytics :: String -> String -> Int 
analytics s1 s2 = round (100 * (fromIntegral (charsDiff (countChars s1 charlist []) (countChars s2 charlist []) 0)) / (fromIntegral (length s1)))

charsDiff :: [Int] -> [Int] -> Int -> Int
charsDiff [] [] t = t
charsDiff (x:xs) (y:ys) t = charsDiff xs ys (t+abs (x-y))

countChars :: String -> [Char] -> [Int] -> [Int]
countChars s [] r = r
countChars s (x:xs) r = countChars s xs (r ++ [countChar s x 0]) 

countChar :: String -> Char -> Int -> Int
countChar [] c n = n
countChar (x:xs) c n = if c == x 
        then countChar xs c (n+1)
        else countChar xs c n 

charlist = (['0'..'9']++['a'..'z']++['A'..'Z'])


-- Oppgave 7 ----------------------------------------------------

-- Oversetter originale String og bruker analytics på original og oversatt String
main :: String -> (String, Int)
main s = (toNewspeak s,analytics s (toNewspeak s))