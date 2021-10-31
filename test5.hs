import System.IO
import System.Directory

main = do putStrLn "Skriv filnavn"
          filnavn <- getLine
          exist <- doesFileExist filnavn
          if exist 
          then
            do fil <- openFile filnavn ReadMode
               tekst <- hGetContents fil
               let (x:xs) = lines tekst
                   nytekst = unlines (xs++[x])
               hClose fil
               (filnavn2, f2) <- openTempFile "." " temp"
               hPutStr f2 nytekst
               hClose f2
               removeFile filnavn
               renameFile filnavn2 filnavn
          else 
            do putStrLn "Fil eksisterer ikke"

