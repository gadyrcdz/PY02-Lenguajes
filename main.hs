import System.IO
import System.Exit

import Control.Monad


main = forever (printMenu >> readChoice >>= menuAction)

printMenu = putStr "\n1)Opciones Operativas\n2)Opciones Generales\n3)Salir\nChoose One: " >> hFlush stdout

readChoice = hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> getChar

menuAction '1' = putStrLn "\nOpciones Operativas!"
menuAction '2' = putStrLn "\nOpciones Generales!"
menuAction '3' = exitSuccess
menuAction _ = hPutStrLn stderr "\nInvalid choice."