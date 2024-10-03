module Operativas (
    cargarMobiliario,
    mostrarMobiliario,
    Mobiliario,
    crearSala,
    mostrarSala,
    crearSalas,
    SalaO
) where

import Data.List (find)
import System.IO
import Control.Monad.RWS.Class (MonadState(put))

-- Definir un tipo de dato para Mobiliario
data Mobiliario = Mobiliario {
    codigo      :: String,
    nombre      :: String,
    descripcion :: String,
    tipo        :: String
} deriving (Show)

-- Función para cargar y procesar el archivo CSV
cargarMobiliario :: FilePath -> IO [Mobiliario]
cargarMobiliario filePath = do
    contenido <- readFile filePath
    let lineas = lines contenido
    let items = map procesarLinea lineas
    return items

-- Función para procesar cada línea del CSV y convertirla en un Mobiliario
procesarLinea :: String -> Mobiliario
procesarLinea linea =
    let campos = splitByComma linea
    in case campos of
        (codigo:nombre:descripcion:tipo:_) -> Mobiliario codigo nombre descripcion tipo
        _ -> error $ "Línea inválida: " ++ linea  -- Muestra un error en casp de que algo en el archivo falle


-- Función auxiliar para dividir una línea por comas
splitByComma :: String -> [String]
splitByComma [] = [""]
splitByComma (c:cs)
    | c == ','  = "" : rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = splitByComma cs

-- Función para mostrar la lista de mobiliario
mostrarMobiliario :: [Mobiliario] -> IO ()
mostrarMobiliario [] = putStrLn "Esos son todos los mobiliarios."
mostrarMobiliario (m:ms) = do
    putStrLn $ "Código: " ++ codigo m ++ ", Nombre: " ++ nombre m
    putStrLn $ "Descripción: " ++ descripcion m ++ ", Tipo: " ++ tipo m
    putStrLn "-------------------------------"
    mostrarMobiliario ms


-- ------------------------------FUNCIONES DE Carga y mostrar salas de reuniones----------------------------------------------------------------------------------------------------------
-- Definir un tipo de dato para Sala
data SalaO = Sala {
    codigoSala :: String,
    nombreSala :: String,
    edificioSala :: String,
    pisoSala :: Int,
    ubicacionSala :: String,
    capacidadSala :: Int,
    mobiliarioSala :: [Mobiliario]  -- Lista de mobiliario asociado a la sala
} deriving (Show)

-- Función para generar un código de sala único
generarCodigoSala :: Int -> String
generarCodigoSala n = "SALA" ++ show n

-- Función para crear una sala, seleccionando mobiliario existente
crearSala :: Int -> [Mobiliario] -> IO SalaO
crearSala n mobiliarioExistente = do
    putStrLn "Ingrese el nombre de la sala:"
    nombre <- getLine
    putStrLn "Ingrese el edificio de la sala:"
    edificio <- getLine
    putStrLn "Ingrese el piso de la sala:"
    piso <- readLn
    putStrLn "Ingrese la ubicación de la sala:"
    ubicacion <- getLine
    putStrLn "Ingrese la capacidad de la sala:"
    capacidad <- readLn

    -- Mostrar mobiliario existente y permitir seleccionar los que posee la sala
    putStrLn "Mobiliario disponible en el sistema:"
    mostrarMobiliario mobiliarioExistente
    putStrLn "Seleccione los números de los mobiliarios que posee la sala (separados por comas):"
    seleccion <- getLine

    -- Procesar la selección de mobiliario
    let indices = map (read :: String -> Int) (wordsWhen (== ',') seleccion)
        mobiliarioSala = [mobiliarioExistente !! (i - 1) | i <- indices, i > 0, i <= length mobiliarioExistente]

    -- Generar la sala con su código único
    let codigo = generarCodigoSala n
    let sala = Sala codigo nombre edificio piso ubicacion capacidad mobiliarioSala

    putStrLn $ "Sala creada con el código: " ++ codigo
    return sala

-- Función para permitir la creación de múltiples salas
crearSalas :: Int -> [Mobiliario] -> [SalaO] -> IO [SalaO]
crearSalas id mobiliarioExistente salas = do
    nuevaSala <- crearSala id mobiliarioExistente
    let salasActualizadas = salas ++ [nuevaSala]
    putStrLn "¿Desea crear otra sala? (s/n):"
    respuesta <- getLine
    if respuesta == "s"
        then crearSalas (id + 1) mobiliarioExistente salasActualizadas  -- Llamada recursiva para crear más salas
        
        else return salasActualizadas  -- Terminar si no se desea crear más salas


-- Función auxiliar para dividir una cadena por un carácter específico
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'





mostrarMobiliarioSala :: [Mobiliario] -> IO ()
mostrarMobiliarioSala [] = putStrLn "Eso es todo"
mostrarMobiliarioSala (m:ms) = do
    putStrLn "Entre"
    putStrLn $ "- " ++ nombre m ++ " (" ++ descripcion m ++ ")"
    putStrLn "siguiente"
    mostrarMobiliarioSala ms



-- Función para mostrar la información de una sala dado su código
mostrarSala :: String -> [SalaO] -> IO ()
mostrarSala codigo salas = case find (\s -> codigoSala s == codigo) salas of
    Nothing -> putStrLn "No se encontró una sala con ese código."
    Just sala -> do
        putStrLn $ "Código de Sala: " ++ codigoSala sala
        putStrLn $ "Nombre: " ++ nombreSala sala
        putStrLn $ "Edificio: " ++ edificioSala sala
        putStrLn $ "Piso: " ++ show (pisoSala sala)
        putStrLn $ "Ubicación: " ++ ubicacionSala sala
        putStrLn $ "Capacidad: " ++ show (capacidadSala sala)
        putStrLn "Mobiliario en la sala:"
        mostrarMobiliarioSala (mobiliarioSala sala)

