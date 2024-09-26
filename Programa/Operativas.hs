module Operativas (
    cargarMobiliario,
    mostrarMobiliario,
    Mobiliario
) where

import System.IO

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


