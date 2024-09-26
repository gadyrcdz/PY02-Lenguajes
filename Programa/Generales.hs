module Generales where

import Data.List (find)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Tipos de datos para representar usuarios, salas y reservas
type UserID = String
type RoomID = String
type ReservationID = String
type Date = Day

data Reserva = Reserva {
    reservaId :: ReservationID,
    userId :: UserID,
    roomId :: RoomID,
    date :: Date,
    personas :: Int
} deriving (Show)

data Sala = Sala {
    salaId :: RoomID,
    nombreSala :: String,
    capacidadSala :: Int
} deriving (Show)

-- Lista simulada de salas disponibles
salasDisponibles :: [Sala]
salasDisponibles = [
    Sala "S001" "Sala 1" 10,
    Sala "S002" "Sala 2" 20,
    Sala "S003" "Sala 3" 15
    ]

-- Lista simulada de reservas
reservas :: IORef [Reserva]
reservas = unsafePerformIO $ newIORef []

-- Función para generar un ID único de reserva
generarIdReserva :: IO ReservationID
generarIdReserva = do
    currentReservas <- readIORef reservas
    return $ "R" ++ show (length currentReservas + 1)

-- Función para comprobar si la sala está disponible en la fecha solicitada
salaDisponible :: RoomID -> Date -> IO Bool
salaDisponible roomIdInput fechaInput = do
    currentReservas <- readIORef reservas
    -- Verifica si ya existe una reserva con el mismo roomId y fecha
    let reservaExistente = find (\r -> roomId r == roomIdInput && date r == fechaInput) currentReservas
    return $ case reservaExistente of
        Nothing -> True  -- La sala está disponible
        Just _  -> False -- La sala ya está reservada

-- Función para la Gestión de Reserva
gestionarReserva :: IO ()
gestionarReserva = do
    putStrLn "Ingrese su ID de usuario:"
    hFlush stdout
    userIdInput <- getLine
    putStrLn "Ingrese el ID de la sala:"
    hFlush stdout
    roomIdInput <- getLine
    putStrLn "Ingrese la fecha (YYYY-MM-DD):"
    hFlush stdout
    dateInput <- getLine
    putStrLn "Ingrese la cantidad de personas:"
    hFlush stdout
    personasInput <- getLine

    -- Verificar si la sala existe
    let salaEncontrada = find (\s -> salaId s == roomIdInput) salasDisponibles
    case salaEncontrada of
        Nothing -> putStrLn "Sala no encontrada, intente de nuevo."
        Just sala -> do
            let personasNum = read personasInput :: Int
            -- Validar capacidad de la sala
            if personasNum > capacidadSala sala
                then putStrLn "La sala no tiene capacidad suficiente."
                else do
                    -- Validar y parsear la fecha
                    case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateInput of
                        Nothing -> putStrLn "Fecha no válida, intente de nuevo."
                        Just fechaParsed -> do
                            -- Verificar si la sala está disponible en la fecha solicitada
                            disponible <- salaDisponible roomIdInput fechaParsed
                            if not disponible
                                then putStrLn "La sala ya está reservada para esa fecha."
                                else do
                                    -- Generar ID de reserva
                                    newId <- generarIdReserva
                                    -- Crear y añadir la nueva reserva
                                    let nuevaReserva = Reserva newId userIdInput roomIdInput fechaParsed personasNum
                                    modifyIORef reservas (nuevaReserva :)
                                    putStrLn $ "Reserva creada exitosamente con ID: " ++ reservaId nuevaReserva
