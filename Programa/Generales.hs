module Generales where

import Data.List (find, delete)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import main (Usuario, usuarios(...)) -- Importar usuarios y el tipo Usuario desde main.hs

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
} deriving (Show, Eq)

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
    -- Verificar si el usuario existe
    let usuarioEncontrado = find (\u -> idCedula u == userIdInput) usuarios
    case usuarioEncontrado of
        Nothing -> putStrLn "Usuario no encontrado, intente de nuevo."
        Just usuario -> do
            putStrLn $ "Bienvenido, " ++ nombreUs usuario
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

-----------------------------------------------Consultar Reserva-----------------------------------------------
-- Función para consultar reserva por ID de usuario o por ID de reserva
consultarReserva :: IO ()
consultarReserva = do
    putStrLn "Consultar reserva"
    putStrLn "1. Consultar por ID de usuario"
    putStrLn "2. Consultar por ID de reserva"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese su ID de usuario:"
            hFlush stdout
            userIdInput <- getLine
            reservasUsuario userIdInput
        "2" -> do
            putStrLn "Ingrese el ID de la reserva:"
            hFlush stdout
            reservaIdInput <- getLine
            reservaPorId reservaIdInput
        _   -> putStrLn "Opción no válida."

-- Función para buscar y mostrar reservas por ID de usuario
reservasUsuario :: UserID -> IO ()
reservasUsuario userIdInput = do
    currentReservas <- readIORef reservas
    let reservasEncontradas = filter (\r -> userId r == userIdInput) currentReservas
    if null reservasEncontradas
        then putStrLn "No se encontraron reservas para este usuario."
        else mapM_ mostrarReserva reservasEncontradas

-- Función para buscar y mostrar una reserva por su ID
reservaPorId :: ReservationID -> IO ()
reservaPorId reservaIdInput = do
    currentReservas <- readIORef reservas
    let reservaEncontrada = find (\r -> reservaId r == reservaIdInput) currentReservas
    case reservaEncontrada of
        Nothing -> putStrLn "No se encontró ninguna reserva con ese ID."
        Just reserva -> mostrarReserva reserva

-- Función para mostrar los detalles de una reserva
mostrarReserva :: Reserva -> IO ()
mostrarReserva reserva = do
    putStrLn $ "ID de la reserva: " ++ reservaId reserva
    putStrLn $ "ID de usuario: " ++ userId reserva
    putStrLn $ "ID de sala: " ++ roomId reserva
    putStrLn $ "Fecha de la reserva: " ++ show (date reserva)
    putStrLn $ "Cantidad de personas: " ++ show (personas reserva)
-----------------------------------------------Cancelar Reserva-----------------------------------------------
-- Función para cancelar una reserva
cancelarReserva :: IO ()
cancelarReserva = do
    putStrLn "Ingrese el ID de la reserva que desea cancelar:"
    hFlush stdout
    reservaIdInput <- getLine

    currentReservas <- readIORef reservas
    -- Buscar la reserva por el ID
    let reservaEncontrada = find (\r -> reservaId r == reservaIdInput) currentReservas
    case reservaEncontrada of
        Nothing -> putStrLn "Reserva no encontrada, intente de nuevo."
        Just reserva -> do
            -- Eliminar la reserva encontrada
            let nuevasReservas = delete reserva currentReservas
            writeIORef reservas nuevasReservas
            putStrLn $ "Reserva con ID " ++ reservaIdInput ++ " cancelada exitosamente."

-----------------------------------------------Modificar Reserva-----------------------------------------------
-- Función para comprobar si la sala está disponible en la fecha solicitada
-- Ignora la reserva actual durante la modificación
salaDisponibleParaModificacion :: RoomID -> Date -> ReservationID -> IO Bool
salaDisponibleParaModificacion nuevoRoomId dateParsed reservaIdActual = do
    currentReservas <- readIORef reservas
    -- Verifica si ya existe una reserva con el mismo roomId y fecha, pero diferente reservaId
    let reservaExistente = find (\r -> roomId r == nuevoRoomId && date r == dateParsed && reservaId r /= reservaIdActual) currentReservas
    return $ case reservaExistente of
        Nothing -> True  -- La sala está disponible
        Just _  -> False -- La sala ya está reservada


-- Función para modificar una reserva existente
modificarReserva :: IO ()
modificarReserva = do
    putStrLn "Ingrese el ID de la reserva que desea modificar:"
    hFlush stdout
    reservaIdInput <- getLine

    currentReservas <- readIORef reservas
    -- Buscar la reserva por el ID
    let reservaEncontrada = find (\r -> reservaId r == reservaIdInput) currentReservas
    case reservaEncontrada of
        Nothing -> putStrLn "Reserva no encontrada, intente de nuevo."
        Just reserva -> do
            putStrLn "Seleccione el campo a modificar:"
            putStrLn "1. Modificar ID de Sala"
            putStrLn "2. Modificar Fecha"
            putStrLn "3. Modificar Cantidad de Personas"
            putStrLn "4. Cancelar modificación"
            opcion <- getLine
            case opcion of
                "1" -> do
                    putStrLn "Ingrese el nuevo ID de sala:"
                    hFlush stdout
                    nuevoRoomId <- getLine
                    -- Verificar si la nueva sala existe
                    let salaEncontrada = find (\s -> salaId s == nuevoRoomId) salasDisponibles
                    case salaEncontrada of
                        Nothing -> putStrLn "Sala no encontrada, intente de nuevo."
                        Just sala -> do
                            -- Verificar si la nueva sala está disponible en la misma fecha, ignorando la reserva actual
                            disponible <- salaDisponibleParaModificacion nuevoRoomId (date reserva) (reservaId reserva)
                            if not disponible
                                then putStrLn "La nueva sala ya está reservada para la fecha seleccionada."
                                else do
                                    let reservaModificada = reserva { roomId = nuevoRoomId }
                                    actualizarReserva reserva reservaModificada
                                    putStrLn "Reserva modificada exitosamente (ID de sala)."
                "2" -> do
                    putStrLn "Ingrese la nueva fecha (YYYY-MM-DD):"
                    hFlush stdout
                    nuevaFechaInput <- getLine
                    -- Validar la nueva fecha
                    case parseTimeM True defaultTimeLocale "%Y-%m-%d" nuevaFechaInput of
                        Nothing -> putStrLn "Fecha no válida, intente de nuevo."
                        Just nuevaFecha -> do
                            -- Verificar si la sala está disponible en la nueva fecha
                            disponible <- salaDisponible (roomId reserva) nuevaFecha
                            if not disponible
                                then putStrLn "La sala ya está reservada para la nueva fecha."
                                else do
                                    let reservaModificada = reserva { date = nuevaFecha }
                                    actualizarReserva reserva reservaModificada
                                    putStrLn "Reserva modificada exitosamente (Fecha)."
                
                "3" -> do
                    putStrLn "Ingrese la nueva cantidad de personas:"
                    hFlush stdout
                    nuevaCantidadInput <- getLine
                    let nuevaCantidad = read nuevaCantidadInput :: Int
                    -- Verificar capacidad de la sala
                    let sala = find (\s -> salaId s == roomId reserva) salasDisponibles
                    case sala of
                        Nothing -> putStrLn "Error interno: Sala no encontrada."
                        Just s -> if nuevaCantidad > capacidadSala s
                            then putStrLn "La sala no tiene capacidad suficiente."
                            else do
                                let reservaModificada = reserva { personas = nuevaCantidad }
                                actualizarReserva reserva reservaModificada
                                putStrLn "Reserva modificada exitosamente (Cantidad de personas)."
                
                "4" -> putStrLn "Modificación cancelada."
                
                _   -> putStrLn "Opción no válida, intente de nuevo."

-- Función para actualizar la lista de reservas
actualizarReserva :: Reserva -> Reserva -> IO ()
actualizarReserva viejaReserva nuevaReserva = do
    currentReservas <- readIORef reservas
    let nuevasReservas = nuevaReserva : filter (\r -> reservaId r /= reservaId viejaReserva) currentReservas
    writeIORef reservas nuevasReservas

-----------------------------------------------Consulta de disponibilidad de salas-----------------------------------------------
-- Función para la consulta de disponibilidad de salas
consultaDisponibilidadSala :: IO ()
consultaDisponibilidadSala = do
    putStrLn "----- Consulta de Disponibilidad de Sala -----"
    putStrLn "1. Consultar por una fecha específica"
    putStrLn "2. Consultar por un rango de fechas"
    putStrLn "Seleccione una opción:"
    opcion <- getLine
    case opcion of
        "1" -> consultaPorFechaEspecifica
        "2" -> consultaPorRangoDeFechas
        _   -> do
            putStrLn "Opción no válida, por favor seleccione nuevamente."
            consultaDisponibilidadSala

-- Función para consultar la disponibilidad por una fecha específica
consultaPorFechaEspecifica :: IO ()
consultaPorFechaEspecifica = do
    putStrLn "Ingrese la fecha (YYYY-MM-DD):"
    dateInput <- getLine
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateInput of
        Nothing -> putStrLn "Fecha no válida, intente de nuevo."
        Just dateParsed -> do
            -- Obtener las reservas existentes en la fecha
            currentReservas <- readIORef reservas
            let salasReservadas = [roomId r | r <- currentReservas, date r == dateParsed]
            -- Mostrar salas disponibles
            let salasLibres = filter (\s -> salaId s `notElem` salasReservadas) salasDisponibles
            if null salasLibres
                then putStrLn "No hay salas disponibles para esa fecha."
                else do
                    putStrLn "Salas disponibles:"
                    mapM_ (putStrLn . nombreSala) salasLibres

-- Función para consultar la disponibilidad por un rango de fechas
consultaPorRangoDeFechas :: IO ()
consultaPorRangoDeFechas = do
    putStrLn "Ingrese la fecha de inicio (YYYY-MM-DD):"
    inicioInput <- getLine
    putStrLn "Ingrese la fecha de fin (YYYY-MM-DD):"
    finInput <- getLine
    case (parseTimeM True defaultTimeLocale "%Y-%m-%d" inicioInput, parseTimeM True defaultTimeLocale "%Y-%m-%d" finInput) of
        (Just inicioParsed, Just finParsed) -> do
            -- Iterar sobre el rango de fechas y mostrar la disponibilidad por día
            let fechas = [inicioParsed..finParsed]
            mapM_ (\fecha -> mostrarDisponibilidadPorDia fecha) fechas
        _ -> putStrLn "Fechas no válidas, intente de nuevo."

-- Función auxiliar para mostrar la disponibilidad de un día específico
mostrarDisponibilidadPorDia :: Date -> IO ()
mostrarDisponibilidadPorDia dateParsed = do
    putStrLn $ "\nDisponibilidad para la fecha: " ++ show dateParsed
    currentReservas <- readIORef reservas
    let salasReservadas = [roomId r | r <- currentReservas, date r == dateParsed]
    let salasLibres = filter (\s -> salaId s `notElem` salasReservadas) salasDisponibles
    if null salasLibres
        then putStrLn "No hay salas disponibles."
        else do
            putStrLn "Salas disponibles:"
            mapM_ (putStrLn . nombreSala) salasLibres