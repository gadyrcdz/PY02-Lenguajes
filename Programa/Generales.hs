module Generales where

import Data.List (find, delete)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Usuarios 
import Operativas (SalaO(..), codigoSala, nombreSala, capacidadSala)

-- Tipos de datos para representar usuarios y reservas
type UserID = String
type ReservationID = String
type Date = Day

data Reserva = Reserva {
    reservaId :: ReservationID,
    userId :: UserID,
    sala :: SalaO,
    date :: Date,
    personas :: Int
} deriving (Show, Eq)

-- Lista simulada de salas disponibles (ahora usando SalaO)
salasDisponibles :: [SalaO]
salasDisponibles = [] -- Esta lista debería ser poblada con salas creadas en Operativas.hs

actualizarSalasDisponibles :: [SalaO] -> IO ()
actualizarSalasDisponibles nuevasSalas = do
    let salasDisponibles = nuevasSalas  -- Actualizamos las salas disponibles
    putStrLn "Salas disponibles actualizadas."

-- Lista simulada de reservas
reservas :: IORef [Reserva]
reservas = unsafePerformIO $ newIORef []

-- Función para actualizar las reservas
actualizarReservas :: [Reserva] -> IO ()
actualizarReservas nuevasReservas = writeIORef reservas nuevasReservas

-- Función para obtener las reservas actuales
obtenerReservas :: IO [Reserva]
obtenerReservas = readIORef reservas
-- Función para generar un ID único de reserva
generarIdReserva :: IO ReservationID
generarIdReserva = do
    currentReservas <- readIORef reservas
    return $ "R" ++ show (length currentReservas + 1)

-- Función para comprobar si la sala está disponible en la fecha solicitada
salaDisponible :: SalaO -> Date -> IO Bool
salaDisponible salaInput fechaInput = do
    currentReservas <- readIORef reservas
    -- Verifica si ya existe una reserva con la misma sala y fecha
    let reservaExistente = find (\r -> codigoSala (sala r) == codigoSala salaInput && date r == fechaInput) currentReservas
    return $ case reservaExistente of
        Nothing -> True  -- La sala está disponible
        Just _  -> False -- La sala ya está reservada

-- Función para la Gestión de Reserva
gestionarReserva :: [SalaO] -> IO ()
gestionarReserva salasDisponibles = do
    putStrLn "Ingrese su ID de usuario:"
    hFlush stdout
    userIdInput <- getLine
    -- Verificar si el usuario existe
    let usuarioEncontrado = find (\u -> idCedula u == userIdInput) usuarios
    case usuarioEncontrado of
        Nothing -> putStrLn "Usuario no encontrado, intente de nuevo."
        Just usuario -> do
            putStrLn $ "Bienvenido, " ++ nombreUs usuario
            putStrLn "Ingrese el código de la sala:"
            hFlush stdout
            salaInput <- getLine
            putStrLn "Ingrese la fecha (YYYY-MM-DD):"
            hFlush stdout
            dateInput <- getLine
            putStrLn "Ingrese la cantidad de personas:"
            hFlush stdout
            personasInput <- getLine

            -- Verificar si la sala existe en la lista actualizada
            let salaEncontrada = find (\s -> codigoSala s == salaInput) salasDisponibles
            case salaEncontrada of
                Nothing -> putStrLn "Sala no encontrada, intente de nuevo."
                Just sala -> do
                    let personasNum = read personasInput :: Int
                    -- Validar la capacidad de la sala
                    if personasNum > capacidadSala sala
                        then putStrLn "La sala no tiene capacidad suficiente."
                        else do
                            -- Validar y parsear la fecha
                            case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateInput of
                                Nothing -> putStrLn "Fecha no válida, intente de nuevo."
                                Just fechaParsed -> do
                                    -- Verificar si la sala está disponible en la fecha solicitada
                                    disponible <- salaDisponible sala fechaParsed
                                    if not disponible
                                        then putStrLn "La sala ya está reservada para esa fecha."
                                        else do
                                            -- Generar ID de reserva
                                            newId <- generarIdReserva
                                            -- Crear y añadir la nueva reserva
                                            let nuevaReserva = Reserva newId userIdInput sala fechaParsed personasNum
                                            modifyIORef reservas (nuevaReserva :)
                                            putStrLn $ "Reserva creada exitosamente con ID: " ++ reservaId nuevaReserva

-----------------------------------------------Consultar Reserva-----------------------------------------------
-- Función para consultar reserva por ID de usuario o por ID de reserva
consultarReserva :: [SalaO] -> IO ()
consultarReserva _ = do  -- No se necesita usar la lista de salas aquí
    putStrLn "Consultar reserva"
    putStrLn "1. Buscar por ID de reserva"
    putStrLn "2. Buscar por ID de usuario"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID de la reserva:"
            hFlush stdout
            reservaIdInput <- getLine
            reservaPorId reservaIdInput
        "2" -> do
            putStrLn "Ingrese el ID de usuario:"
            hFlush stdout
            userIdInput <- getLine
            reservasUsuario userIdInput
        _ -> putStrLn "Opción no válida, intente de nuevo."

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
    putStrLn $ "Código de sala: " ++ codigoSala (sala reserva)
    putStrLn $ "Nombre de sala: " ++ nombreSala (sala reserva)
    putStrLn $ "Fecha de la reserva: " ++ show (date reserva)
    putStrLn $ "Cantidad de personas: " ++ show (personas reserva)

-----------------------------------------------Cancelar Reserva-----------------------------------------------
-- Función para cancelar una reserva
cancelarReserva :: [SalaO] -> IO ()
cancelarReserva _ = do  -- La lista de salas no es necesaria en este contexto
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
salaDisponibleParaModificacion :: SalaO -> Date -> ReservationID -> IO Bool
salaDisponibleParaModificacion nuevaSala dateParsed reservaIdActual = do
    currentReservas <- readIORef reservas
    -- Verifica si hay alguna reserva en la misma sala y fecha, pero ignora la actual
    let reservaExistente = find (\r -> codigoSala (sala r) == codigoSala nuevaSala && date r == dateParsed && reservaId r /= reservaIdActual) currentReservas
    return $ case reservaExistente of
        Nothing -> True  -- La sala está disponible
        Just _  -> False -- La sala ya está reservada

-- Función para modificar una reserva existente
modificarReserva :: [SalaO] -> IO ()
modificarReserva salasDisponibles = do
    putStrLn "Ingrese el ID de la reserva que desea modificar:"
    hFlush stdout
    reservaIdInput <- getLine

    currentReservas <- readIORef reservas
    -- Busca la reserva por su ID
    let reservaEncontrada = find (\r -> reservaId r == reservaIdInput) currentReservas
    case reservaEncontrada of
        Nothing -> putStrLn "Reserva no encontrada, intente de nuevo."
        Just reserva -> do
            -- Muestra opciones de modificación
            putStrLn "Seleccione el campo a modificar:"
            putStrLn "1. Modificar Sala"
            putStrLn "2. Modificar Fecha"
            putStrLn "3. Modificar Cantidad de Personas"
            putStrLn "4. Cancelar modificación"
            opcion <- getLine
            case opcion of
                "1" -> do
                    putStrLn "Ingrese el nuevo código de sala:"
                    hFlush stdout
                    nuevoCodigo <- getLine
                    -- Busca la nueva sala por su código
                    let nuevaSalaOpt = find (\s -> codigoSala s == nuevoCodigo) salasDisponibles
                    case nuevaSalaOpt of
                        Nothing -> putStrLn "Sala no encontrada, intente de nuevo."
                        Just nuevaSala -> do
                            -- Verifica si la sala está disponible para la misma fecha (ignorando la reserva actual)
                            disponible <- salaDisponibleParaModificacion nuevaSala (date reserva) (reservaId reserva)
                            if not disponible
                                then putStrLn "La nueva sala ya está reservada para la fecha seleccionada."
                                else do
                                    -- Modifica la sala de la reserva
                                    let reservaModificada = reserva { sala = nuevaSala }
                                    actualizarReserva reserva reservaModificada
                                    putStrLn "Reserva modificada exitosamente (Sala)."
                "2" -> do
                    putStrLn "Ingrese la nueva fecha (YYYY-MM-DD):"
                    hFlush stdout
                    nuevaFechaInput <- getLine
                    -- Verifica si la nueva fecha es válida
                    case parseTimeM True defaultTimeLocale "%Y-%m-%d" nuevaFechaInput of
                        Nothing -> putStrLn "Fecha no válida, intente de nuevo."
                        Just nuevaFecha -> do
                            -- Verifica si la sala está disponible para la nueva fecha
                            disponible <- salaDisponible (sala reserva) nuevaFecha
                            if not disponible
                                then putStrLn "La sala ya está reservada para la nueva fecha."
                                else do
                                    -- Modifica la fecha de la reserva
                                    let reservaModificada = reserva { date = nuevaFecha }
                                    actualizarReserva reserva reservaModificada
                                    putStrLn "Reserva modificada exitosamente (Fecha)."
                "3" -> do
                    putStrLn "Ingrese la nueva cantidad de personas:"
                    hFlush stdout
                    nuevaCantidadInput <- getLine
                    let nuevaCantidad = read nuevaCantidadInput :: Int
                    -- Verifica si la sala tiene capacidad suficiente
                    if nuevaCantidad > capacidadSala (sala reserva)
                        then putStrLn "La sala no tiene capacidad suficiente."
                        else do
                            -- Modifica la cantidad de personas en la reserva
                            let reservaModificada = reserva { personas = nuevaCantidad }
                            actualizarReserva reserva reservaModificada
                            putStrLn "Reserva modificada exitosamente (Cantidad de personas)."
                "4" -> putStrLn "Modificación cancelada."
                _   -> putStrLn "Opción no válida, intente de nuevo."

-- Función para actualizar la lista de reservas
actualizarReserva :: Reserva -> Reserva -> IO ()
actualizarReserva viejaReserva nuevaReserva = do
    currentReservas <- readIORef reservas
    -- Filtra las reservas para eliminar la antigua y agregar la modificada
    let nuevasReservas = nuevaReserva : filter (\r -> reservaId r /= reservaId viejaReserva) currentReservas
    writeIORef reservas nuevasReservas
-----------------------------------------------Consulta de disponibilidad de salas-----------------------------------------------
-- Función para la consulta de disponibilidad de salas
consultaDisponibilidadSala :: [SalaO] -> IO ()
consultaDisponibilidadSala salasDisponibles = do
    putStrLn "----- Consulta de Disponibilidad de Sala -----"
    putStrLn "1. Consultar por una fecha específica"
    putStrLn "2. Consultar por un rango de fechas"
    putStrLn "Seleccione una opción:"
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> consultaPorFechaEspecifica salasDisponibles
        "2" -> consultaPorRangoDeFechas salasDisponibles
        _   -> do
            putStrLn "Opción no válida, por favor seleccione nuevamente."
            consultaDisponibilidadSala salasDisponibles

-- Función para consultar la disponibilidad por una fecha específica
consultaPorFechaEspecifica :: [SalaO] -> IO ()
consultaPorFechaEspecifica salasDisponibles = do
    putStrLn "Ingrese la fecha (YYYY-MM-DD):"
    hFlush stdout
    dateInput <- getLine
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateInput of
        Nothing -> putStrLn "Fecha no válida, intente de nuevo."
        Just dateParsed -> do
            currentReservas <- readIORef reservas
            -- Obtener las salas reservadas en esa fecha
            let salasReservadas = [sala r | r <- currentReservas, date r == dateParsed]
            -- Filtrar las salas libres comparando con las salas reservadas
            let salasLibres = filter (\s -> s `notElem` salasReservadas) salasDisponibles
            if null salasLibres
                then putStrLn "No hay salas disponibles para esa fecha."
                else do
                    putStrLn "Salas disponibles:"
                    mapM_ (\s -> putStrLn $ nombreSala s ++ " (Código: " ++ codigoSala s ++ ")") salasLibres

-- Función para consultar la disponibilidad por un rango de fechas
consultaPorRangoDeFechas :: [SalaO] -> IO ()
consultaPorRangoDeFechas salasDisponibles = do
    putStrLn "Ingrese la fecha de inicio (YYYY-MM-DD):"
    hFlush stdout
    inicioInput <- getLine
    putStrLn "Ingrese la fecha de fin (YYYY-MM-DD):"
    hFlush stdout
    finInput <- getLine
    case (parseTimeM True defaultTimeLocale "%Y-%m-%d" inicioInput, parseTimeM True defaultTimeLocale "%Y-%m-%d" finInput) of
        (Just inicioParsed, Just finParsed) -> do
            let fechas = [inicioParsed..finParsed]  -- Generar una lista de fechas dentro del rango
            mapM_ (mostrarDisponibilidadPorDia salasDisponibles) fechas
        _ -> putStrLn "Fechas no válidas, intente de nuevo."

-- Función auxiliar para mostrar la disponibilidad de un día específico
mostrarDisponibilidadPorDia :: [SalaO] -> Date -> IO ()
mostrarDisponibilidadPorDia salasDisponibles dateParsed = do
    putStrLn $ "\nDisponibilidad para la fecha: " ++ show dateParsed
    currentReservas <- readIORef reservas
    -- Obtener las salas reservadas en esa fecha
    let salasReservadas = [sala r | r <- currentReservas, date r == dateParsed]
    -- Filtrar las salas libres
    let salasLibres = filter (\s -> s `notElem` salasReservadas) salasDisponibles
    if null salasLibres
        then putStrLn "No hay salas disponibles."
        else do
            putStrLn "Salas disponibles:"
            mapM_ (\s -> putStrLn $ nombreSala s ++ " (Código: " ++ codigoSala s ++ ")") salasLibres
