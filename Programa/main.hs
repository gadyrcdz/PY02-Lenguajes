module Main where
import Text.XHtml (menu)
import Operativas()
import Generales(gestionarReserva, consultarReserva, cancelarReserva, modificarReserva, consultaDisponibilidadSala)
import Usuarios(Usuario(..), usuarios, mostrarUsuarios)
-- Función principal para mostrar el menú
main :: IO ()
main = do
    putStrLn "----- Menú Principal -----"
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Ver Usuarios en Memoria"
    putStrLn "4. Salir"
    putStrLn "Seleccione una opción:"
    opcion <- getLine
    menuHandler opcion

-- Función para manejar la selección del menú
menuHandler :: String -> IO ()
menuHandler "1" = submenuOperativas  -- Llama al submenú de Opciones Operativas
menuHandler "2" = submenuGenerales
menuHandler "3" = do 
    mostrarUsuarios usuarios
    main
menuHandler "4" = putStrLn "Saliendo del programa."
menuHandler _   = do
    putStrLn "Opción no válida, por favor seleccione nuevamente."
    main

-- Función para el submenú de Opciones Operativas
submenuOperativas :: IO ()
submenuOperativas = do
    putStrLn "----- Submenú Opciones Operativas -----"
    putStrLn "1. Cargar y Mostrar salas de reunión"
    putStrLn "2. Cargar y Mostrar salas de reunión"
    putStrLn "3. Informe de reservas"
    putStrLn "4. Volver al menú principal"
    putStrLn "Seleccione una opción:"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Has seleccionado la Opción de Crear y Mostrar mobiliario de sala."
            submenuOperativas  -- Vuelve al submenú

        "2" -> do
            putStrLn "Has seleccionado la Opción de Cargar y Mostrar salas de reunión."
            submenuOperativas  -- Vuelve al submenú
        "3" -> do 
            putStrLn "Has seleccionado la opcion de Informe de Reservas"
            submenuOperativas

        "4" -> main  -- Vuelve al menú principal
        _   -> do
            putStrLn "Opción no válida, por favor seleccione nuevamente."
            submenuOperativas  -- Vuelve a mostrar el submenú


submenuGenerales :: IO()
submenuGenerales = do
    putStrLn "----- Submenú Opciones Generales -----"
    putStrLn "1. Gestión de reserva "
    putStrLn "2. Consultar de reserva"
    putStrLn "3. Cancelación de reservas"
    putStrLn "4. Modificación de reservas"
    putStrLn "5. Consulta de disponibilidad de sala"
    putStrLn "6. Volver al menú principal"
    putStrLn "Seleccione una opción:"
    opcion <- getLine
    case opcion of 
        "1" -> do 
            putStrLn "Has seleccionado la Opción de Gestión de reserva."
            gestionarReserva
            submenuGenerales
        "2" -> do 
            putStrLn "Consultar de reserva."
            consultarReserva
            submenuGenerales
        "3"-> do 
            putStrLn "Cancelación de reservas."
            cancelarReserva
            submenuGenerales
        "4" -> do
            putStrLn "Modificación de reservas."
            modificarReserva
            submenuGenerales
        "5" -> do
            putStrLn "Consulta de disponibilidad de sala."
            consultaDisponibilidadSala
            submenuGenerales
        "6" -> do
            putStrLn "Volviendo al Menu principal."
            main
        _ -> do
            putStrLn "Opcion Invalida, Selecione una Opcione correcta"
            submenuGenerales
