module Main where
import Text.XHtml (menu)
import Operativas(cargarMobiliario,
    mostrarMobiliario,
    Mobiliario, crearSala, mostrarSala, SalaO, crearSalas)  -- Importa las funciones y el tipo de datos desde Operativas.hs
import Generales(gestionarReserva, consultarReserva, cancelarReserva, modificarReserva, consultaDisponibilidadSala, Sala)
import Usuarios(Usuario(..), usuarios, mostrarUsuarios)
-- Función principal para mostrar el menú
main :: IO ()
main = do
    let mobiliario = listaVaciaMobiliario  -- Inicializa lista vacía de mobiliario
    let salas = listaVaciaSala             -- Inicializa lista vacía de salas
    putStrLn "----- Menú Principal -----"
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Ver Usuarios en Memoria"
    putStrLn "4. Salir"
    putStrLn "Seleccione una opción:"
    opcion <- getLine
    menuHandler opcion mobiliario salas

-- Función para manejar la selección del menú
menuHandler :: String -> [Mobiliario] -> [SalaO] -> IO ()
menuHandler "1" mobiliario salas = submenuOperativas mobiliario salas  -- Llama al submenú de Opciones Operativas
menuHandler "2" mobiliario salas = submenuGenerales mobiliario salas
menuHandler "3" mobiliario salas = do
    -- mostrarUsuarios usuarios
    main
menuHandler "4" _ _ = putStrLn "Saliendo del programa."
menuHandler _ mobiliario salas = do
    putStrLn "Opción no válida, por favor seleccione nuevamente."
    main

listaVaciaMobiliario :: [Mobiliario]
listaVaciaMobiliario = []

listaVaciaSala :: [SalaO]
listaVaciaSala = []

-- Función para el submenú de Opciones Operativas
submenuOperativas :: [Mobiliario] -> [SalaO] -> IO ()
submenuOperativas mobiliarioExistente salasExistentes = do
    putStrLn "----- Submenú Opciones Operativas -----"
    putStrLn "1. Carga y mostrar mobiliario de sala"
    putStrLn "2. Cargar salas de reunión"
    putStrLn "3. Mostrar salas de reunión"
    putStrLn "4. Informe de reservas"
    putStrLn "5. Volver al menú principal"
    putStrLn "Seleccione una opción:"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la ruta del archivo:"
            ruta <- getLine
            nuevoMobiliario <- cargarMobiliario ruta
            mostrarMobiliario nuevoMobiliario
            -- Llama recursivamente a submenuOperativas pasando la nueva lista de mobiliario
            submenuOperativas nuevoMobiliario salasExistentes

        "2" -> do
            putStrLn "Has seleccionado la Opción de Cargar y Mostrar salas de reunión."
            -- Crear una sala utilizando el mobiliario cargado
            nuevasSalas <- crearSalas 1 mobiliarioExistente salasExistentes
            -- Llama recursivamente pasando la lista actualizada de salas
            submenuOperativas mobiliarioExistente nuevasSalas

        "3" -> do 
            putStrLn "Ingrese el código de la sala que desea consultar:"
            codigo <- getLine
            mostrarSala codigo salasExistentes
            submenuOperativas mobiliarioExistente salasExistentes

        "4" -> do
            putStrLn "Has seleccionado la opción de Informe de Reservas."
            -- Aquí podrías implementar la funcionalidad de informes de reservas
            submenuOperativas mobiliarioExistente salasExistentes

        "5" -> main  -- Vuelve al menú principal

        _ -> do
            putStrLn "Opción no válida, por favor seleccione nuevamente."
            submenuOperativas mobiliarioExistente salasExistentes  -- Vuelve a mostrar el submenú

-- Función para el submenú de Opciones Generales (puedes implementarla)
submenuGenerales :: [Mobiliario] -> [SalaO] -> IO ()
submenuGenerales mobiliario salas = do
    putStrLn "----- Submenú Opciones Generales -----"
    -- Aquí implementas las opciones generales
    main

-- Ejemplo de función para mostrar usuarios (puedes adaptarla)
mostrarUsuarios :: [Usuario] -> IO ()
mostrarUsuarios usuarios = do
    -- Aquí va el código para mostrar los usuarios
    putStrLn "Mostrando usuarios..."
    main


-- submenuGenerales :: IO()
-- submenuGenerales = do
--     putStrLn "----- Submenú Opciones Generales -----"
--     putStrLn "1. Gestión de reserva "
--     putStrLn "2. Consultar de reserva"
--     putStrLn "3. Cancelación de reservas"
--     putStrLn "4. Modificación de reservas"
--     putStrLn "5. Consulta de disponibilidad de sala"
--     putStrLn "6. Volver al menú principal"
--     putStrLn "Seleccione una opción:"
--     opcion <- getLine
--     case opcion of 
--         "1" -> do 
--             putStrLn "Has seleccionado la Opción de Gestión de reserva."
--             gestionarReserva
--             submenuGenerales
--         "2" -> do 
--             putStrLn "Consultar de reserva."
--             consultarReserva
--             submenuGenerales
--         "3"-> do 
--             putStrLn "Cancelación de reservas."
--             cancelarReserva
--             submenuGenerales
--         "4" -> do
--             putStrLn "Modificación de reservas."
--             modificarReserva
--             submenuGenerales
--         "5" -> do
--             putStrLn "Consulta de disponibilidad de sala."
--             consultaDisponibilidadSala
--             submenuGenerales
--         "6" -> do
--             putStrLn "Volviendo al Menu principal."
--             main
--         _ -> do
--             putStrLn "Opcion Invalida, Selecione una Opcione correcta"
--             submenuGenerales
