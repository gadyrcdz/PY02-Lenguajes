module Main where
import Text.XHtml (menu)
import Operativas(cargarMobiliario,
    mostrarMobiliario,
    Mobiliario, 
    crearSala, 
    mostrarSala, 
    SalaO, 
    crearSalas)  -- Importa las funciones y el tipo de datos desde Operativas.hs
import Generales(gestionarReserva, 
    consultarReserva, 
    cancelarReserva, 
    modificarReserva, 
    consultaDisponibilidadSala, 
    Reserva,
    obtenerReservas,
    actualizarReservas)  -- Importa las funciones y el tipo de datos desde Generales.hs
import Usuarios(Usuario(..), usuarios, mostrarUsuarios)
import System.IO (hFlush, stdout)

-- Función principal para mostrar el menú
mainMenu :: [Mobiliario] -> [SalaO] -> IO ()
mainMenu mobiliario salas = do
    reservasActuales <- obtenerReservas
    putStrLn "----- Menú Principal -----"
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Ver Usuarios en Memoria"
    putStrLn "4. Salir"
    putStrLn "Seleccione una opción:"
    opcion <- getLine
    menuHandler opcion mobiliario salas reservasActuales

menuHandler :: String -> [Mobiliario] -> [SalaO] -> [Reserva] -> IO ()
menuHandler "1" mobiliario salas reservas = submenuOperativas mobiliario salas reservas
menuHandler "2" mobiliario salas reservas = submenuGenerales mobiliario salas reservas
menuHandler "3" mobiliario salas reservas = do
    --mostrarUsuarios usuarios
    mainMenu mobiliario salas 
menuHandler "4" _ _ _ = putStrLn "Saliendo del programa."
menuHandler _ mobiliario salas _ = do
    putStrLn "Opción no válida, por favor seleccione nuevamente."
    mainMenu mobiliario salas

listaVaciaMobiliario :: [Mobiliario]
listaVaciaMobiliario = []

listaVaciaSala :: [SalaO]
listaVaciaSala = []

listaVaciaReserva :: [Reserva]
listaVaciaReserva = []

-- Función para el submenú de Opciones Operativas
submenuOperativas :: [Mobiliario] -> [SalaO] -> [Reserva] -> IO ()
submenuOperativas mobiliarioExistente salasExistentes reservas = do
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
            submenuOperativas nuevoMobiliario salasExistentes reservas

        "2" -> do
            putStrLn "Has seleccionado la Opción de Cargar y Mostrar salas de reunión."
            -- Crear una sala utilizando el mobiliario cargado
            nuevasSalas <- crearSalas 1 mobiliarioExistente salasExistentes
            -- Llama recursivamente pasando la lista actualizada de salas
            submenuOperativas mobiliarioExistente nuevasSalas reservas
        "3" -> do 
            putStrLn "Ingrese el código de la sala que desea consultar:"
            codigo <- getLine
            mostrarSala codigo salasExistentes
            submenuOperativas mobiliarioExistente salasExistentes reservas

        "4" -> do
            putStrLn "Has seleccionado la opción de Informe de Reservas."
            submenuOperativas mobiliarioExistente salasExistentes reservas

        "5" -> mainMenu mobiliarioExistente salasExistentes  -- Vuelve al menú principal con los datos actuales

        _ -> do
            putStrLn "Opción no válida, por favor seleccione nuevamente."
            submenuOperativas mobiliarioExistente salasExistentes reservas  -- Vuelve a mostrar el submenú



-- Función para el submenú de Opciones Generales
submenuGenerales :: [Mobiliario] -> [SalaO] -> [Reserva] -> IO ()
submenuGenerales mobiliario salas reservas = do
    putStrLn "\n----- Submenú Opciones Generales -----"
    putStrLn "1. Gestión de reserva"
    putStrLn "2. Consultar reserva"
    putStrLn "3. Cancelación de reservas"
    putStrLn "4. Modificación de reservas"
    putStrLn "5. Consulta de disponibilidad de sala"
    putStrLn "6. Volver al menú principal"
    putStrLn "Seleccione una opción:"
    hFlush stdout
    opcion <- getLine
    case opcion of 
        "1" -> do 
            gestionarReserva salas 
            nuevasReservas <- obtenerReservas
            submenuGenerales mobiliario salas nuevasReservas
        "2" -> do 
            consultarReserva salas 
            submenuGenerales mobiliario salas reservas
        "3" -> do 
            cancelarReserva salas 
            nuevasReservas <- obtenerReservas
            submenuGenerales mobiliario salas nuevasReservas
        "4" -> do
            modificarReserva salas 
            nuevasReservas <- obtenerReservas
            submenuGenerales mobiliario salas nuevasReservas
        "5" -> do
            consultaDisponibilidadSala salas 
            submenuGenerales mobiliario salas reservas
        "6" -> mainMenu mobiliario salas
        _   -> do
            putStrLn "Opción no válida, por favor seleccione nuevamente."
            submenuGenerales mobiliario salas reservas

-- Ejemplo de función para mostrar usuarios (puedes adaptarla)
mostrarUsuarios :: [Usuario] -> IO ()
mostrarUsuarios usuarios = do
    -- Aquí va el código para mostrar los usuarios
    putStrLn "Mostrando usuarios..."
    mainMenu listaVaciaMobiliario listaVaciaSala

-- Inicializa las listas vacías y llama a la función main
main :: IO ()
main = do
    actualizarReservas listaVaciaReserva
    mainMenu listaVaciaMobiliario listaVaciaSala
