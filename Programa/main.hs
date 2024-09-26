-- Función principal para mostrar el menú
main :: IO ()
main = do
    putStrLn "----- Menú Principal -----"
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir"
    putStrLn "Seleccione una opción:"
    opcion <- getLine
    menuHandler opcion

-- Función para manejar la selección del menú
menuHandler :: String -> IO ()
menuHandler "1" = submenuOperativas  -- Llama al submenú de Opciones Operativas
menuHandler "2" = putStrLn "Has seleccionado Opciones Generales."
menuHandler "3" = putStrLn "Saliendo del programa."
menuHandler _   = do
    putStrLn "Opción no válida, por favor seleccione nuevamente."
    main

-- Función para el submenú de Opciones Operativas
submenuOperativas :: IO ()
submenuOperativas = do
    putStrLn "----- Submenú Opciones Operativas -----"
    putStrLn "1. Crear y Mostrar mobiliario de sala "
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
