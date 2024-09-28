module Usuarios(
    Usuario(..),
    usuarios,
    mostrarUsuarios,
) where

data Usuario = Usuario {

    idCedula :: String,
    nombreUs :: String,
    puesto :: String
}deriving(Show, Eq)

usuarios :: [Usuario] 
usuarios = [
    Usuario "703080762" "Gadyr Caderon" "Duenio",
    Usuario "01234567" "Bayron la cabra" "Duenio",
    Usuario "12345678" "Fredd come nepes" "Gerente",
    Usuario "12345678" "Juan El calvito Perez" "Conserje",
    Usuario "12345678" "Miguel Perron" "Sapo"
    ]

-- Función para mostrar los usuarios en memoria
mostrarUsuarios:: [Usuario] -> IO()
mostrarUsuarios [] = putStrLn "No hay más "
mostrarUsuarios(x: xs) = do
    putStrLn $ "ID: " ++ idCedula x ++ " Nombre Usuario: "++ nombreUs x ++ " Puesto: " ++ puesto x
    mostrarUsuarios xs 

