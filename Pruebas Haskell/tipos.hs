module Tipos where

-- Comentario de UNA linea

{-
    Comentario
    MULTI-linea
-}

miNumeroEntero :: Integer
miNumeroEntero = 5

miNumeroDecimal :: Float
miNumeroDecimal = 2.85

miLetra :: Char
miLetra = 'Q'

miNombre :: [Char]
miNombre = "Roberto"

miApellido :: [Char]
miApellido = ['G','o','m','e','z']

listaDeNumeros :: [Integer]
listaDeNumeros = [1,2,3,4]

tuplaDeNumeros :: (Int, Float)
tuplaDeNumeros = (5, 80.78)

-- Añade el elemento al principio de la lista
anadirElementoEnLista :: [a] -> a -> [a]
anadirElementoEnLista lista elemento = elemento:lista 

-- (e:l) lo que hace es poner en e el primer elemento de la lista y en l el resto de la lista
sacarElementoEnLista :: [a] -> a
sacarElementoEnLista (e:l) = e

listaDelUnoAlCien = [1..100]

listaDeNumerosParesDelUnoAlCien = [0, 2..100]

listaDeNumerosImparesDelUnoAlCien = [1, 3..] --Si lo dejo asi se va hasta el infinito

-- Lista por comprensión
multiplosDeTres = [n | n <- [1..30], mod n 3 == 0] -- Multiplos de tres hasta el 30 