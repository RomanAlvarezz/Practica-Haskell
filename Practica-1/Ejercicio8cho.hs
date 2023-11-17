module Ejercicio8cho where

--A
divisorss n = if (n<0) then [] else [x | x <- [1..n], n `mod` x == 0]

--B matches, que dados un entero x y una lista de enteros descarta de la lista los elementos distintos a x
matches n l = [x | x <- l, x == n]

--C  unique, que dada una lista xs de enteros, devuelve la lista con los elementos no repetidos de xs.
--unique l = [x | x <- l, y <-, x == y]
isRepeated _ [] _ = False
--isRepeated 2 _  _ = True
--isRepeated cont (e:l) n = if (e == n) then (isRepeated (cont + 1) l n) else (isRepeated cont l n)
isRepeated cont (e:l) n = if (cont < 2) then (if e==n then (isRepeated (cont + 1) l n) else (isRepeated cont l n)) else True


repeated :: [Int] -> Bool
repeated [] = False
repeated [_] = False
repeated (h:t) = if elem h t then True
                             else repeated t





buscarNumeroRepetido _ [] _ = False  
buscarNumeroRepetido contador (x:xs) num
    | (contador == 2) = True 
    | (x == num) = buscarNumeroRepetido (contador + 1) xs num 
    | otherwise = buscarNumeroRepetido contador xs num 


--Por que no anda?
buscarNumeroRepetido' :: Int -> [Int] -> Int -> Bool
buscarNumeroRepetido' _ [] _ = False  
buscarNumeroRepetido' cont (e:l) n = if (cont == 2)
                                        then True
                                    else if (e == n)
                                        then buscarNumeroRepetido' (cont + 1) l n
                                    else buscarNumeroRepetido' cont l n

pruebaAnidacion :: Int -> [Char]
pruebaAnidacion n = if (n == 1)
                        then "Uno"
                    else if (n == 2)
                        then "Dos"
                    else "Otro"

buscarNumeroRepetido'' :: Int -> [Int] -> Bool
buscarNumeroRepetido'' e l = if ((length [x | x <- l, x == e]) > 1) then True else False