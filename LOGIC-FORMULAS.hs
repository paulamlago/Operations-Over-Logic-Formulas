{- PAULA MUÑOZ LAGO
   PROGRAMACIÓN DECLARATIVA
   FEBRERO 2018
-}

{- ejemplos
   f1 = Si (No (V "p")) (Si (V "p") (Y (V "q") (No (V "q"))))
   f2 = Y (V "p") (Si (No (V "q")) (No (V "p")))
   f3 = Y (Y (V "p") (V "q")) (O (No (V "q")) (V "r"))
   f4 = No (Y (V "p") (O (V "p")(V "r"))) 
   f5 = No (Y (O (V "p") (V "r")) (V "p"))
   f6 = Si (Y (V "a") (V "b")) (O (V "a") (No (V "c")))
   f7 = O (V "p") (No (V "p"))
-}
type Var = String -- nombres de variables
data FProp = V Var | No FProp | Y FProp FProp | O FProp FProp | Si FProp FProp | Sii FProp FProp
type Interpretacion = [(String, Bool)]

--instancias de tipos

instance Show FProp where
    show (V a)    = a
    show (No a)   = "!"  ++ show a
    show (Y a b)  = "(" ++ show a ++ " && " ++ show b ++ ")"
    show (O a b)  = "(" ++ show a ++ " || " ++ show b ++ ")"
    show (Si a b) = "(" ++ show a ++ "->" ++ show b ++ ")"
    show (Sii a b) = "(" ++ show a ++ "<->" ++ show b ++ ")"

instance Eq FProp where
    (V a) == (V b) = (a == b)
    (No a) == (No b) = (a == b)
    (Y a b) == (Y c d) = True
    (O a b) == (O c d) = True
    (Si a b) == (Si c d) = (a == c) && (b == d)
    (Sii a b) == (Sii c d) = (a == c) && (b == d)

instance Ord FProp where
    a <= b = consecuencia a b

-- main, donde ocurre la interaccion con el usuario 

main = do putStrLn "Welcome to the propositional logic application!"
          putStrLn "Introduce a formula: "
          f <- getLine --pide una formula en este punto para los apartados del menu 1, 2, 3, 8
          putStrLn "1. Get the variables in the formula"
          putStrLn "2. Is it a tautology?"
          putStrLn "3. Is it satisfactible?"
          putStrLn "4. Is it a consequence of another formula?" --en los apartados del 4 al 7 necesitará más formulas
          putStrLn "5. Is it equivalent to another formula?"
          putStrLn "6. Consecuences of a list of formulas"
          putStrLn "7. Equivalents of a list of formulas"
          putStrLn "8. Print the formula"
          putStrLn "0. Exit"
          putStrLn "Your option: "

          op <- getChar -- solo pide un caracter como opcion ya que los indices van de 0 a 9
          putStrLn ""
          if (op /= '0') then 
            parseOp op (fromStringToFProp f)
          else putStrLn "Thank you for using this program!..."

--funcion encargada de transformar la string introducida por el usuario a una formula de tipo FProp
fromStringToFProp::String -> FProp                                                             --1   2     
fromStringToFProp s | null s == False && head s == 'V' = (V $ (take 3 (drop 2 s)))  --drop 2 por V espacio               --1 2 3 4
                    | null s == False && head s == 'N' && s !! 1 == 'o' = (No $ fromStringToFProp (drop 4 s)) --drop 4 por N O   (
                    | null s == False && head s == 'S' && s !! 1 == 'i' = (Si (fromStringToFProp (drop 4 s)) (fromStringToFProp (drop (nextParenthesis s 2 0) s)))
                    | null s == False && head s == 'S' && s !! 1 == 'i' && s !! 2 == 'i' = (Sii (fromStringToFProp (drop 5 s)) (fromStringToFProp (drop (nextParenthesis s 2 0) s)))
                    | null s == False && head s == 'Y' = (Y (fromStringToFProp (drop 3 s)) (fromStringToFProp (drop (nextParenthesis s 2 0) s)))
                    | null s == False && head s == 'O' = (O (fromStringToFProp (drop 3 s)) (fromStringToFProp (drop (nextParenthesis s 2 0) s)))
                    | otherwise = fromStringToFProp (drop 1 s)

--se utiliza en la funcion fromStringToFProp para saber a partir de qué punto tiene que seguir leyendo
nextParenthesis::String -> Int -> Int -> Int --el primer parametro la cadena, el segundo la posicion por la que queremos cortarla, y el tercero el numero de parentesis abiertos
nextParenthesis s pos opened | null s == False && head s == ')' = if (opened > 1) then nextParenthesis (drop 1 s) (pos+1) (opened -1)
                                                                  else (pos + 1)
                             | null s == False && head s == '(' = nextParenthesis (drop 1 s) (pos + 1) (opened + 1)
                             | null s == False = nextParenthesis (drop 1 s) (pos + 1) opened
                             | otherwise = pos

parseOp::Char -> FProp -> IO() --funcion destinada a procesar las distintas opciones que puede introducir el usuario
parseOp op f
         | op == '1' = print $ vars f
         | op == '2' = print $ tautologia f
         | op == '3' = print $ satisfactible f
         | op == '4' = do { putStrLn "Please, introduce another formula to check if the first one is a consecuence of the second: "
                            ; f1 <- getLine
                            ; print $ consecuencia f (fromStringToFProp f1) }
         | op == '5' = do { putStrLn "Please, introduce another formula to check if they are equivalent: "
                            ; f1 <- getLine
                            ; print $ equivalente f (fromStringToFProp f1) }
         | op == '6' = do {  putStrLn "Please, introduce the NUMBER offormulas that you want to insert in the list: "
                            ; n <- getLine
                            ; let n' = (read n :: Int) -- de esta forma se convierte el tipo string que tenia n en int y lo guarda en n'
                            ; l <- mapM (\a -> do  --gracias a mapM repetimos esta lambda expresion n' veces para cada valor entre 1 y n'
                            ;  putStrLn $ "Insert the formula number " ++ show a ++ ": "
                            ;  f1 <- getLine
                            ;  let f2 = fromStringToFProp f1 --de esta forma convierte la string f1 en FProp y lo guarda en f2
                            ;  return f2
                            ;  ) [1..n']
                            ; print $ consecuencias (f:l) }
         | op == '7' = do {  putStrLn "Please, introduce the NUMBER of formulas that you want to insert in the list: "
                            ; n <- getLine
                            ; let n' = (read n :: Int)
                            ; l <- mapM (\a -> do  
                            ;  putStrLn $ "Insert the formula number " ++ show a ++ ": "
                            ;  f1 <- getLine
                            ;  let f2 = fromStringToFProp f1
                            ;  return f2
                            ;  ) [1..n']
                            ; print $ equivalentes (f:l) }
         | op == '8' = print f --este caso solo imprime la formula (para comprobar que fromStringToFProp funciona correctamente)
         | otherwise = putStrLn "Index ouf of bounds! The menu ranges from 0 to 8, sorry!"

vars:: FProp -> [String]
vars f = elimRepetidos (vars' f [])

vars':: FProp -> [String] -> [String]
vars' (V a) b = a:b
vars' (No a) b = (vars' a b)
vars' (Y a c) b = (vars' a b)++(vars' c b)
vars' (O a c) b = (vars' a b)++(vars' c b)
vars' (Si a c) b = (vars' a b)++(vars' c b)
vars' (Sii a c) b = (vars' a b)++(vars' c b)

elimRepetidos:: [String] -> [String]
elimRepetidos a = elimRepetidos' a []

elimRepetidos'::[String] -> [String] -> [String]
elimRepetidos' [] _ = []
elimRepetidos' (e:a) b | elem e b = elimRepetidos' a b
                       | otherwise = e:elimRepetidos' a (e:b)

--busca el valor (True o False) de una variable entre las combinaciones
buscaEnCombinaciones::String -> Interpretacion -> Bool
buscaEnCombinaciones a xs = head [y | (x, y) <- xs, x == a]

--genera todas las combinaciones posibles para las variables de la formula f (la parte izquierda de la tabla de verdad)
--asigna valores (True o False) para todas las variables de la formula
{- por ejemplo: 
     p     q
    True True
    True False
    False True
    False False
-}
combinaciones:: FProp -> [Interpretacion]
combinaciones f = auxiliar (vars f) 
              where auxiliar [] = [[]] 
                    auxiliar (v:vs) = [(v,True):i | i <- is] ++ [(v,False):i | i <- is]  
                      where is = auxiliar vs 

--una formula logica es una tautologia si es cierta para todas las combinaciones de sus variables
tautologia::FProp -> Bool
tautologia f = let interpretaciones = combinaciones f --obtiene las combinaciones de sus variables (parte izquierda de la tabla de verdad)
                   v = length interpretaciones
               in length [True | i <- interpretaciones, tautologia' i f == True] == v --Si hay tantos "True" como lineas en la tabla es tautologia

--dada una cierta combinacion de valores, o interpretacion, comprueba si es una tautologia o no
tautologia':: Interpretacion -> FProp -> Bool
tautologia' i (V a)  = buscaEnCombinaciones a i
tautologia' i (No a) = not(tautologia' i a)
tautologia' i (Y a b) = (tautologia' i a) && (tautologia' i b)
tautologia' i (O a b) = (tautologia' i a) || (tautologia' i b)
tautologia' i (Si a b) | (tautologia' i a) == False = True
                       | otherwise = (tautologia' i b)
tautologia' i (Sii a b) = (tautologia' i a) == (tautologia' i b)

--una formula es satisfactible si para algunas combinaciones, la formula es cierta, es decir, si en alguna fila de la tabla de verdad ha 
--resultado True
satisfactible::FProp -> Bool
satisfactible f = let interpretaciones = combinaciones f
               in length [True | i <- interpretaciones, tautologia' i f == True] > 0 

consecuencia::FProp -> FProp ->Bool --f' es consecuencia logica de f si f -> f'
consecuencia f f' = tautologia (Si f f')

equivalente::FProp -> FProp ->Bool --f' y f son equivalentes si f <-> f'
equivalente f f' = tautologia (Sii f f')

--dada una lista fs de fórmulas, consecuencias fs es una lista con cada fórmula
--f de fs emparejada con la lista de aquellas fórmulas de fs que son consecuencia lógica de f
consecuencias::[FProp] -> [(FProp, [FProp])]
consecuencias xs = [(x, [y | y <- xs, y /= x, consecuencia y x]) | x <- xs] --por ello esta lista intensional genera tuplas de (formula, [formula])
                                                                            --asi emparejamos cada formula con las que son consecuencia de ésta

--dada una lista fs de fórmulas, equivalentes fs es el conjunto cociente de fs por
--la relación de equivalencia lógica, es decir, es una partición de fs en sublistas, cada una de las
--cuales está formada por fórmulas de fs equivalentes entre sı́.
equivalentes::[FProp] ->[[FProp]]
equivalentes fs = [x:[y | y <- fs, y /= x, equivalente x y]| x <- fs] -- en este caso generamos una lista de listas con formulas equivalentes entre ellas