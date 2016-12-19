module Routes where

import Data.List
import Data.Maybe
import Data.Char

data PathPattern = Literal String | Capture String deriving (Eq, Show)

data Routes f = Route [PathPattern] f | Scope [PathPattern] (Routes f) | Many [Routes f] deriving Show

-- Ejercicio 1: Dado un elemento separador y una lista, se deber a partir la lista en sublistas de acuerdo a la aparicíon del separador (sin incluirlo).

split :: Eq a => a -> [a] -> [[a]]				 
split c = foldr (\x r -> if x == c then [] : r else (x : head r) : tail r) [[]]

-- Ejercicio 2: A partir de una cadena que denota un patrón de URL se deberá construir la secuencia de literales y capturas correspondiente.

pattern :: String -> [PathPattern]
pattern s = map getCaptureOrLiteral onlyPathPatternsList
	where
		getCaptureOrLiteral = (\x -> if head x == ':' then Capture (tail x) else Literal x)
		onlyPathPatternsList = filter (/= "") (split '/' s)
	

-- Ejercicio 3: Obtiene el valor registrado en una captura determinada. Se puede suponer que la captura está definida en el contexto.
type PathContext = [(String, String)]

get :: String -> PathContext -> String
get s = foldr (\x r -> if fst x == s then snd x else r) "" 

-- Ejercicio 4: Dadas una ruta particionada y un patrón de URL, trata de aplicar el patrón a la ruta y devuelve, en caso de que
--              la ruta sea un prefijo válido para el patrón, el resto de la ruta que no se haya llegado a consumir y el contexto capturado hasta el punto alcanzado.
-- Se puede usar recursión explícita.

matches :: [String] -> [PathPattern] -> Maybe ([String], PathContext)
matches [] [] = Just ([], [])
matches [] (pp:pps) = Nothing
matches (s:ss) [] = Just ((s:ss), [])
matches (s:ss) (pp:pps) = case pp of
							Literal l -> if l == s then matches ss pps else Nothing
							Capture c -> case (matches ss pps) of
								Nothing -> Nothing
								Just (ls, pc) -> Just (ls, pc ++ [(c,s)])


-- DSL para rutas
route :: String -> a -> Routes a
route s f = Route (pattern s) f

scope :: String -> Routes a -> Routes a
scope s r = Scope (pattern s) r

many :: [Routes a] -> Routes a
many l = Many l

-- Ejercicio 5: Definir el fold para el tipo Routes f y su tipo. Se puede usar recursión explícita.

foldRoutes :: ([b] -> b) -> ([PathPattern] -> b -> b) -> ([PathPattern] -> f -> b) -> Routes f -> b									
foldRoutes fMany fScope fRoute r = case r of
									Route pps f -> fRoute pps f
									Scope pps rr -> fScope pps (recu rr)
									Many rrs -> fMany (map (recu) rrs)
									where recu = foldRoutes fMany fScope fRoute

-- Auxiliar para mostrar patrones. Es la inversa de pattern.
patternShow :: [PathPattern] -> String
patternShow ps = concat $ intersperse "/" ((map (\p -> case p of
  Literal s -> s
  Capture s -> (':':s)
  )) ps)

-- Ejercicio 6: Genera todos los posibles paths para una ruta definida.

paths :: Routes a -> [String]
paths = foldRoutes (concat) 
				   (\pps r -> map (\x -> if null x 
									     then ((patternShow pps) ++ x)
										 else (patternShow pps) ++ ['/'] ++ x) r)
				   (\pps _ -> [patternShow pps])

-- Ejercicio 7: Evalúa un path con una definición de ruta y, en caso de haber coincidencia, obtiene el handler correspondiente 
--              y el contexto de capturas obtenido.
{-
Nota: la siguiente función viene definida en el módulo Data.Maybe.
 (=<<) :: (a->Maybe b)->Maybe a->Maybe b
 f =<< m = case m of Nothing -> Nothing; Just x -> f x
-}

splitToStringSlash :: [String] -> String
splitToStringSlash [] = ""
splitToStringSlash [x] = x ++ splitToStringSlash []
splitToStringSlash (x:xs) = x ++ "/" ++ splitToStringSlash xs
  
eval :: Routes a -> String -> Maybe (a, PathContext)
eval = foldRoutes 	(\rs -> \s -> if null (cleanNothings (map (\f -> f s) rs))
							then Nothing
							else head (cleanNothings (map (\f -> f s) rs)))
					(\pps r -> \s -> case (matches (split '/' s) pps) of
										Nothing -> Nothing
										Just (ls, pcs) -> case (r (splitToStringSlash ls)) of
															Nothing -> Nothing
															Just (a, pc) -> Just (a, pcs ++ pc))
					(\pps f -> \s -> case (matches (split '/' s) pps) of
										Nothing -> Nothing
										Just (ls, pcs) -> if ( (null pcs && null ls) || ls == [""] || not (null pcs) )
														  then Just (f, pcs)
														  else Nothing)
  
cleanNothings :: [Maybe (a, PathContext)] -> [Maybe (a, PathContext)]
cleanNothings = foldr (\x r -> case x of
								Nothing -> r
								Just (a, pc) -> x : r) []


-- Ejercicio 8: Similar a eval, pero aquí se espera que el handler sea una función que recibe como entrada el contexto 
--              con las capturas, por lo que se devolverá el resultado de su aplicación, en caso de haber coincidencia.

exec :: Routes (PathContext -> a) -> String -> Maybe a
exec routes path = case eval routes path of
                    Nothing -> Nothing
                    Just (h, pc) -> Just (h pc)

-- Ejercicio 9: Permite aplicar una funcion sobre el handler de una ruta. Esto, por ejemplo, podría permitir la ejecución 
--              concatenada de dos o más handlers.
wrap :: (a -> b) -> Routes a -> Routes b
wrap f = foldRoutes (\rs -> Many rs)
					(\pps r -> Scope pps r)
					(\pps handler -> Route pps (f handler))

-- Ejercicio 10: Genera un Routes que captura todas las rutas, de cualquier longitud. A todos los patrones devuelven el mismo valor. 
--               Las capturas usadas en los patrones se deberán llamar p0, p1, etc. 
--               En este punto se permite recursión explícita.
-- exec (catch_all length) "" da 0 en el pdf y a nosotros Just 0. Creemos que está bien el nuestro
catch_all :: a -> Routes a
catch_all h = dameRoute 0 h

dameRoute n h = many [route "" h, scope (":p" ++ numerosAString n) (dameRoute (n+1) h)]

numerosAString :: Int -> String
numerosAString n = if n >= 10 then numerosAString (n `div` 10) ++ numerosAString (n `mod` 10) else [chr $ 48+n]