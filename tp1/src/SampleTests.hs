-- import Routes

----------------------------------- código de esqueleto.hs porque no funciona el import con Hugs  -------------------------------------------
import Data.List
import Data.Maybe
import Data.Char
import Test.HUnit
import Data.List (sort)
import Data.Maybe (fromJust, isNothing)

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
----------------------------------- código de esqueleto.hs porque no funciona el import con Hugs -------------------------------------------



rutasFacultad = many [
  route ""             "ver inicio",
  route "ayuda"        "ver ayuda",
  scope "materia/:nombre/alu/:lu" $ many [
    route "inscribir"   "inscribe alumno",
    route "aprobar"     "aprueba alumno"
  ],
  route "alu/:lu/aprobadas"  "ver materias aprobadas por alumno"
  ]

rutasStringOps = route "concat/:a/:b" (\ctx -> (get "a" ctx) ++ (get "b" ctx))

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
        "patterns" ~: testsPattern,
        "get" ~: testsGet,
        "matches" ~: testsMatches,
        "paths" ~: testsPaths,
        "eval" ~: testsEval,
        "evalWrap" ~: testsEvalWrap,
        "evalCtxt"~: testsEvalCtxt,
        "execEntity" ~: testsExecEntity,
        "catch_all" ~: testsCatchAll
        ]

splitSlash = split '/'

testsPattern = test [
		splitSlash "" ~=? [""],
        splitSlash "/" ~=? ["",""],
        splitSlash "//" ~=? ["","",""],
        splitSlash "/foo" ~=? ["", "foo"],
        splitSlash "/foo/bar/" ~=? ["", "foo","bar",""],
        pattern "" ~=? [],
        pattern "/" ~=? [],
        pattern "//////////////////" ~=? [],
        pattern "alu/:lu/aprobadas" ~=? [Literal "alu", Capture "lu", Literal "aprobadas"],
		pattern "/alu/:lu/aprobadas" ~=? [Literal "alu", Capture "lu", Literal "aprobadas"],
		pattern "/alu/:lu/aprobadas/" ~=? [Literal "alu", Capture "lu", Literal "aprobadas"],
		pattern "/alu/:lu//aprobadas/" ~=? [Literal "alu", Capture "lu", Literal "aprobadas"],
        pattern "lit1/:cap1/:cap2/lit2/:cap3" ~=? [Literal "lit1", Capture "cap1", Capture "cap2", Literal "lit2", Capture "cap3"]
        ]

testsGet = test [
		"plp" ~=? get "nombre" [("nombre","plp"), ("lu","007-01")],
		"007-01" ~=? get "lu" [("nombre","plp"), ("lu","007-01")]
		]

testsMatches = test [
        Just (["tpf"],[("nombreMateria","plp")]) ~=? matches (splitSlash "materias/plp/tpf") (pattern "materias/:nombreMateria"),
		Just (["alu", "007-1"],[("nombre","plp")]) ~=? matches ["materia","plp","alu","007-1"] [Literal "materia", Capture "nombre"],  
		Nothing ~=? matches ["otra","ruta"] [Literal "ayuda"],
		Nothing ~=? matches [] [Literal "algo"]
        ]

path0 = route "foo" 1
path1 = scope "foo" (route "bar" 2)
path2 = scope "foo" (route ":bar" 3)
path3 = scope "" $ scope "" $ many [ scope "" $ route "foo" 1]

testsEvalCtxt = test [
        Just (1, []) ~=? eval path0 "foo",
        Just (2, []) ~=? eval path1 "foo/bar",
        isNothing (eval path1 "foo/bla") ~? "",
        Just (3, [("bar", "bla")]) ~=? eval path2 "foo/bla",
        Just (1, []) ~=? eval path3 "foo"
        ]

path4 = many [
  (route "" 1),
  (route "lo/rem" 2),
  (route "ipsum" 3),
  (scope "folder" (many [
    (route "lorem" 4),
    (route "ipsum" 5)
    ]))
  ]


testsEval = test [
		Just ("ver materias aprobadas por alumno" ,[("lu","007-01")]) ~=? eval rutasFacultad "alu/007-01/aprobadas",
		Just ("aprueba alumno" ,[("lu","007-01") ,("nombre","plp")]) ~=? eval rutasFacultad "materia/plp/alu/007-01/aprobar",
		Nothing ~=? eval rutasFacultad "alu/007-01",
                1 ~=? justEvalP4 "",
                4 ~=? justEvalP4 "folder/lorem"
        ]
        where justEvalP4 s = fst (fromJust (eval path4 s))

path410 = wrap (+10) path4

testsEvalWrap = test [
		Just ("aduya rev" , []) ~=? eval (wrap reverse rutasFacultad) "ayuda",
                14 ~=? justEvalP410 "folder/lorem"
        ]
        where justEvalP410 s = fst (fromJust (eval path410 s))


-- ejempo donde el valor de cada ruta es una función que toma context como entrada.
-- para estos se puede usar en lugar además de eval, la función exec para devolver
-- la applicación de la función con sobre el contexto determinado por la ruta
rest entity = many [
  (route entity (const (entity++"#index"))),
  (scope (entity++"/:id") (many [
    (route "" (const (entity++"#show"))),
    (route "create" (\ctx -> entity ++ "#create of " ++ (get "id" ctx))),
    (route "update" (\ctx -> entity ++ "#update of " ++ (get "id" ctx))),
    (route "delete" (\ctx -> entity ++ "#delete of " ++ (get "id" ctx)))
    ]))
  ]

path5 = many [
  (route "" (const "root_url")),
  (rest "post"),
  (rest "category")
  ]

testsPaths = test [
		["","ayuda","materia/:nombre/alu/:lu/inscribir","materia/:nombre/alu/:lu/aprobar","alu/:lu/aprobadas"] ~=? paths rutasFacultad,
        sort ["","post","post/:id","post/:id/create","post/:id/update","post/:id/delete","category","category/:id","category/:id/create","category/:id/update","category/:id/delete"] ~=?
                sort (paths path5)
        ]


testsExecEntity = test [
		Just "foobar" ~=? exec rutasStringOps "concat/foo/bar",
		Just "foobar." ~=? exec (wrap (\f ctx -> f ctx ++ ".") rutasStringOps) "concat/foo/bar",
        Just "root_url" ~=? exec path5 "",
        Just "post#index" ~=? exec path5 "post",
        Just "post#show" ~=? exec path5 "post/35",
        Just "category#create of 7" ~=? exec path5 "category/7/create"
        ]
		
testsCatchAll = test [
		["",":p0",":p0/:p1",":p0/:p1/:p2",":p0/:p1/:p2/:p3"] ~=? take 5 (paths (catch_all 42)),
		Just (42,[("p0","cualquier") ,("p1","ruta")]) ~=? eval (catch_all 42) "cualquier/ruta",
		Just 0 ~=? exec (catch_all length) "",
		Just 3 ~=? exec (catch_all length) "foo/bar/baz"
		]
 
