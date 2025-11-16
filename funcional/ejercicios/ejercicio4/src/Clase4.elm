module Clase4 exposing (..)

{-| Ejercicios de Programación Funcional - Clase 4
Este módulo contiene ejercicios para practicar pattern matching y mónadas en Elm
usando árboles binarios como estructura de datos principal.

Temas:
- Pattern Matching con tipos algebraicos
- Mónada Maybe para operaciones opcionales
- Mónada Result para manejo de errores
- Composición monádica con andThen
-}
import String exposing (left)
import String exposing (right)
import Dict exposing (insert)
import Dict exposing (values)


-- ============================================================================
-- DEFINICIÓN DEL ÁRBOL BINARIO
-- ============================================================================

type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


-- ============================================================================
-- PARTE 0: CONSTRUCCIÓN DE ÁRBOLES
-- ============================================================================


-- 1. Crear Árboles de Ejemplo

arbolVacio : Tree Int
arbolVacio =
    Empty


arbolHoja : Tree Int
arbolHoja =
    Node 5 Empty Empty


arbolPequeno : Tree Int
arbolPequeno =
    Node 3 (Node 1 Empty Empty) (Node 5 Empty Empty)


arbolMediano : Tree Int
arbolMediano =
    Node 10
        (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))
        (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))


-- 2. Es Vacío


esVacio : Tree a -> Bool
esVacio arbol =
    case arbol of
        Empty -> True
        Node _ _ _ -> False -- _ _ _ quiere decir que va cualquier valor


-- 3. Es Hoja


esHoja : Tree a -> Bool
esHoja arbol =
    case arbol of
        Node _ Empty Empty -> True
        _ -> False


-- ============================================================================
-- PARTE 1: PATTERN MATCHING CON ÁRBOLES
-- ============================================================================


-- 4. Tamaño del Árbol


tamano: Tree a -> Int
tamano arbol =
    case arbol of
        Empty -> 0
        Node _ left right -> 1 + (tamano left) + (tamano right)



-- 5. Altura del Árbol


altura : Tree a -> Int
altura arbol =
    case arbol of
        Empty -> 0
        Node _ izq der -> 1 + (max (altura izq) (altura der))


-- 6. Suma de Valores


sumarArbol : Tree Int -> Int
sumarArbol arbol =
    case arbol of
        Empty -> 0
        Node valor left right -> valor + sumarArbol left + sumarArbol right


-- 7. Contiene Valor


contiene : a -> Tree a -> Bool
contiene valor arbol =
    case arbol of
        Empty -> False
        Node v left right -> 
            if v == valor then
                True
            else if contiene valor left then
               True
            else if contiene valor right then
                True
            else
                False


-- 8. Contar Hojas


contarHojas : Tree a -> Int
contarHojas arbol =
    case arbol of
        Empty -> 0
        Node _ Empty Empty -> 1
        Node _ izq der -> (contarHojas izq) + (contarHojas der)


-- 9. Valor Mínimo (sin Maybe)


minimo : Tree Int -> Int
minimo arbol =
    case arbol of
        Empty -> 0
        Node valor Empty Empty -> valor 
        Node valor left right -> min valor (min (minimo left) (minimo right))



-- 10. Valor Máximo (sin Maybe)


maximo : Tree Int -> Int
maximo arbol =
    case arbol of
        Empty -> 0
        Node valor Empty Empty -> valor 
        Node valor left right -> max valor (max (maximo left) (maximo right))


-- ============================================================================
-- PARTE 2: INTRODUCCIÓN A MAYBE
-- ============================================================================


-- 11. Buscar Valor


buscar : a -> Tree a -> Maybe a
buscar valor arbol =
    case arbol of
        Empty -> Nothing
        Node v left right ->
            if v == valor then 
                Just v
            else 
                case buscar valor left of
                    Just encontrado -> Just encontrado
                    Nothing -> buscar valor right


-- 12. Encontrar Mínimo (con Maybe)


encontrarMinimo : Tree Int -> Maybe Int
encontrarMinimo arbol =
    case arbol of
        Empty -> Nothing
        Node v Empty Empty -> Just v
        Node v left right ->
            case (encontrarMinimo left, encontrarMinimo right) of
                (Nothing, Nothing) -> Just v
                (Just minIzq, Nothing) -> Just (min v minIzq)
                (Nothing, Just minDer) -> Just (min v minDer)
                (Just minIzq, Just minDer) -> Just (min v (min minIzq minDer))


-- 13. Encontrar Máximo (con Maybe)


encontrarMaximo : Tree comparable -> Maybe comparable
encontrarMaximo arbol =
    case arbol of
        Empty -> Nothing
        Node v Empty Empty -> Just v
        Node v left right ->
            case (encontrarMaximo left, encontrarMaximo right) of
                (Nothing, Nothing) -> Just v
                (Just maxIzq, Nothing) -> Just (max v maxIzq)
                (Nothing, Just maxDer) -> Just (max v maxDer)
                (Just maxIzq, Just maxDer) -> Just (max v (max maxIzq maxDer))


-- 14. Buscar Por Predicado


buscarPor : (a -> Bool) -> Tree a -> Maybe a
buscarPor predicado arbol =
    case arbol of
        Empty -> Nothing
        Node v left right -> 
            if predicado v then
                Just v
            else 
                case buscarPor predicado left of
                    Just valor -> Just valor
                    Nothing -> buscarPor predicado right


-- 15. Obtener Valor de Raíz


raiz : Tree a -> Maybe a
raiz arbol =
    case arbol of
        Empty -> Nothing
        Node v _ _ -> Just v


-- 16. Obtener Hijo Izquierdo


hijoIzquierdo : Tree a -> Maybe (Tree a)
hijoIzquierdo arbol =
    case arbol of
        Empty -> Nothing
        Node _ Empty _ -> Nothing
        Node _ izq _ -> Just izq


hijoDerecho : Tree a -> Maybe (Tree a)
hijoDerecho arbol =
    case arbol of
        Empty -> Nothing
        Node _ _ Empty -> Nothing
        Node _ _ der -> Just der


-- 17. Obtener Nieto


nietoIzquierdoIzquierdo : Tree a -> Maybe (Tree a)
nietoIzquierdoIzquierdo arbol =
    Maybe.andThen hijoIzquierdo (hijoIzquierdo arbol)
    -- hace lo mismo
    -- case hijoIzquierdo arbol of
    --     Nothing -> Nothing
    --     Just tree -> hijoIzquierdo tree


-- 18. Buscar en Profundidad

obtenerSubarbol : a -> Tree a -> Maybe (Tree a)
obtenerSubarbol valor arbol =
    case arbol of
        Empty -> Nothing
        Node v left right ->
            if valor == v then
                Just arbol
            else
                case obtenerSubarbol valor left of
                    Just encontrado -> Just encontrado
                    Nothing -> obtenerSubarbol valor right



buscarEnSubarbol : a -> a -> Tree a -> Maybe a
buscarEnSubarbol valor1 valor2 arbol =
    case obtenerSubarbol valor1 arbol of
        Nothing -> Nothing
        Just subarbol -> buscar valor2 subarbol

-- ============================================================================
-- PARTE 3: RESULT PARA VALIDACIONES
-- ============================================================================


-- 19. Validar No Vacío


validarNoVacio : Tree a -> Result String (Tree a)
validarNoVacio arbol =
    case arbol of
        Empty -> Err "El árbol está vacío"
        Node _ _ _ -> Ok arbol


-- 20. Obtener Raíz con Error


obtenerRaiz : Tree a -> Result String a
obtenerRaiz arbol =
    case arbol of 
        Empty -> Err "No se puede obtener la raíz de un árbol vacío"
        Node v _ _ -> Ok v


-- 21. Dividir en Valor Raíz y Subárboles


dividir : Tree a -> Result String ( a, Tree a, Tree a )
dividir arbol =
    case arbol of
        Empty -> Err "No se puede dividir un árbol vacío"
        Node v left right -> Ok (v, left, right)


-- 22. Obtener Mínimo con Error


obtenerMinimo : Tree comparable -> Result String comparable
obtenerMinimo arbol =
    case arbol of 
        Empty -> Err "No hay mínimo en un árbol vacío"
        Node v left right -> 
            let
                minIzq = obtenerMinimo left
                minDer = obtenerMinimo right
            in
                case (minIzq, minDer) of
                    (Err _, Err _) -> Ok v
                    (Ok valorIzq, Err _) -> Ok (min v valorIzq)
                    (Err _, Ok valorDer) -> Ok (min v valorDer)
                    (Ok valorIzq, Ok valorDer) -> Ok (min v (min valorIzq valorDer))

-- 23. Verificar si es BST


esBST : Tree comparable -> Bool
esBST arbol =
    case arbol of
        Empty ->
            True

        Node v left right ->
            case (left, right) of
                (Empty, Empty) ->
                    True

                (Node vl _ _, Empty) ->
                    vl < v && esBST left

                (Empty, Node vr _ _) ->
                    vr > v && esBST right

                (Node vl _ _, Node vr _ _) ->
                    vl < v && vr > v && esBST left && esBST right



-- 24. Insertar en BST

insertarBST : comparable -> Tree comparable -> Result String (Tree comparable)
insertarBST valor arbol =
    case arbol of
        Empty -> Ok (Node valor Empty Empty)
        Node v left right ->
            if v == valor then 
                Err "El valor ya existe en el árbol"
            else if valor < v then
                case (insertarBST valor left) of
                    Err err -> Err err
                    Ok nuevoArbol -> Ok (Node v nuevoArbol right)
            else
                case (insertarBST valor right) of
                    Err err -> Err err
                    Ok nuevoArbol -> Ok (Node v left nuevoArbol)


-- 25. Buscar en BST

buscarEnBST : comparable -> Tree comparable -> Result String comparable
buscarEnBST valor arbol =
    case arbol of
        Empty -> Err "El valor no se encuentra en el árbol"
        Node v left right ->
            if v == valor then 
                Ok v
            else if valor < v then
                buscarEnBST valor left
            else
                buscarEnBST valor right


-- 26. Validar BST con Result


validarBST : Tree comparable -> Result String (Tree comparable)
validarBST arbol =
    Err "El árbol no es un BST válido"


-- ============================================================================
-- PARTE 4: COMBINANDO MAYBE Y RESULT
-- ============================================================================


-- 27. Maybe a Result


maybeAResult : String -> Maybe a -> Result String a
maybeAResult mensajeError maybe =
    case maybe of
        Nothing -> Err mensajeError
        Just valor -> Ok valor


-- 28. Result a Maybe


resultAMaybe : Result error value -> Maybe value
resultAMaybe result =
    case result of
        Err _ -> Nothing
        Ok value -> Just value


-- 29. Buscar y Validar

buscarPositivo: Int -> Tree Int -> Result String Int
buscarPositivo valor arbol =
    case arbol of
        Empty -> Err "El valor no se encuentra en el árbol"
        Node v left right ->
            if v == valor then Ok v
            else
                case (buscarPositivo valor left) of
                    Err _ -> buscarPositivo valor right
                    Ok vlr -> Ok vlr



-- 30. Pipeline de Validaciones


validarArbol : Tree Int -> Result String (Tree Int)
validarArbol arbol =
    Err "Validación fallida"


-- 31. Encadenar Búsquedas


buscarEnDosArboles : Int -> Tree Int -> Tree Int -> Result String Int
buscarEnDosArboles valor arbol1 arbol2 =
    Err "Búsqueda fallida"


-- ============================================================================
-- PARTE 5: DESAFÍOS AVANZADOS
-- ============================================================================


-- 32. Recorrido Inorder


inorder : Tree a -> List a
inorder arbol =
    case arbol of
        Empty -> []
        Node v left right ->
            (inorder left)++v::(inorder right)


-- 33. Recorrido Preorder


preorder : Tree a -> List a
preorder arbol =
    case arbol of
        Empty -> []
        Node v left right ->
            v::(inorder left)++(inorder right)


-- 34. Recorrido Postorder


postorder : Tree a -> List a
postorder arbol =
    case arbol of
        Empty -> []
        Node v left right ->
            (inorder left)++(inorder right)++[v]


-- 35. Map sobre Árbol


mapArbol : (a -> b) -> Tree a -> Tree b
mapArbol funcion arbol =
    case arbol of
        Empty -> Empty
        Node v left right ->
            Node (funcion v) (mapArbol funcion left) (mapArbol funcion right)


-- 36. Filter sobre Árbol


filterArbol : (a -> Bool) -> Tree a -> Tree a
filterArbol predicado arbol =
    Empty


-- 37. Fold sobre Árbol


foldArbol : (a -> b -> b) -> b -> Tree a -> b
foldArbol funcion acumulador arbol =
    case arbol of
        Empty -> acumulador
        Node v left right ->
            foldArbol funcion (funcion v (foldArbol funcion acumulador left)) right


-- 38. Eliminar de BST


eliminarBST : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarBST valor arbol =
    Err "El valor no existe en el árbol"


-- 39. Construir BST desde Lista


desdeListaBST : List comparable -> Result String (Tree comparable)
desdeListaBST lista =
    Err "Valor duplicado"


-- 40. Verificar Balance


estaBalanceado : Tree a -> Bool
estaBalanceado arbol =
    case arbol of
        Empty -> True
        Node _ left right ->
            let
              diferenciaAlturas = abs((altura left) - (altura right))  
            in
            if (diferenciaAlturas <= 1 && estaBalanceado left && estaBalanceado right) then
                True
            else
                False

-- 41. Balancear BST


balancear : Tree comparable -> Tree comparable
balancear arbol =
    Empty


-- 42. Camino a un Valor


type Direccion
    = Izquierda
    | Derecha

-- dfs :  Tree a ->  List a
-- dfs arbol =
--     case arbol of
--         Empty -> []
--         Node v left right ->
--             case left of ->
--                 Empty ->
--                     v::(dfs right)
--                 Node vLeft lLeft lRight ->
--                     v::vLeft::(dfs left)



encontrarCamino : a -> Tree a -> Result String (List Direccion)
encontrarCamino valor arbol =
    case arbol of
        Empty -> Err "El valor no existe en el árbol"
        Node v left right ->
            if v == valor then
                Ok []
            else
                case (encontrarCamino valor left) of
                    Err _ -> 
                        case (encontrarCamino valor right) of
                            Err errDer -> Err errDer
                            Ok caminoIzq -> Ok (Derecha::caminoIzq)
                    Ok caminoDer -> Ok (Izquierda::caminoDer)



-- 43. Seguir Camino


seguirCamino : List Direccion -> Tree a -> Result String a
seguirCamino camino arbol =
    case (camino, arbol) of
        ([], Empty) -> Err "Camino inválido"
        ([], Node v _ _) -> Ok v
        (head::tail, Node _ left right) -> 
            if head == Izquierda then
                seguirCamino tail left
            else
                seguirCamino tail right
        (_::_, Empty) -> Err "Camino inválido"
    -- case camino of
    --     [] -> Err "Finalizado"
    --     head::tail -> 
    --         if head == Izquierda then
    --             case arbol of
    --                 Empty -> Err "Camino inválido"
    --                 Node _ left _ ->
    --                     case (seguirCamino tail left) of
    --                         Err err -> 
    --                             if err == "Finalizado" then
    --                                 -- conseguir head de left
    --                                 case left of
    --                                     Empty -> Err "Camino inválido"
    --                                     Node v _ _ -> Ok v
    --                             else
    --                                 Err err
    --                         Ok valor -> Ok valor
    --         else
    --             case arbol of
    --                 Empty -> Err "Camino inválido"
    --                 Node _ _ right ->
    --                     case (seguirCamino tail right) of
    --                         Err err -> 
    --                             if err == "Finalizado" then
    --                                 -- conseguir head de left
    --                                 case right of
    --                                     Empty -> Err "Camino inválido"
    --                                     Node v _ _ -> Ok v
    --                             else
    --                                 Err err
    --                         Ok valor -> Ok valor   


-- 44. Ancestro Común Más Cercano


ancestroComun : comparable -> comparable -> Tree comparable -> Result String comparable
ancestroComun valor1 valor2 arbol =
    Err "Uno o ambos valores no existen en el árbol"


-- ============================================================================
-- PARTE 6: DESAFÍO FINAL - SISTEMA COMPLETO
-- ============================================================================


-- 45. Sistema Completo de BST
-- (Las funciones individuales ya están definidas arriba)


-- Operaciones que retornan Bool
-- esBSTValido : Tree comparable -> Bool
-- esBSTValido arbol =
--     esBST arbol


-- estaBalanceadoCompleto : Tree comparable -> Bool
-- estaBalanceadoCompleto arbol =
--     estaBalanceado arbol


-- contieneValor : comparable -> Tree comparable -> Bool
-- contieneValor valor arbol =
--     contiene valor arbol


-- -- Operaciones que retornan Maybe
-- buscarMaybe : comparable -> Tree comparable -> Maybe comparable
-- buscarMaybe valor arbol =
--     buscar valor arbol


-- encontrarMinimoMaybe : Tree comparable -> Maybe comparable
-- encontrarMinimoMaybe arbol =
--     encontrarMinimo 


-- encontrarMaximoMaybe : Tree comparable -> Maybe comparable
-- encontrarMaximoMaybe arbol =
--     encontrarMaximo arbol


-- -- Operaciones que retornan Result
-- insertarResult : comparable -> Tree comparable -> Result String (Tree comparable)
-- insertarResult valor arbol =
--     insertarBST valor arbol


-- eliminarResult : comparable -> Tree comparable -> Result String (Tree comparable)
-- eliminarResult valor arbol =
--     eliminarBST valor arbol


-- validarResult : Tree comparable -> Result String (Tree comparable)
-- validarResult arbol =
--     validarBST arbol


-- obtenerEnPosicion : Int -> Tree comparable -> Result String comparable
-- obtenerEnPosicion posicion arbol =
--     Err "Posición inválida"


-- -- Operaciones de transformación
-- map : (a -> b) -> Tree a -> Tree b
-- map funcion arbol =
--     mapArbol funcion arbol


-- filter : (a -> Bool) -> Tree a -> Tree a
-- filter predicado arbol =
--     filterArbol predicado arbol


-- fold : (a -> b -> b) -> b -> Tree a -> b
-- fold funcion acumulador arbol =
--     foldArbol funcion acumulador arbol


-- -- Conversiones
-- aLista : Tree a -> List a
-- aLista arbol =
--     inorder arbol


-- desdeListaBalanceada : List comparable -> Tree comparable
-- desdeListaBalanceada lista =
--     Empty
