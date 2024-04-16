---
# Laboratorio de Funcional
Integrantes: 

 - Renzo Condormango
 - Simón Celoria
 - Francisco Jose Porcel de Peralta

Consigna:https://tinyurl.com/funcional-2024-famaf

# 1. Tareas

## Verificación de que pueden hacer las cosas.
- [x] Haskell instalado y testeos provistos funcionando. (En Install.md están las instrucciones para instalar.)

## 1.1. Lenguaje
- [x] Módulo `Dibujo.hs` con el tipo `Dibujo` y combinadores. Puntos 1 a 3 de la consigna.
- [x] Definición de funciones (esquemas) para la manipulación de dibujos.
- [x] Módulo `Pred.hs`. Punto extra si definen predicados para transformaciones innecesarias (por ejemplo, espejar dos veces es la identidad).

## 1.2. Interpretación geométrica
- [x] Módulo `Interp.hs`.

## 1.3. Expresión artística (Utilizar el lenguaje)
- [x] El dibujo de `Dibujos/Feo.hs` se ve lindo.
- [x] Módulo `Dibujos/Grilla.hs`.
- [x] Módulo `Dibujos/Escher.hs`.
- [x] Listado de dibujos en `Main.hs`.

## 1.4 Tests
- [x] Tests para `Dibujo.hs`.
- [x] Tests para `Pred.hs`.

# 2. Experiencia
## 2.4 Tests
### Creación de los tests:
Empezamos creando los tests con copilot sugiriéndole que utilice la librería **HUnit** de haskell, nos escribió algo mas o menos así: 
```Haskell
import Test.HUnit
import Dibujo

-- Test for function1
function1Test :: Test
function1Test = TestCase $ do
  let result = function1 "test data"
  assertEqual "for (function1 \"test data\")," "expected result" result

-- Test for function2
function2Test :: Test
function2Test = TestCase $ do
  let result = function2 "test data"
  assertEqual "for (function2 \"test data\")," "expected result" result

-- Add more tests for other functions here...

-- Test list
tests :: Test
tests = TestList [TestLabel "function1Test" function1Test, TestLabel "function2Test" function2Test]

-- Main function
main :: IO Counts
main = runTestTT tests
```
El código que nos proporciono nos dio una idea muy cercana de como funcionaba la estructura de unit testing en haskell, dándonos la idea de que hay que hacer funciones separadas para cada test y luego ejecutar una lista de test en una función main.
### Cambiamos la estructura del main:
A partir de este punto investigamos como mejorar el main ya que no funcionaba correctamente cuando agregábamos mas test, el resultado de la ejecución siempre era el mismo.

Cambiamos el main de la siguiente forma:
``` Haskell

tests :: Test
tests =
                TestList
                    [ -- Lista de tests
                    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
```
Lo mas importante en este cambio es que guardamos el resultado de **runTestTT test** en la variable result (Podríamos mejorar el nombre de esta variable), luego si existió alguna falla retornamos error con **Exit.exitFailure** y si todos los test son pasados correctamente salimos exitosamente del programa.
### ¿Como sabemos si un test fallo?¿Cual es la estructura de los test unitarios?
La estructura de cada test tanto en las pruebas para predicados como para la semántica y sintaxis del lenguaje es la siguiente:
```Haskell
testX :: Test
testX =
    TestCase $ assertEqual "X" (--Funcion a testear)
                 --Resultado esperado
```
En donde el tipo de la función es **Test** que viene incluido en la librería HUnit. Luego  **TestCase** toma una función (o expresión) que se va a probar y retorna un valor de verificación (o aserción) que indica si la prueba pasó o falló. Aca es donde **assertEqueal** verifica que la primera expresión sea igual a la segunda. Con esta estructura es que construimos todos los test tanto en TestDibujo.hs como en TestPred.hs.

### Tests más interesantes:
Los test mas relevantes en esta lista son:
```Haskell

testfoldDib :: Test
testfoldDib =
  TestCase $ assertEqual
    "foldDib"
    1
      (foldDib
        (const 1)
        id
        id
        id
        (\_ _ x y -> x * y)
        (\_ _ x y -> x * y)
        (+)
        (apilar 1.0 1.0 
          (juntar 1.0 1.0 (figura Triangulo) (figura Rectangulo)) 
          (rotar (rotar (figura Rectangulo)))
        )
      )
```
Por un lado el test de **foldDib** ademas de testear que efectivamente funcione correctamente, prueba otra utilidad de la función que es contar la cantidad de figuras que hay en el dibujo, ademas también de poder otorgarle a foldDib una lista rápida de funciones mostrando la gran flexibilidad de esta función en cuanto a tipos.

Y por otro lado tenemos el test con el que comprobamos que:
* *map figura = id*
* *map (g . f) = mapDib g . mapDib f*
```Haskell
-- Test de identidad en Dibujo.hs
testId :: Test
testId =
    TestCase $ assertEqual "id" (show $ mapDib id (rotar (espejar (figura 1)))) "Rotar (Espejar (Figura 1))"
```
### Conclusión.
Aprendimos rápidamente y por encima como hacer unit testing en haskell para asegurarnos que funcionen de forma independiente cada uno de los módulos que fuimos creando e implementando durante el laboratorio.
# 3. Preguntas
Al responder tranformar cada pregunta en una subsección para que sea más fácil de leer.

1. ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.
2. ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?
3. ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?
4. ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?

# 4. Extras
Completar si hacen algo.

