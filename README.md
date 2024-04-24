

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
# Lenguaje
### ¿Por qué usamos haskell para esta tarea en particular?
* **Expresividad:** Es un lenguaje de programación funcional que ofrece una sintaxis limpia y expresiva, lo que facilita la creación de abstracciones y la manipulación de datos. Esto puede ser útil al definir operaciones sobre dibujos y composiciones de manera clara y concisa.
*   **Inmutabilidad y transparencia referencial**: Los datos son inmutables por defecto y las funciones son puras, por lo que no tienen efectos secundarios. Esto hace que el diseño y la manipulación de dibujos sea más seguros y menos propensos a errores, ya que se evitan efectos colaterales inesperados.
* **Abstracciones de alto nivel**: Haskell ofrece poderosas herramientas para abstraer patrones comunes y escribir código genérico y reutilizable. Esto puede ser útil al definir funciones que operan en diferentes tipos de dibujos o al implementar algoritmos de transformación y renderizado.
* **Tipado fuerte**: Ofrece todas las ventajas de esta característica de los lenguajes, una gran prevención de errores de tipos.

En resumen, Haskell ofrece un conjunto de características que pueden hacer que la creación de un lenguaje para dibujar y representar dibujos en pantalla sea más eficiente, seguro y expresivo. Su naturaleza funcional y las herramientas disponibles en el ecosistema de Haskell pueden simplificar el diseño y la implementación de un lenguaje de este tipo.

# Sintaxis
## Modulo dibujo

### ¿Que es el modulo dibujo?

El modulo dibujo es el encargado de crear la sintaxis de nuestro lenguaje para la construcción de dibujos, es decir, es un conjunto de reglas y estructuras que nos dicen como se puede escribir y construir en nuestro lenguaje en particular.

La sintaxis define la forma correcta en la que se deben combinar las palabras clave, operadores, identificadores y otros elementos del lenguaje para crear resultados válidos.
Es lo que sienta las bases para lograr comprender el lenguaje de forma correcta y hacerlo solido para evitar errores. 

Para esto hacemos uso de las siguientes funciones:

### Declaración del tipo dibujo

En nuestro caso declaramos el tipo dibujo de forma recursiva de la siguiente forma:
``` haskell
data Dibujo a = Figura a | Rotar (Dibujo a) 
              | Espejar (Dibujo a) | Rot45 (Dibujo a) 
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
              deriving(Eq,Show)
```
El tipo básico es **Figura a** donde puede adquirir cualquier tipo de figura primitiva como cuadrado, circulo, etc. el tipo **a** es un parámetro de tipo que indica el tipo de datos de la figura.

Después tenemos varios constructores que toman como parámetro un dibujo y representan la respectiva accion sobre ese dibujo donde retornan el dibujo con la accion aplicada, por ejemplo Rotar (Dibujo a) devuelve la rotación del Dibujo a.
``` haskell
deriving(Eq,Show)
```
Luego la declaración `deriving (Eq, Show)` al final permite que el tipo `Dibujo a` sea comparable para la igualdad (`Eq`) y que pueda ser convertido a una representación de cadena (`Show`) automáticamente por el compilador. Esto es útil para poder imprimir y depurar valores de tipo `Dibujo a`.

### Funciones constructoras:
Estas funciones constructoras son simplemente funciones auxiliares que facilitan la creación y combinación de dibujos utilizando los constructores del tipo de dato `Dibujo a` algunas son:

-   `figura :: a -> Dibujo a`: Esta función constructora crea una figura básica del tipo `a` y la envuelve en el constructor `Figura`. Simplifica la creación de figuras básicas.
-  encimar :: Dibujo a -> Dibujo a -> Dibujo a`: Esta función constructora toma dos dibujos y los superpone uno encima del otro utilizando el constructor `Encimar`. Es una forma conveniente de combinar dos dibujos en una superposición.
    
-   `apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a`: Esta función constructora toma dos valores `Float` que indican la proporción de apilamiento vertical y dos dibujos, y los combina en una disposición vertical utilizando el constructor `Apilar`.
    
-   `juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a`: Similar a `apilar`, esta función constructora toma dos valores `Float` que indican la proporción de juntado horizontal y dos dibujos, y los combina en una disposición horizontal utilizando el constructor `Juntar`.  


Y asi con varias funciones.
### A continuación vamos a ver la implementación de las tres funciones mas complicadas en el modulo dibujo que usan implementaciones anteriores del mismo modulo.
### Función Foldib. Mapdib, Change:
Gran parte del entendimiento de las siguientes funciones vino a raíz de pensar al Dibujo como una estructura de árbol, en donde las hojas son las figuras.

**mapDib**  

En el caso de la función `mapDib` es una implementación de la función `map` para el tipo de datos `Dibujo`, `map` se usa comúnmente para aplicar una función a cada elemento de una estructura de datos, en este caso, `Dibujo`.

La firma de la función es:
```haskell
mapDib :: (a ->  b) -> Dibujo a -> Dibujo b
```

Toma una función de a -> b y un dibujo a y nos retorna un dibujo de tipo b. Es decir convierte los elementos de tipo a en el dibujo y los convierte en elementos de tipo b, resultando en un dibujo de tipo b.

Ahora veamos la implementación:
-   Si el dibujo es una figura (`Figura dib`), aplica la función `f` al contenido de la figura (`dib`), y envuelve el resultado en una nueva figura.
 ``` haskell
mapDib f (Figura dib) = Figura (f dib)
```
-   Si el dibujo es una rotación (`Rotar dib`), aplica recursivamente `mapDib f` al dibujo contenido en la rotación.
 ``` haskell
mapDib f (Rotar dib) = Rotar (mapDib f dib)
```
Y así sucesivamente en cada caso, vamos aplicando la función al dibujo contenido de la transformación (Rotación, espejado, encimado, etc.) obteniendo así el mapeo del dibujo con cada transformación aplicada respectivamente a cada elemento del dibujo.

**change**

En este caso la función change se encarga de cambiar todas las figuras básicas de un dibujo por otra, para esto usa una implementación similar a mapDib donde va cambiando cada figura del dibujo.

La firma de la función es:

``` haskell
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
```
Toma una función que toma un a y retorna un Dibujo b y un dibujo b, retornando un Dibujo b,.
La principal y única diferencia entre change y mapDib se encuentra en el primer caso:
``` haskell
change f (Figura dib) = f dib
```
En esta línea, si el dibujo es una figura (`Figura dib`), aplica la función `f` directamente al contenido de la figura (`dib`). En `mapDib`, se envuelve el resultado en una nueva figura, aquí se retorna directamente el resultado de `f dib`.

De esta forma cambia permanentemente la forma del dibujo a por un dibujo b.

**foldDib**

Para simplificar el entendimiento de la implementación y de la función en si vamos a resumir la explicacion de esta función en que esta función es la encargada de aplicar la recursion al dibujo en si, aplicando todas y cada una de las funciones en cada caso respectivamente.

La firma de la función es un poco complicada:
```haskell
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
```
Toma 7 funciones. un dibujo a y nos retorna un tipo b, donde este tipo b en si es el colapso de aplicar todas las funciones del dibujo a.

Podemos pensarlo como dibujo a  es **Rotar ( Rotar ( Figura a))** foldDib lo que va a hacer es aplicar efectivamente la interpretación de Rotar sobre Rotar y aplicar la rotación a la interpretación de la Figura a. Resultando por ejemplo en los vectores necesarios para imprimir la figura rotada dos veces.

¿Como hace esto nuestra funcion?

Veamos un solo caso:
```haskell
foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc (Rotar a) = fRotar (foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc a)
```
Lo que vemos es que foldDib aplica la funcion fRotar (Distinta de la función Rotar) que seria la interpretación sobre la recursión de foldDib, aplicando así efectivamente la interpretación de la rotación.

# Semántica
## Modulo Interp
El módulo Interp sería el encargado de darle una interpretación a nuestro constructor Dibujo, viendolo del lado de un lenguaje
sería la parte relacionada a la semántica. Acá declaramos distintas funciones para poder darle una interpretación a las funciones 
que declaramos en Dibujos.hs.
Por ejemplo en Dibujos.hs tenemos declarado rotar, pero esto es solo una sintaxis que usamos, como podemos decirle a la máquina
que tiene que rotar un dibujo?

```haskell
rotar :: Dibujo a -> Dibujo a
rotar = Rotar
```
Pues para eso está el Interp.hs

```haskell
rot :: FloatingPic -> FloatingPic
rot fp d w h= fp (d V.+ w) h ( V.negate w)
```
declaramos una función equivalente al rotar para que la máquina luego sepa que cuando se encuentre con un dibujo del tipo
--> Rotar (Figura Basica)
tendría que aplicarle rot a esta figura básica

Hacemos uso de operadores que nos dá la librería Gloss para poder operar con vectores y así devolver también algo del tipo Picture
para que luego se pueda mostrar en pantalla mediante el uso de esta librería.

Estas funciones como **rot**, **esp**, **sup**, etc que están definidas en Interp.hs serían como auxiliares. La función que utilizamos
realmente para interpretar un dibujo está definido como:

```haskell
interp :: Output a -> Output (Dibujo a)
interp funcBase = foldDib funcBase rot esp r45 api jun sup
```
el cual funcBase sería una función para interpretación de dibujos básicos que podemos definir nosotros, para poder nosotros mismos
definir nuestras figuras básicas y como se deben interpretar estas. Luego **rot**, **esp**, **sup**, etc serían las demás funciones
que utilizamos junto con foldDib para poder interpretar correctamente la figura.

**Funcionamiento:**
Si a interp se le pasa una funcBase y un Dibujo tipo Apilar 1 1 (Rotar (Figura Cuadrado)) (Rotar (Figura Triangulo))

(teniendo en cuenta que funcBase esté definido de forma tal que puede interpretar un Cuadrado y un Triangulo)

```haskell
interp funcBase (Apilar 1 1 (Rotar (Figura Cuadrado)) (Rotar (Figura Triangulo)))
```
interp devolvería:

```haskell
api 1 1 (rot (funcBase Cuadrado)) (rot (funcBase Triangulo))
```

Lo cual se vería en pantalla como un cuadrado rotado 90° apilado sobre un triangulo rotado 90°

# Testeo
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

# Conclusión.
Aprendimos rápidamente y por encima como hacer unit testing en haskell para asegurarnos que funcionen de forma independiente cada uno de los módulos que fuimos creando e implementando durante el laboratorio.
# 3. Preguntas

# ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.
Las funcionalidades se dividen en distintos módulos para poder tener una organización y poder producir código legible, con una estructura clara y que no se mezclen
tares. De esta forma vamos a poder tener una mejor comprensión y en caso de que hayan errores será más fácil de debuggear ya que el error lo vamos a poder ver
dependiendo de si es un error de Sintaxis (Dibujos.hs) o Semántica (Interp.hs)
A su vez, tener las funcionalidades dividas en módulos nos puede servir para reutilizar código, poder exportar los distintos módulos en diferentes archivos
y así poder usarlos sin tener que pensar mucho en como están definidos en la mayoría de casos.

## Modulo Dibujo.hs
Las funcionalidades de dibujo.hs y su rol estan detalladas en la parte de experiencia.

## Modulo Pred.hs
El módulo Pred.hs no es utilizado en general durante el proyecto, sin embargo creemos que podría servir para poder hacer cambios específicos sobre dibujos
que cumplan cierta condición (esto mediante el uso de la función `cambiar`).
También se podría usar para asegurarnos de que se cumplan ciertas restricciones sobre nuestros dibujos. (como utilizarlos para similar asserts)

## Modulo Interp.hs
Las funcionalidades de Interp.hs y su rol estan detalladas en la parte de experiencia.
# ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?

### Gran Adaptabilidad 
Al implementar el lenguaje de forma que no sea necesario definir que es una `figura` y cuáles serán usadas en particular, permite al lenguaje adaptarse sin problemas a las necesidades del usuario.
El que la implementación de las funciones no dependa de como este definido el tipo `figura`, nos otorga diversas ventajas. 

#### Flexibilidad
Una ventaja que ofrece está implementación es que se puede definir figura de forma ad hoc en cada dibujo con el tipo que resulte más útil en ese caso. Así se puede, por ejemplo, usar una tupla conteniendo el nombre de una forma y un color: `(BasicaSinColor, Color)` o incluso un booleano.  Permitiendo así que el lenguaje se adapte a la idea del usuario en lugar de al revés

#### Variedad
Otra ventaja es que, de esta forma, no limita la creatividad. Esto es así puesto que al no venir predefinidas cuáles son las formas básicas que uno puede usar, permite a cada uno crear las formas que uno desee sin limitaciones de cantidad o complejidad.

#### Ejemplo:
A modo de ejemplo, nosotros hacemos uso de esta flexibilidad en nuestros dibujos, pudiendo usar más o menos colores y solo describiendo aquellas formas básicas que se necesitan para ese dibujo en particular. Además de poder dar uso sin problemas a todo el repertorio de Gloss, como el uso de Polygon en lugar de line para crear figuras rellenas o ThickCircles para crear círculos con grosor, todas estas intercambiables unas con otras, gracias a que Figura es una variable de tipo

#  ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?

## La funcion **FoldDib** que nosotros implementamos tiene una serie de ventajas sobre hacer patter-matching directo
### Mayor flexibilidad
Al permitir la función `foldDib` introducir funciones como parámetros, esto nos permite una flexibilidad enorme a la hora de usar tipos en esta función, como se puede observar en los test las funciones pasadas a `foldDib` nada tienen que ver con dibujos y aun así su comportamiento es el mismo que si el tipo fuera Dibujo o Figura.

De esta forma al ser `funcbase` una de las funciones pasadas a la función esta se adapta a la función base de cada contexto para poder ser utilizada correctamente.

### Separacion de preocupaciones
El pattern matching directo puede hacer que las funciones que operan sobre tipos de datos recursivos se vuelvan largas y difíciles de mantener. Al usar `foldDib`, puedes separar claramente la lógica de manipulación de la estructura de datos de la lógica específica de cada operación, lo que facilita el mantenimiento y la comprensión del código.

### Polimorfismo
`foldDib` es una forma de lograr polimorfismo paramétrico sobre el tipo `Dibujo`. Esto significa que las funciones de plegado pueden trabajar con cualquier tipo de dibujo (`Dibujo a`), siempre que se proporcionen las funciones necesarias para manejar cada caso.

### En resumen:
`foldDib` proporciona una forma elegante y flexible de trabajar con tipos de datos recursivos, también proporciona una buena imagen visual de su comportamiento si interpretamos un dibujo como un árbol y a `foldDib` como el colapsamiento de ese árbol en su nodo. De la misma forma podríamos ver `mapDib` como un rastreo y modificación de cada nodo de ese árbol, lo que facilita el entendimiento de todo el código y nuestra estructura.
# ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?

## Pred.hs
En Pred se implementan funciones realizar verificaciones de predicados sobre cualquier dibujo.
Estas funciones se podrían utilizar para realizar alguna acción en base a si se cumple una condición o no.
## Tests
Por el otro lado, tests lo único que busca es verificar que nuestras funciones tengan el correcto funcionamiento.

# 4. Extras
### Investigacion
A modo de ejercicio extra consideramos interesante la idea de añadir nuevos dibujos además de Escher. Es así que exploramos la posibilidad de representar fractales.
Comenzamos buscando información sobre cómo puede un software generar fractales, Dada la naturaleza finita de la computación en contraste con infinidad que representan los fractales en teoría. Investigando otros softwares capaces de representar fractales y como lograban dicha hazaña, llegamos a la conclusión que podíamos crearlos mediante funciones recursivas, más las mismas debían estar limitadas a un "nivel de detalle" o "grado". Esto último ya que de otra forma resultaría difícil o directamente imposible computarlos.
### Triángulo de Sierpinski:
El primer dibujo que creamos fue el famoso Triángulo de Sierpinski, ya que su forma repetitiva a base de triángulos parecía sencilla de replicar en nuestro programa. Primero se establece lo básico para que funcione el programa, como importar las librerías y generar las referencias necesarias, el tipo de figura y las funciones de coloreo, todo tal cual hecho para Escher.
Luego la creación particular de este dibujo se basa en 3 funciones. La primera siendo `grado`, la cual solamente devuelve un `int`, el cual usaremos como el nivel de detalle con el cual queremos realizar el dibujo. Esto último llamando a la tercera función definida con `grado` como parámetro.  
La segunda siendo `piramidal`, la cual dada un `dibujo` devuelve otro compuesto de 3 instancias del original dispuestas en forma triangular. Para esto fue necesario además de las funciones `juntar` y `apilar`, la utilización de rectángulos transparentes, puesto que por cómo funciona `apilar`, el dibujo de arriba se estiraría y no “encajaría” con las puntas de los triángulos de abajo, es por eso que en la fila de arriba se utilizan 2 rectángulos transparentes, uno a cada lado de la figura, con el objetivo de que ocupen el espacio justo para que ese el dibujo del medio se alinee perfectamente con sus 2 iteraciones de la fila de abajo.
La última función es `SierpinskiRecursivo`, la cual dado el un `int`, devuelve un triángulo de Sierpinski con ese nivel de detalle, esto lo hace llamando a piramidal de esta misma función con `int-1`, hasta llegar al caso base de 1 donde finalmente devuelve un triángulo.
```haskell
sierpinskiRecursive :: Int -> Dibujo Sierpinski 
sierpinskiRecursive 1 = rotar (figVioleta Triangulo) 
sierpinskiRecursive n =  piramidal (sierpinskiRecursive (n-1))
```
### Alfombra de Sierpinski:
Maravillados con los fractales de Waclaw Sierpinski, decidimos formar, además, la famosa alfombra de Sierpinski, cuyo concepto se veía alcanzable con solo algunas modificaciones al necesario para dibujar su triángulo.
Para este dibujo partimos desde una copia del triángulo de Sierpinski, de ahí se modifica, naturalmente, la función recursiva para que así el caso base devuelva un cuadrado en lugar de un triángulo. 
Luego se sustituye la función `piramidal` por la función `encuadrar`, la cual toma un `dibujo` y devuelve una composición donde ese mismo dibujo se repite nuevamente en todas las casillas de una cuadrícula 3x3 excepto en el casillero del medio, dónde en su lugar va un cuadrado invisible.
Con esto notamos que, si bien técnicamente funciona, puesto que los cuadrados estan hechos de líneas, son indistinguibles del cuadrado del medio, excepto cuando se lo ve de lejos y usando un `grado` alto. Es por esto que exploramos la librería de Gloss y encontramos la solución, puesto que nos permite importar `Polygon`, para sustituir line en la interpretación del rectángulo, lo cual no representa ningún problema puesto que las `figuras` son parámetros de tipo y no importa si son líneas o polígonos, dándole así relleno en lugar de solo contorno. De esta forma es posible apreciar el dibujo incluso en `grados` bajos.
### Círculos:
Finalmente, explorando las posibilidades que ofrece Gloss nos hayamos con la oportunidad de crear otro dibujo empleando círculos, es así como, bajo el nombre de "prueba", podemos hallar un patrón simple de círculos con triángulos de fondo.
Para este dibujo se toma como base el de la alfombra de Sierpinski y se modifica para sustituir los cuadrados por círculos, los cuales a diferencia de line o Polygon, no toman el tipo `path` como argumento sino solo 2 `floats` representando radio y grosor, por lo que para desplazarlos es necesario acompañarlos de la función `Translate`. 
Luego se modifica la función final de ensamblaje para que superponga todo sobre un fondo negro con un rombo formado por 2 polígonos de triángulo rotados apilados. 
La función encuadrar no es modificada y se aprovecha que con esa misma al ser aplicada a círculos da un patrón moderadamente interesante, pudiendo aún aumentar o disminuir el grado del dibujo, más un grado distinto a 2 podría no alinearse bien con el fondo.
### Inspiración:
[Software generador de Fractales](https://en.wikipedia.org/wiki/Fractal-generating_software)
[- Triangulo de Sierpinski](https://es.wikipedia.org/wiki/Tri%C3%A1ngulo_de_Sierpinski)
[- Alfombra de Sierpinski](https://es.wikipedia.org/wiki/Alfombra_de_Sierpinski)
