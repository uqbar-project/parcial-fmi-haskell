module Library where
import PdePreludat

-- Parcial FMI
-- Punto 1 - 2 puntos global
-- 1.a Representar al TAD Pais 
type Recurso = String

data Pais = Pais {
    ingresoPerCapita :: Number,
    activosPublico :: Number,
    activosPrivado :: Number,
    recursosNaturales :: [Recurso],
    deuda :: Number
} deriving (Eq, Show)

-- 1.b Generar al pais Namibia
namibia :: Pais
namibia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50

-- Punto 2 - 4 puntos
-- Prestarle plata
type Estrategia = Pais -> Pais

prestarPlata :: Number -> Estrategia
prestarPlata cuanto pais = pais {
    deuda = deuda pais + cobrarNumberereses cuanto
}

cobrarNumberereses :: Number -> Number
cobrarNumberereses cuanto = cuanto * 1.5

-- 2.b 
-- Reducir x puestos de trabajo del sector publico
reducirPuestos :: Number -> Estrategia
reducirPuestos cantidadPuestos pais = pais {
    activosPublico = activosPublico pais - cantidadPuestos,
    ingresoPerCapita = ingresoPerCapita pais * (1 - reduccionIngreso cantidadPuestos)
}

reduccionIngreso :: Number -> Number
reduccionIngreso cantidadPuestos | cantidadPuestos > 100 = 0.2
                                 | otherwise             = 0.15

-- 2.c 
-- Darle a una empresa afin la explotacion de alguno de los recursos
explotar :: Recurso -> Estrategia
explotar recurso pais = pais {
    recursosNaturales = quitarRecurso recurso $ recursosNaturales pais,
    deuda = deuda pais - 20
}

quitarRecurso :: Recurso -> [Recurso] -> [Recurso]
quitarRecurso recurso = filter (/= recurso)

-- 2.d
blindaje :: Estrategia
blindaje pais = (prestarPlata (pbi pais * 0.5) . reducirPuestos 500) pais

pbi :: Pais -> Number
pbi pais = ingresoPerCapita pais * poblacionActiva pais
-- el fromNumberegral no es importante

poblacionActiva :: Pais -> Number
poblacionActiva pais = activosPrivado pais + activosPublico pais

-- Punto 3 - 2 puntos
-- a) Modelar una receta que consista en prestar 200 millones, y darle a una empresa X
-- la explotación de la Minería de un país.
type Receta = [Estrategia]

receta :: Receta
receta = [prestarPlata 2000, explotar "Mineria"]

-- b) Aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). 
-- Justificar cómo se logra el efecto colateral.
aplicarReceta :: Receta -> Pais -> Pais
-- opción con foldl + lambda
-- aplicarReceta receta pais = foldl (\pais estrategia -> estrategia pais) pais receta
-- opción con foldr + $
aplicarReceta receta pais = foldr ($) pais receta

-- Punto 4 - 3 puntos
-- 4.a) Conocer los países que pueden zafar, que son aquellos que tienen "Petróleo" entre sus riquezas naturales.
puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter $ elem "Petroleo" . recursosNaturales

-- 4.b) Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
totalDeuda :: [Pais] -> Number
totalDeuda = foldr ((+) . deuda) 0

-- Punto 5 - 2 puntos
-- dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, 
-- en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor
estaOrdenado :: Pais -> [Receta] -> Bool
estaOrdenado pais [receta] = True
estaOrdenado pais (receta1:receta2:recetas)
     = revisarPBI receta1 pais <= revisarPBI receta2 pais && estaOrdenado pais (receta2:recetas)
     where revisarPBI receta = pbi . aplicarReceta receta

-- Punto 6 - 1 punto
-- Si un país tiene infinitos recursos naturales, modelado con esta función
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

--    a) ¿qué sucede con la función 4a? 
--    b) ¿y con la 4b?
--    Justifique ambos puntos relacionándolos con algún concepto.
pruebaInfinita1 = puedenZafar [namibia, Pais 1 1 1 recursosNaturalesInfinitos 1]
--              no termina nunca, porque quiere buscar "Mineria" entre los recursos
pruebaInfinita2 = totalDeuda [namibia, Pais 1 1 1 recursosNaturalesInfinitos 1]
--              se puede porque al no evaluar los recursos solamente suma deuda
-- relacionado con evaluacion diferida, solo se evalua lo que se necesita

-- Evaluacion
-- **********
-- 14     = 10
-- 13     =  9
-- 12, 11 =  8
-- 10     =  7
--  8, 9  =  6
--  7     = Revision
--  6     =  3
--  5..0  =  2

f :: Ord p => ((a, Bool) -> b) -> (b -> p) -> p -> [a] -> p
f x y h (c:cs) | (y . x) (c, True) > h = f x y h cs
               | otherwise             = h
               