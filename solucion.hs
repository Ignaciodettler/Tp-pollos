import Data.List
import Text.Show.Functions()

data Pollo = UnPollo{
    nombre :: String,
    diasVivo:: Integer,
    pesoGramos:: Integer,
    artesMarcialesDominados:: [String]
} deriving Show

data Entrenador = UnEntrenador {
    nombreEntrenador :: String,
    entrenamiento :: Entrenamiento
}deriving Show

data Raton = UnRaton{
    peso :: Integer,
    altura :: Integer,
    cantidadBigotes :: Integer
}deriving Show

type Entrenamiento = (Pollo -> Pollo)
gin :: Pollo
gin = UnPollo "ginger" 10 150 []

roc:: Pollo
roc = UnPollo "rocky" 1 3000 []

pau:: Pollo
pau = UnPollo "Paula" 2 300 ["judo"]

engordar :: Integer -> Pollo -> Pollo
engordar gramos unPollo = unPollo { pesoGramos =   gramos + pesoGramos unPollo}

mayorDeEdad :: Pollo -> Bool
mayorDeEdad unPollo = diasVivo unPollo > 180

listaVacia :: Pollo -> Bool
listaVacia unPollo = artesMarcialesDominados unPollo == []

--cruzarPollos :: [Pollo]->Pollo
--cruzarPollos unosPollos = UnPollo {nombre = }



---2da parte--

arguiniano :: Entrenamiento
arguiniano unPollo = engordar 100 unPollo

miyagi :: Entrenamiento
miyagi unPollo = comprobarYAgregar "karate" unPollo

marcelito :: Entrenamiento
marcelito  = miyagi.olvidarArtes    

olvidarArtes :: Entrenamiento
olvidarArtes unPollo = unPollo { artesMarcialesDominados = []}

brujaTapita:: Raton-> Entrenamiento
brujaTapita unRaton unPollo = engordar (calcularAlimento unRaton) unPollo

calcularAlimento :: Raton -> Integer
calcularAlimento unRaton = ((peso unRaton) * (altura unRaton)) - (cantidadBigotes unRaton)

marioBros:: String-> Entrenamiento
marioBros arteMarcial = agregarANombre "Super Mario " .(comprobarYAgregar  arteMarcial)                  

enseñarArteMarcial :: String -> Entrenamiento
enseñarArteMarcial arteMarcial unPollo = unPollo{ artesMarcialesDominados = arteMarcial : (artesMarcialesDominados unPollo)}

comprobarYAgregar :: String -> Entrenamiento
comprobarYAgregar arteMarcial unPollo | elem arteMarcial (artesMarcialesDominados unPollo) == False = enseñarArteMarcial arteMarcial unPollo
                                      | otherwise = unPollo
agregarANombre:: String -> Entrenamiento
agregarANombre agregado unPollo =  unPollo { nombre = agregado ++ (nombre unPollo)}          

--no entiendo como haria entrenar, porque no todos los entrenadores tienen la misma cantidad de parametro 
entrenar:: Entrenador-> Entrenamiento
entrenar unEntrenador unPollo = unEntrenador unPollo

elQueMasEnseña :: Entrenador -> Entrenador -> Pollo->String
elQueMasEnseña unEntrenador otroEntrenador unPollo | length (artesMarcialesDominados (entrenar unEntrenador unPollo)) >  length (artesMarcialesDominados (entrenar otroEntrenador unPollo)) = "El primer entrenador enseña más"
                                                   | otherwise = "El segundo entrenador enseña más"