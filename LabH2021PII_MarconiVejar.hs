------------------------------------------------
-- Module : LabH2021
-- Developer : Marconi Vejar, Axel Gonzalo
--
-- Programacion II - Laboratorio Haskell 2021
------------------------------------------------


module LabH2021 where


----------------------------------------------------  EJERCICIO 1



-- fun-> funcion que devuelve un Bool.
allR :: (a -> Bool) -> [a] -> Bool  
allR fun [] = True
allR fun (x:xs) = (fun x) && (allR fun xs)


allRA :: (a -> Bool) -> [a] -> Bool  
allRA fun l = acc fun l True
 where 
  acc fun l False = False --Frena la recursion cuando encuentra false.
  acc fun [] acu = acu
  acc fun (x:xs) acu = acc fun xs ((fun x)&&acu)
--La recursion es de cola sin necesidad del acumulador.

allfl :: (a -> Bool) -> [a] -> Bool
allfl fun l = foldl (&&) True (map (fun) l) -- Convierto la lista en otra lista con valores booleanos.

allfr :: (a -> Bool) -> [a] -> Bool
allfr fun l = foldr (&&) True (map (fun) l) -- Convierto la lista en otra lista con valores booleanos.



----------------------------------------------------  EJERCICIO 2



filterR :: (a -> Bool) -> [a] -> [a]
filterR fun [] = []
filterR fun (x:xs) = if (fun x) then x:(filterR fun xs) else filterR fun xs

filterRA :: (a -> Bool) -> [a] -> [a]
filterRA fun l = acum fun l []
 where 
  acum fun [] ac = ac
  acum fun (x:xs) ac = if (fun x) then acum fun xs (ac++[x]) else acum fun xs ac

filterfl :: (a -> Bool) -> [a] -> [a]
filterfl fun l = foldl (++) [] [(aux l)]
 where
  aux [] = []
  aux (x:xs) = if (fun x) then x:aux xs else aux xs

filterfr :: (a -> Bool) -> [a] -> [a]
filterfr fun l = foldr (:) [] (aux l)
 where
  aux [] = []
  aux (x:xs) = if (fun x) then x:aux xs else aux xs



----------------------------------------------------  EJERCICIO 3

takeR :: Int -> [a] -> [a]
takeR 0 l = []
takeR n [] = [] -- Si el usuario quiere tomar mas elementos de los que tiene la lista, la recursion para cuando se vacía la lista.
takeR n (x:xs) = x:(takeR (n-1) xs)

takeRA :: Int -> [a] -> [a]
takeRA n l = acum n l []
 where 
  acum 0 l ac = ac
  acum n [] ac = ac --Si el usuario quiere tomar mas elementos de los que tiene la lista, la recursion para cuando se vacía la lista.
  acum n (x:xs) ac = acum (n-1) xs (ac++[x])

takefl :: Int -> [a] -> [a]
takefl n l = foldl (++) [] [(aux n l [])]
 where
  aux 0 l ac = ac
  aux n [] ac = ac 
  aux n (x:xs) ac = aux (n-1) xs (ac++[x])

takefr :: Int -> [a] -> [a]
takefr n l = foldr (:) [] (aux n l [])
 where
  aux 0 l ac = ac
  aux n [] ac = ac
  aux n (x:xs) ac = aux (n-1) xs (ac++[x])



----------------------------------------------------  EJERCICIO 4



{- ej. 4.a



Expresion: map item tabla1S
--- Usa una funcion de extracción (item) como primer argumento de la funcion map. Map aplica esa funcion a toda la tabla1S, por lo tanto
--- devuelve una lista con todos los ITEMS de la tabla1S.


Expresion: filter ((==200).codItem)tabla1S
--- Usa una composicion de funciones como argumento de filter. Primero extrae el codigo de un elemento de la tabla1S y despues
--- lo compara con 200. Si el codigo es igual a 200, mete todos los datos de ese item en una lista. Hace lo mismo con cada
--- item de tabla1S.
--- Entonces, devuelve una lista con TODOS LOS DATOS de todos los items con codigo 200.



-}



-- Funcion solicitada en ej.4 (fReponer para Recursantes)

--                           -

-- Funcion solicitada en ej.4 (increPU para NO Recursantes)

increPU :: (Eq j, Fractional j) => [(a, b, c, d, e, f, g, h, i, j, k)] -> j -> [(a, b, c, d, e, f, g, h, i, j, k)]
increPU tabla n = aux tabla n []
 where
  aux [] n acc = acc -- (Frenar la recursion)
  aux l 0 acc = l -- (Incrementar 0% los precios)
  aux (x:xs) n acc = aux xs n (acc++[((codItem x),(item x),(marca x),(rubro x),(proveedor x),(unidad x),(cant x),(cmin x),(cmax x),((precio x)*(1+n/100)),(pganancia x))]) 



----------------------------------------------------



-- MODELO SISTEMA DE STOCK DE ALMACEN --
--------------------------------------------------
type Cod_Item = Int -- Codigo Interno del producto
type Item = String -- Descripcion del producto
type Marca = String -- Marca
type Rubro = String -- Rubro
type Cod_Proveedor = Int -- Codigo Interno del proveedor
type U_Med = String -- Unidad de Medida: --1LT,800GRM, 1500CM3, etc
type Cant_Existente = Int -- cantidad de productos en deposito (E)
type V_Min = Int -- valor en existencia recomendado para reposicion (EMin)
type V_Max = Int -- valor maximo de acopio en deposito (EMax)
type Precio_U = Float -- precio o valor de compra unitario
type P_Ganancia = Int -- Porcentaje de ganancia sobre el precio de compra
type Nombre = String
type Direccion = String
type Telefono = String 
--tupla con datos de 1 item de Stock
type Item_Stock = (
 Cod_Item,
 Item,
 Marca,
 Rubro,
 Cod_Proveedor,
 U_Med,
 Cant_Existente,
 V_Min,
 V_Max,
 Precio_U,
 P_Ganancia
 )
--tupla con datos de 1 proveedor
type Proveedor = (
 Cod_Proveedor,
 Nombre,
 Direccion,
 Telefono
 )
--Tablas BD
type T_Stock = [Item_Stock] --Tabla con el Stock de un comercio
type T_Proveedor = [ Proveedor] --Tabla con los proveedores de un comercio

--FUNCIONES DE EXTRACCIÓN
codItem (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan)= cod
item (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = item
marca (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = marca
rubro (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = rubro
proveedor (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = prov
unidad (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = umed
cant (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = cant
cmin (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = cmin
cmax (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = cmax
precio (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = preciou
pganancia (cod,item,marca,rubro,prov,umed,cant,cmin,cmax,preciou,pgan) = pgan



-- datos predefinidos (Ejemplo)
tabla1S:: T_Stock
tabla1S= [
 (100,"ARROZ GRANO GRANDE","CONDOR","Alimentos",20,"1LT",8000,500,10000,20,30),
 (107,"ARROZ GRANO GRANDE","GALLO","Alimentos",20,"1KG",6000,200,8000,25,30),
 (200,"ACEITE DE GIRASOL","NATURA","Alimentos",20,"1LT",9800,600,10000,40,30),
 (200,"ACEITE DE  GIRASOL","COCINERO","Alimentos",20,"1LT",900,500,10000,30,30),
 (410,"AGUA MINERAL S/GAS BAJO SODIO","SER","Alimentos",31,"1.5LT",20,50,3000,10,35),
 (412,"AGUA SABORIZADA LIMA LIMON","SER","Alimentos",31,"2LT",1570,50,3000,15, 35),
 (478,"ALFAJOR CHOCOLATE TITA","TERRABUSI","Alimentos",31,"36GR",900,200,5000,4, 30),
 (479,"ALFAJOR CHOCOLATE RODESIA","TERRABUSI","Alimentos",31,"40GR",9,200,3500, 4,30),
 (708,"LECHE DESC. PASTEURIZADA","SERENISIMA","Alimentos",31,"1TL",230,100,1200,20,30),
 (767,"ARVEJAS SECAS REMOJADAS","NOEL","Alimentos",20,"300GR",1203,500,3000,10,30),
 (801,"ANTITRANSPIRANTE ROLL ON","ETIQUET","PERFUMERIA",20,"60gr",30,45,2000,25,30) ]
tabla1P :: T_Proveedor
tabla1P= [ (20,"Juan Perez","Belgrano 1827, San Luis, 5700, Argentina","2664-786543"),(31,"Jose Lopez","Junin 444, Mendoza, 5500, Argentina","261-3452677")]