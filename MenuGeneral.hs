funcionInicio= do
    funcionMenu True
funcionMenu hacer = do
    if hacer
        then do
        putStrLn("| B I E N V E N I D O   A L   M E N Ú   G E N E R A L |\n\n SELECCIONA UNA DE LAS OPCIONES SIGUIENTES")
        putStrLn("\n1.- SERIE DE FIBONACCI")
        putStrLn("2.- RECURSIVIDAD DEL 1 - 10")
        putStrLn("3.- FACTORIAL DE UN NUMERO")
        putStrLn("4.- RECURSIVIAD DEL 10 - 0")
        putStrLn("5.- PALINDROMOS")
        putStrLn("6.- CALCULADORA ARITMETICA")
        putStrLn("7.- SALIR")
        opc <- getLine

        case opc of
            "1" -> funcionFibonacci--Entrada por teclado
            "2" -> funcionRecursividadpp 1 --Parametro ya establecido
            "3" -> funcionFactorial --ent
            "4" -> funcionRecursividadmm [0,1,2,3,4,5,6,7,8,9,10] --lista de parametros en arreglo del 0 al 10
            "5" -> funcionPalindromos--Entrada por teclado
            "6" -> funcionCalcu --lEntrada por teclado y seccionado por menu (entrada por teclado)
            "7" -> funcionMenu False
    else
        putStr "Hasta la próxima . . . \n"
--AQUI EMPEZAMOS LO FIBONACCIDO
funcionFibonacci = do
    putStrLn("BIENVENIDO A LA SUCESION FIBONACCI EN HASKELL\n\n")
    putStrLn "¿Cual es la posicion que deseas saber?\n"
    n <- getLine
    let nInt = read n::Int
    --print (nInt)
    serie nInt
    funcionMenu True
serie nInt = do
    let seriea = ["0","1","1","2","3","5","8","14","21","35","55","89","144","233","377"]
    if(nInt <= 15)
        then do
            print(seriea !! nInt)
    else do
        funcionMenu True    
--AQUI EMPEZAMOS DEL 1 2 3 4 5 6 8 9 10
funcionRecursividadpp n = do
    if n <= 10
        then do
            print n
            funcionRecursividadpp(n+1)
    else do
        putStrLn("\n SE CUMPLIÓ LA CONDICIÓN CRACK \n")
    funcionMenu True
--AQUI EMPEZAMOS EL FACTORIAL DE UN NUMERO
funcionFactorial = do
    putStrLn("BIENVENIDO A EL FACTORIAL DE UN NUMERO EN HASKELL")
    print("Ingresa el numero crack:")
    n <- getLine
    print((product [1..(read n)]))
    putStrLn("\n SE CUMPLIÓ LA CONDICIÓN CRACK \n")
    putStrLn("\n Regresando al menú principal ... \n")
    funcionMenu True

--AQUI EMPEZAMOS DEL 10 9 8 7 6 5 4 3 2 1 0
funcionRecursividadmm n = do
    if null n
        then do
            putStrLn("\n SE CUMPLIÓ LA CONDICIÓN CRACK \n")
            funcionMenu True
    else do
        print(n)
        let nl = init n
        funcionRecursividadmm(nl)

--AQUI EMPIEZA PALINDROMOS
funcionPalindromos = do
    putStrLn("BIENVENIDO A LOS PALINDROMOS EN HASKELL \n")
    putStrLn("Ingresa la cadena:   \n")
    cdn <- getLine
    let cadena = cdn == reverse cdn
    if cadena == True
        then do 
            putStrLn("\n Ésta cadena Si es un Palindromo \n")
            funcionPalindromos
        else do
            putStrLn("\n Ésta cadena No es un Palindromo \n")
            putStrLn("\n Regresando al menú principal ... \n")
            funcionMenu True

--AQUI EMPIEZA LA CALCULADORA
funcionCalcu = do
    putStrLn("BIENVENIDO A LA CALCULADORA ARITMETICA EN HASKELL")
    funcion True

funcion compila = do
    if compila
        then do
        putStr "\n1.- Suma \n"
        putStr "2.- Resta \n"
        putStr "3.- Multiplicación \n"
        putStr "4.- División \n"
        putStr "5.- Salir \n"
        op <- getLine

        case op of
            "1" -> suma
            "2" -> resta
            "3" -> multi
            "4" -> division
            "5" -> funcionMenu True
            
    else
        putStr "Pulsa 5 si quieres ir al menu principal"

suma = do
    putStrLn ("Introduce al primer numero: \n")
    a <- getLine
    putStrLn ("Introduce al segundo numero: \n")
    b <- getLine

    let aInt = read a::Int
    let bInt = read b::Int
    let resultado = aInt + bInt

    putStrLn ("El resultado es: "++ show resultado)
    putStrLn("\n SE CUMPLIÓ LA CONDICIÓN CRACK \n")
    funcion True

resta = do
    putStrLn ("Introduce al primer numero: \n")
    a <- getLine
    putStrLn ("Introduce al segundo numero: \n")
    b <- getLine

    let aInt = read a::Int 
    let bInt = read b::Int
    let resultado = aInt - bInt

    putStrLn ("El resultado es: "++ show resultado)
    putStrLn("\n SE CUMPLIÓ LA CONDICIÓN CRACK \n")
    funcion True

multi = do
    putStrLn ("Introduce al primer numero: \n")
    a <- getLine
    putStrLn ("Introduce al segundo numero: \n")
    b <- getLine

    let aInt = read a::Int 
    let bInt = read b::Int
    let resultado = aInt * bInt

    putStrLn ("El resultado es: "++ show resultado)
    putStrLn("\n SE CUMPLIÓ LA CONDICIÓN CRACK \n")
    funcion True

division = do
    putStrLn ("Introduce al primer numero: \n")
    a <- readLn
    putStrLn ("Introduce al segundo numero: \n")
    b <- readLn
    --cambio de uso de variables en esta seccion (la neta porque me salía error con los parseos xd)
    print ("El resultado es: "++ show(a/b))
    putStrLn("\n SE CUMPLIÓ LA CONDICIÓN CRACK \n")
    funcion True
 
