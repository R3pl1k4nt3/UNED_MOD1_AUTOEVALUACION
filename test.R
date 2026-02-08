library(testthat)

source("programacion_basica.R") 

# ============================================================================
# BLOQUE 1: Tests para la función 'concatenar'
# ============================================================================

test_that("Happy Path", {
  
  # Caso 1: Lista simple estándar
  input_1 <- list("Hola", "Mundo", "R")
  output_1 <- concatenar(input_1)
  
  expect_is(output_1, "character")       # Compruebo tipo
  expect_equal(output_1, "Hola Mundo R") # Compruebo valor
  expect_length(output_1, 1)             # Compruebo que devuelve un solo string
  
  # Caso 2: Lista nombrada (Simulación de Diccionario Python)
  # Verificamos que ignora las claves y usa los valores, como se pide
  input_2 <- list(
    sujeto = "El",
    verbo = "gato",
    adjetivo = "negro"
  )
  expect_equal(concatenar(input_2), "El gato negro")
})

test_that("Gestión de errores - Sad Path", {
  
  # Error 1: Entrada no es lista
  expect_error(
    concatenar(c("vector", "simple")), 
    "La entrada de la función debe ser una lista"
  )
  
  # Error 2: Lista vacía
  expect_error(
    concatenar(list()), 
    "La lista de entrada no puede estar vacía"
  )
  
  # Error 3: Tipos incorrectos (Números mezclados)
  # Probamos que el mensaje de error identifica el índice (índice 2 es el número)
  input_mix <- list("Texto", 123, "Otro")
  expect_error(
    concatenar(input_mix), 
    "Todos los elementos deben ser strings. Fallo en índices: 2"
  )
})

# ============================================================================
# BLOQUE 2: Tests para la función 'palabras_vocales'
# ============================================================================

test_that("Happy Path", {
  
  # Caso 1: Ganador único
  # 'murciélago' (5) vs 'sol' (1)
  input_1 <- list("murciélago", "sol")
  output_1 <- palabras_vocales(input_1)
  
  expect_is(output_1, "list")            # Requisito: devolver lista
  expect_length(output_1, 1)             # Solo un ganador
  expect_equal(output_1[[1]], "murciélago")
  
  # Caso 2: Empate (Debe devolver múltiples valores)
  # 'casa' (2) vs 'pato' (2) vs 'luz' (1)
  input_2 <- list("casa", "pato", "luz")
  output_2 <- palabras_vocales(input_2)
  
  expect_length(output_2, 2)             # Esperamos 2 ganadores
  expect_equal(output_2[[1]], "casa")
  expect_equal(output_2[[2]], "pato")
  
  # Caso 3: Mayúsculas, Acentos Internacionales
  # 'FORÊT' (2 vocales) vs 'día' (2 vocales) -> Empate
  input_3 <- list("FORÊT", "día")
  output_3 <- palabras_vocales(input_3)
  
  # Verificamos que devuelve la palabra ORIGINAL (con mayúsculas y acentos)
  expect_true("FORÊT" %in% unlist(output_3))
  expect_true("día" %in% unlist(output_3))
})

# ============================================================================
# BLOQUE 3: Tests para la función 'lista_no_negativos'
# ============================================================================

test_that("HAPPY PATH", {
  
  # Reutilizamos el input_check, así que los errores deben ser idénticos
  
  # Error de tipo
  expect_error(palabras_vocales("No soy una lista"), "debe ser una lista")
  
  # Error de contenido numérico
  input_error <- list(3.14, "pi")
  expect_error(palabras_vocales(input_error), "Fallo en índices: 1")
})


test_that("Happy Path - lista_no_negativos", {
  
  # Caso 1: Mezcla estándar de positivos, negativos y decimales
  input_1 <- list(10, -5, 3.5, -1.2, 0)
  output_1 <- lista_no_negativos(input_1)
  
  expect_is(output_1, "list")               # Compruebo tipo (debe ser lista)
  expect_equal(unlist(output_1), c(10, 3.5, 0)) # Compruebo valores filtrados
  expect_length(output_1, 3)                # Compruebo que quedan 3 elementos
  
  # Caso 2: Todos negativos
  # Verificamos que devuelve una lista vacía pero de tipo lista
  input_2 <- list(-10, -20, -30)
  output_2 <- lista_no_negativos(input_2)
  expect_length(output_2, 0)
  expect_is(output_2, "list")
  
  # Caso 3: Lista con ceros y positivos (No debe eliminar nada)
  input_3 <- list(0, 100, 0.001)
  expect_equal(unlist(lista_no_negativos(input_3)), c(0, 100, 0.001))
})

test_that("Gestión de errores - Sad Path", {
  
  # Error 1: Entrada no es lista
  expect_error(
    lista_no_negativos(c(1, 2, 3)), 
    "La entrada de la función debe ser una lista"
  )
  
  # Error 2: Lista vacía
  expect_error(
    lista_no_negativos(list()), 
    "La lista de entrada no puede estar vacía"
  )
  
  # Error 3: Tipos incorrectos (Strings mezclados)
  # El mensaje de error debe identificar el índice del fallo usando input_check_num
  input_mix <- list(10, "NoSoyNumero", 5)
  expect_error(
    lista_no_negativos(input_mix),
    "Todos los elementos deben ser números. Fallo en índices: 2"
  )
})

# ============================================================================
# BLOQUE 4: Tests para la función 'agrupar_por_letra'
# ============================================================================

test_that("Happy Path - agrupar_por_letra", {
  
  # Caso 1: Agrupación estándar
  input_1 <- list("casa", "coche", "arbol", "avion", "dado")
  output_1 <- agrupar_por_letra(input_1)
  
  expect_is(output_1, "list")                    # El output debe ser una lista (diccionario)
  expect_true(all(c("c", "a", "d") %in% names(output_1))) # Comprobamos que existen las claves
  expect_length(output_1$a, 2)                   # 'arbol' y 'avion'
  expect_equal(unlist(output_1$c), c("casa", "coche"))
  
  # Caso 2: OJO MAYUSCULAS (Agrupo 'Hola' y 'hielo' en la misma 'h')
  input_2 <- list("Hola", "hielo")
  output_2 <- agrupar_por_letra(input_2)
  expect_named(output_2, "h")                    # La clave debe ser 'h' minúscula
  expect_length(output_2$h, 2)
})

test_that("Gestión de errores - Sad Path", {
  
  # Error 1: Reutilizamos el input_check de strings (Entrada no es lista)
  expect_error(
    agrupar_por_letra(c("vector")), 
    "La entrada de la función debe ser una lista"
  )
  
  # Error 2: Tipos incorrectos (Número en la lista)
  input_err <- list("perro", 100)
  expect_error(
    agrupar_por_letra(input_err),
    "Todos los elementos deben ser strings. Fallo en índices: 2"
  )
})


# ============================================================================
# BLOQUE 5: Tests para la función 'multiplos_tres_positivos'
# ============================================================================

test_that("Happy Path - multiplos_tres_positivos", {
  
  # Caso 1: Mezcla de números
  # 9 (múltiplo y +), -3 (múltiplo pero -), 4 (no múltiplo), 0 (es múltiplo de 3 y no es negativo)
  input_1 <- list(9, -3, 4, 0, 6)
  output_1 <- multiplos_tres_positivos(input_1)
  
  expect_is(output_1, "list") 
  expect_equal(unlist(output_1), c(9, 0, 6)) # Solo debe devolver 9, 0 y 6
  expect_length(output_1, 3)
  
  # Caso 2: Ningún múltiplo de 3 en la lista
  input_2 <- list(1, 2, 4, 5)
  expect_length(multiplos_tres_positivos(input_2), 0)
})

test_that("Gestión de errores - Sad Path", {
  
  # Error 1: Entrada no es lista (gestión por input_check_num)
  expect_error(
    multiplos_tres_positivos(c(3, 6, 9)), 
    "La entrada de la función debe ser una lista"
  )
  
  # Error 2: Tipos incorrectos (String en lista de números)
  input_err <- list(3, "seis", 9)
  expect_error(
    multiplos_tres_positivos(input_err),
    "Todos los elementos deben ser números. Fallo en índices: 2"
  )
})


# ============================================================================
# BLOQUE 6: Tests para la función 'calculo_mediana'
# ============================================================================

test_that("Happy Path - calculo_mediana", {
  
  # Caso 1: Número impar de elementos (La mediana es el valor central)
  # Ordenados: 1, 3, 5 -> Mediana: 3
  input_1 <- list(5, 1, 3)
  output_1 <- calculo_mediana(input_1)
  
  expect_is(output_1, "numeric")                # Compruebo tipo
  expect_equal(output_1, 3)                     # Compruebo valor
  
  # Caso 2: Número par de elementos (La mediana es el promedio de los dos centrales)
  # Ordenados: 1, 2, 4, 10 -> Mediana: (2+4)/2 = 3
  input_2 <- list(1, 4, 2, 10)
  expect_equal(calculo_mediana(input_2), 3)     # Compruebo valor
  
  # Caso 3: Valores con decimales
  input_3 <- list(1.5, 2.5, 3.5)
  expect_equal(calculo_mediana(input_3), 2.5)   # Compruebo valor
})

test_that("Gestión de errores - Sad Path", {
  
  # Error 1: Entrada no es lista
  expect_error(
    calculo_mediana(c(1, 2, 3)), 
    "La entrada de la función debe ser una lista"
  )
  
  # Error 2: Lista vacía
  expect_error(
    calculo_mediana(list()), 
    "La lista de entrada no puede estar vacía"
  )
  
  # Error 3: Elemento no numérico
  input_err <- list(1, "mediana", 3)
  expect_error(
    calculo_mediana(input_err),
    "Todos los elementos deben ser números. Fallo en índices: 2"
  )
})


# ============================================================================
# BLOQUE 7: Tests para la función 'lista_suma_pos_neg'
# ============================================================================

test_that("Happy Path - lista_suma_pos_neg", {
  
  # Caso 1: Mezcla de números
  # Positivos: 10 + 0 + 8 = 18
  # Negativos: -5 + (-1) = -6
  input_1 <- list(10, -5, 0, -1, 8)
  output_1 <- lista_suma_pos_neg(input_1)
  
  expect_is(output_1, "list")                    # Compruebo que devuelve el diccionario (lista nombrada)
  expect_named(output_1, c("positivos", "negativos")) # Compruebo las claves exactas
  
  # Verificamos que el valor de la clave sea una LISTA y contenga la SUMA correcta
  expect_is(output_1$positivos, "list")
  expect_equal(output_1$positivos[[1]], 18)
  
  expect_is(output_1$negativos, "list")
  expect_equal(output_1$negativos[[1]], -6)
  
  # Caso 2: Lista donde solo hay negativos
  # Positivos deben sumar 0
  input_2 <- list(-10, -20)
  output_2 <- lista_suma_pos_neg(input_2)
  expect_equal(output_2$positivos[[1]], 0)
  expect_equal(output_2$negativos[[1]], -30)
})

test_that("Gestión de errores - Sad Path", {
  
  # Error 1: Entrada no es lista (gestión por input_check_num)
  expect_error(
    lista_suma_pos_neg(c(1, -1)), 
    "La entrada de la función debe ser una lista"
  )
  
  # Error 2: Lista vacía
  expect_error(
    lista_suma_pos_neg(list()), 
    "La lista de entrada no puede estar vacía"
  )
  
  # Error 3: Elementos no numéricos
  input_err <- list(5, "diez", -2)
  expect_error(
    lista_suma_pos_neg(input_err),
    "Todos los elementos deben ser números. Fallo en índices: 2"
  )
})


# ============================================================================
# BLOQUE 9: Tests para la función 'notificar_vocales'
# ============================================================================

test_that("Happy Path - notificar_vocales", {
  
  # Caso 1: Palabra estándar con mezcla de vocales
  # 'Hola' -> a: TRUE, o: TRUE, resto: FALSE
  input_1 <- "Hola"
  output_1 <- notificar_vocales(input_1)
  
  expect_is(output_1, "list")                    # Verificamos que devuelve el diccionario
  expect_length(output_1, 5)                     # Verificamos que tiene las 5 vocales (a,e,i,o,u)
  
  # Comprobamos los valores booleanos específicos
  expect_true(output_1$a)
  expect_true(output_1$o)
  expect_false(output_1$e)
  expect_false(output_1$i)
  expect_false(output_1$u)
  
  # Caso 2: Uso lógica de Cuestión 2 (Acentos y Mayúsculas)
  # 'ÁGUILA' tiene a, u, i.
  output_2 <- notificar_vocales("ÁGUILA")
  expect_true(output_2$a)
  expect_true(output_2$u)
  expect_true(output_2$i)
  expect_false(output_2$e)
  expect_false(output_2$o)
})

test_that("Gestión de errores - Sad Path", {
  
  # Error 1: Entrada no es un string
  # Como uso input_check(list(string)), saltará el mensaje de error de tipos
  expect_error(
    notificar_vocales(100), 
    "Todos los elementos deben ser strings"
  )
  
  # Error 2: Entrada nula o vacía
  expect_error(notificar_vocales(NULL))
})


# ============================================================================
# BLOQUE 10: Tests para la función 'eliminar_repetidos'
# ============================================================================

test_that("Happy Path - eliminar_repetidos", {
  
  # Caso 1: Lista con repetidos exactos
  input_1 <- list("hola", "mundo", "hola", "r")
  output_1 <- eliminar_repetidos(input_1)
  
  expect_is(output_1, "list")
  expect_length(output_1, 3) # "hola", "mundo", "r"
  expect_true("hola" %in% unlist(output_1))
  
  # Caso 2: Repetidos con diferencia de Mayúsculas/Minúsculas
  # Debe tratarlos como iguales y dejar solo uno
  input_2 <- list("Casa", "CASA", "casa")
  output_2 <- eliminar_repetidos(input_2)
  
  expect_length(output_2, 1)
  expect_equal(output_2[[1]], "casa")
})

test_that("Gestión de errores - Sad Path", {
  
  # Error 1: Entrada no es lista
  expect_error(eliminar_repetidos("no soy lista"), "debe ser una lista")
  
  # Error 2: Tipos incorrectos (número en la lista)
  input_err <- list("palabra", 123)
  expect_error(
    eliminar_repetidos(input_err),
    "Todos los elementos deben ser strings. Fallo en índices: 2"
  )
})