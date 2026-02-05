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

test_that("Funcionalidad correcta de palabras_vocales", {
  
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
  
  # Caso 3: Robustez (Mayúsculas, Acentos Internacionales)
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

test_that("Gestión de errores en palabras_vocales", {
  
  # Reutilizamos el input_check, así que los errores deben ser idénticos
  
  # Error de tipo
  expect_error(palabras_vocales("No soy lista"), "debe ser una lista")
  
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
