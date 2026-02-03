library(testthat)

source("programacion_basica.R") 

# ============================================================================
# BLOQUE 1: Tests para la función 'concatenar'
# ============================================================================

test_that("Funcionalidad correcta de concatenar (Happy Path)", {
  
  # Caso 1: Lista simple estándar
  input_1 <- list("Hola", "Mundo", "R")
  output_1 <- concatenar(input_1)
  
  expect_is(output_1, "character")       # Chequeo de tipo
  expect_equal(output_1, "Hola Mundo R") # Chequeo de valor
  expect_length(output_1, 1)             # Debe devolver un solo string
  
  # Caso 2: Lista nombrada (Simulación de Diccionario Python)
  # Verificamos que ignora las claves y usa los valores, como se pide
  input_2 <- list(
    sujeto = "El",
    verbo = "gato",
    adjetivo = "negro"
  )
  expect_equal(concatenar(input_2), "El gato negro")
})

test_that("Gestión de errores en concatenar (Sad Path)", {
  
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

test_that("Gestión de errores en palabras_vocales", {
  
  # Reutilizamos el input_check, así que los errores deben ser idénticos
  
  # Error de tipo
  expect_error(palabras_vocales("No soy lista"), "debe ser una lista")
  
  # Error de contenido numérico
  input_error <- list(3.14, "pi")
  expect_error(palabras_vocales(input_error), "Fallo en índices: 1")
})