# EJERCICIOS OBLIGATORIOS
# PROGRAMACIÓN BÁSICA (5 puntos)

input_check <- function(value_list) {
  
  # 1. ¿Es una lista?
  if (!is.list(value_list)) {
    stop("La entrada de la función debe ser una lista")
  }
  
  # 2. ¿Está vacía?
  
  if (length(value_list) == 0) {
    stop("La lista de entrada no puede estar vacía")
  }
  
  # 3. VERIFICACIÓN ESTRICTA DE TIPOS 
  
  
  # Usamos vapply para asegurar que devuelve TRUE/FALSE
  es_texto <- vapply(value_list, is.character, FUN.VALUE = logical(1))
  
  if (!all(es_texto)) {
    # Buscamos qué posiciones fallaron (ej: el índice 2)
    indices_malos <- which(!es_texto)
    valores_malos <- paste(indices_malos, collapse = ", ")
    
    # Lanzamos el mensaje exacto que espera el test
    stop(paste("Todos los elementos deben ser strings. Fallo en índices:", valores_malos))
  }
}


# Cuestión 1
# 
# Defina una función que concatene una lista de palabras en una única frase
# 
# Output de la función: string que sea el resultado de la frase obtenida

concatenar <- function(value_list){
  
  #Comprobacion de los valores antes de lanzar
  input_check(value_list)
  
  frase <- paste(unlist(value_list), collapse =" ")
  return(frase)
}

# test_concatenar = list("Hola","Mundo","prueba","R")
# 
# test_concatenar2 = list(
#   nombre = "Alex",
#   lenguaje = "R",
#   estado = "Aprendiendo"
# )

# concatenar(test_concatenar)
# concatenar(test_concatenar2)




# Cuestión 2
# 
# A partir de una lista de palabras, defina una función que encuentre la palabra con más vocales. Si hay más de una debe devolverse todas aquellas palabras que cumplan la condición.
# 
# Output de la función: lista de strings con las palabras que cumplan la condición

palabras_vocales <- function(value_list){
  #Comprobacion de los valores antes de lanzar
  input_check(value_list)
  
  vocales <- c("a","e","i","o","u")
  lista_palabras <- unlist(value_list)
  
  # Normalizo el texto. Convierto las mayusculas en minusculas y los posibles acentos para tener todo controlado
  #1. todo a minusculas
  palabras_normalizado <- tolower(lista_palabras)
  
  # 2. Ahora quitamos los posibles acentos, pero solo para contar
  palabras_normalizado <- chartr("áéíóúàèìòùâêîôûäëïöü",
                                 "aeiouaeiouaeiouaeiou",
                                 palabras_normalizado)
  
  # Encuentro iconv que traduce a UTF-8 y de ahí realizo el conteo de vocales
  #palabras_normalizado <- iconv(palabras_normalizado, from = "UTF-8", to ="ASCII/TRANSLIT")
  
  # Creo un vector vacio con la longitud de palabras
  contador <- numeric(length(palabras_normalizado))
  
  for (i in 1:length(palabras_normalizado)){
    palabra_bucle <- palabras_normalizado[i]
    
    # procedemos a splitear las palabras y a comparar 
    letras <- strsplit(palabra_bucle, split = "")[[1]]
    
    vocales_in_palabra <- letras %in% vocales
    contador[i] <- sum(vocales_in_palabra)
  }
  
  #3. Ya tenemos el numero de vocales, ahora debemos devolver la palabra
  
  max_vocales <- max(contador)
  
  # llamamos a la palabra original sin lowercase y sin transformar los acentos
  palabra_max_vocales <- lista_palabras[contador == max_vocales]
  
  return(as.list(palabra_max_vocales))
  
}


# Cuestión 3
# 
# A partir de una lista de números, defina una función que elimine todos los números negativos
# 
# Output de la función: lista de números sin valores negativos

input_check_num <- function(value_list) {
  
  # 1. ¿Es una lista?
  if (!is.list(value_list)) {
    stop("La entrada de la función debe ser una lista")
  }
  
  # 2. ¿Está vacía?
  if (length(value_list) == 0) {
    stop("La lista de entrada no puede estar vacía")
  }
  
  # 3. VERIFICACIÓN ESTRICTA DE TIPOS (Numéricos)
  # Usamos vapply para asegurar que devuelve TRUE/FALSE
  es_numero <- vapply(value_list, is.numeric, FUN.VALUE = logical(1))
  
  if (!all(es_numero)) {
    # Buscamos qué posiciones fallaron
    indices_malos <- which(!es_numero)
    valores_malos <- paste(indices_malos, collapse = ", ")
    
    # Lanzamos el mensaje exacto
    stop(paste("Todos los elementos deben ser números. Fallo en índices:", valores_malos))
  }
}

lista_no_negativos <- function(value_list) {
  
  # Comprobación
  input_check_num(value_list)
  
  # Convertimos a vector para filtrar de forma sencilla
  vector_numeros <- unlist(value_list)
  
  # Filtramos: nos quedamos con los que son mayores o iguales a 0
  no_negativos <- vector_numeros[vector_numeros >= 0]
  
  # Devolvemos el resultado como una lista
  return(as.list(no_negativos))
  
  ## POSIBILIDAD CON FILTER 
  # resultado <- Filter(function(x) x >= 0, value_list)
  # return(resultado)
}

# Cuestión 4
# 
# Defina una función que agrupe palabras por su primera letra.
# 
# Output: diccionario cuya key es la letra y su valor es la lista de strings

agrupar_por_letra <- function(value_list) {
  
  # Comprobación de valores usando input_check
  input_check(value_list)
  
  palabras <- unlist(value_list)
  
  # Creamos una lista vacía que actuará como nuestro diccionario
  diccionario <- list()
  
  # Obtenemos las letras iniciales únicas para crear las "keys"
  # Extraemos el primer carácter y pasamos a minúscula para agrupar bien
  primeras_letras <- tolower(substr(palabras, 1, 1))
  letras_unicas <- unique(primeras_letras)
  
  for (letra in letras_unicas) {
    # Filtramos las palabras originales que empiezan por esa letra
    # Usamos las palabras originales para mantener mayúsculas/acentos si los hubiera
    palabras_grupo <- palabras[primeras_letras == letra]
    
    # Asignamos al diccionario en la clave correspondiente
    diccionario[[letra]] <- as.list(palabras_grupo)
  }
  
  return(diccionario)
}

# Cuestión 5
# 
# Defina una función que devuelva los múltiplos de 3 no negativos
# 
# Output de la función: lista de números (múltiplos de 3 y >= 0)

# SIN REUTILIZAR LA FUNCION DE LA CUESTION 3 -- lista_no_negativos <- function(value_list)
# multiplos_tres_positivos <- function(value_list) {
#   
#   # Comprobación de los valores usando el check numérico ya definido
#   input_check_num(value_list)
#   
#   # Convertimos a vector para operar de forma eficiente
#   vector_numeros <- unlist(value_list)
#   
#   # Aplicamos doble filtro: 
#   # 1. Múltiplo de 3 (resto de división %% es 0)
#   # 2. No negativo (>= 0)
#   resultado_vector <- vector_numeros[vector_numeros %% 3 == 0 & vector_numeros >= 0]
#   
#   # Devolvemos el resultado como una lista
#   return(as.list(resultado_vector))
# }

# Mejor llamo a la funcion lista_no_negativos xq me va a realizar el input_check y luego me realiza el filtro de los negativos. 
multiplos_tres_positivos <- function(value_list) {
  
  # 1. LLamo a la función
  lista_limpia <- lista_no_negativos(value_list)
  
  # 2.múltiplos de 3
  vector_numeros <- unlist(lista_limpia)
  resultado_vector <- vector_numeros[vector_numeros %% 3 == 0]
  
  # Devolvemos el resultado como una lista
  return(as.list(resultado_vector))
}




