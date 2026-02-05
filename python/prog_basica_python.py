import statistics

def input_check(value_list):
    """Verificación para strings (Cuestión 1)"""
    if not isinstance(value_list, list):
        raise TypeError("La entrada de la función debe ser una lista")
    if len(value_list) == 0:
        raise ValueError("La lista de entrada no puede estar vacía")
    
    indices_malos = [i for i, x in enumerate(value_list) if not isinstance(x, str)]
    if indices_malos:
        # En Python sumamos 1 al índice para que el usuario vea la posición 'humana' (1, 2...)
        valores_malos = ", ".join(map(str, [i + 1 for i in indices_malos]))
        raise TypeError(f"Todos los elementos deben ser strings. Fallo en índices: {valores_malos}")

def input_check_num(value_list):
    """Verificación para números (Cuestiones 3, 5, 6, 7)"""
    if not isinstance(value_list, list):
        raise TypeError("La entrada de la función debe ser una lista")
    if len(value_list) == 0:
        raise ValueError("La lista de entrada no puede estar vacía")
    
    indices_malos = [i for i, x in enumerate(value_list) if not isinstance(x, (int, float))]
    if indices_malos:
        valores_malos = ", ".join(map(str, [i + 1 for i in indices_malos]))
        raise TypeError(f"Todos los elementos deben ser números. Fallo en índices: {valores_malos}")

# Cuestión 1
def concatenar(value_list):
    input_check(value_list)
    return " ".join(value_list)

# Cuestión 3
def lista_no_negativos(value_list):
    input_check_num(value_list)
    # Filtramos usando list comprehension (equivalente al filtrado vectorial de R)
    return [x for x in value_list if x >= 0]

# Cuestión 5
def multiplos_tres_positivos(value_list):
    # Reutilizamos la Cuestión 3 (que ya hace el input_check_num y filtra negativos)
    lista_limpia = lista_no_negativos(value_list)
    return [x for x in lista_limpia if x % 3 == 0]

# Cuestión 6
def calculo_mediana(value_list):
    input_check_num(value_list)
    return statistics.median(value_list)

# Cuestión 7
def lista_suma_pos_neg(value_list):
    input_check_num(value_list)
    
    suma_pos = sum([x for x in value_list if x >= 0])
    suma_neg = sum([x for x in value_list if x < 0])
    
    # Devolvemos diccionario con valores tipo lista, como pediste en R
    return {
        "positivos": [suma_pos],
        "negativos": [suma_neg]
    }