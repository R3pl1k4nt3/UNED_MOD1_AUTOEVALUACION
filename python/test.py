import unittest
from prog_basica_python import (
    concatenar, 
    lista_no_negativos, 
    multiplos_tres_positivos, 
    calculo_mediana, 
    lista_suma_pos_neg
)

class TestProgBasica(unittest.TestCase):

    # ============================================================================
    # BLOQUE: Tests Funcionales (Happy Path)
    # ============================================================================

    def test_functional_cuestion_1_concatenar(self):
        """Tests Unitarios resultados funcionales de la función concatenar"""
        
        # Caso 1: Lista estándar
        input_1 = ["Hola", "Mundo", "Python"]
        self.assertEqual(concatenar(input_1), "Hola Mundo Python")
        
        # Caso 2: Un solo elemento
        input_2 = ["Solo"]
        self.assertEqual(concatenar(input_2), "Solo")
        
        # Caso 3: Espacios internos (debe respetarlos y añadir el de unión)
        input_3 = ["Data", "Science "]
        self.assertEqual(concatenar(input_3), "Data Science ")

    def test_functional_cuestion_3_no_negativos(self):
        """Tests Unitarios resultados funcionales de la función lista_no_negativos"""
        
        # Caso 1: Mezcla de positivos y negativos
        input_1 = [10, -5, 3.5, -1.2, 0]
        # output esperado: [10, 3.5, 0]
        self.assertEqual(lista_no_negativos(input_1), [10, 3.5, 0])
        
        # Caso 2: Todos negativos (debe devolver lista vacía)
        input_2 = [-1, -10]
        self.assertEqual(lista_no_negativos(input_2), [])
        
        # Caso 3: Solo positivos
        input_3 = [1, 2]
        self.assertEqual(lista_no_negativos(input_3), [1, 2])

    def test_functional_cuestion_5_multiplos_tres(self):
        """Tests Unitarios resultados funcionales de la función multiplos_tres_positivos"""
        
        # Caso 1: Múltiplos positivos, negativos y no múltiplos
        # 9 (sí), -3 (no por signo), 4 (no por valor), 0 (sí), 6 (sí)
        input_1 = [9, -3, 4, 0, 6]
        self.assertEqual(multiplos_tres_positivos(input_1), [9, 0, 6])
        
        # Caso 2: Ningún múltiplo válido
        self.assertEqual(multiplos_tres_positivos([1, 2, 4, 5, -9]), [])

    def test_functional_cuestion_6_mediana(self):
        """Tests Unitarios resultados funcionales de la función calculo_mediana"""
        
        # Caso 1: Impar (valor central) -> 1, 3, 5 -> Mediana 3
        self.assertEqual(calculo_mediana([5, 1, 3]), 3)
        
        # Caso 2: Par (promedio centrales) -> 1, 2, 4, 10 -> Mediana 3.0
        self.assertEqual(calculo_mediana([1, 4, 2, 10]), 3.0)
        
        # Caso 3: Decimales
        self.assertEqual(calculo_mediana([1.5, 2.5, 3.5]), 2.5)

    def test_functional_cuestion_7_suma_pos_neg(self):
        """Tests Unitarios resultados funcionales de la función lista_suma_pos_neg"""
        
        # Caso 1: Mezcla normal
        # Positivos: 10 + 0 + 8 = 18
        # Negativos: -5 + (-1) = -6
        input_1 = [10, -5, 0, -1, 8]
        output_1 = lista_suma_pos_neg(input_1)
        
        self.assertIsInstance(output_1, dict)
        # Verificamos que los valores sean listas con la suma dentro
        self.assertEqual(output_1["positivos"], [18])
        self.assertEqual(output_1["negativos"], [-6])
        
        # Caso 2: Solo negativos (Positivos debe ser [0])
        output_2 = lista_suma_pos_neg([-2, -4])
        self.assertEqual(output_2["positivos"], [0])
        self.assertEqual(output_2["negativos"], [-6])

    # ============================================================================
    # BLOQUE: Tests Gestión de Errores (Sad Path)
    # ============================================================================

    def test_errors_input_check_strings(self):
        """Tests Unitarios gestión de errores para concatenar (Strings)"""
        
        # Error 1: Entrada no es lista
        # Usamos regex para asegurar que el mensaje coincide
        self.assertRaisesRegex(
            TypeError, 
            "La entrada de la función debe ser una lista", 
            concatenar, 
            "no soy lista"
        )
        
        # Error 2: Lista vacía
        self.assertRaisesRegex(
            ValueError, 
            "La lista de entrada no puede estar vacía", 
            concatenar, 
            []
        )
        
        # Error 3: Elementos no strings
        # Python empieza en índice 0, pero ajustamos el mensaje a humano (índice + 1)
        # Aquí el fallo está en índice 1 (el número 123) -> mensaje debe decir '2'
        input_mix = ["Texto", 123, "Otro"]
        self.assertRaisesRegex(
            TypeError,
            "Todos los elementos deben ser strings. Fallo en índices: 2",
            concatenar,
            input_mix
        )

    def test_errors_input_check_numeros(self):
        """Tests Unitarios gestión de errores para funciones numéricas (Cuestiones 3, 5, 6, 7)"""
        
        # Usaremos lista_no_negativos como proxy para probar el input_check_num,
        # ya que todas las funciones numéricas lo usan internamente.
        
        # Error 1: Entrada no es lista
        self.assertRaisesRegex(
            TypeError, 
            "La entrada de la función debe ser una lista", 
            lista_no_negativos, 
            (1, 2, 3)
        )
        
        # Error 2: Lista vacía
        self.assertRaisesRegex(
            ValueError, 
            "La lista de entrada no puede estar vacía", 
            lista_no_negativos, 
            []
        )
        
        # Error 3: Elementos no numéricos
        # Fallo en índice 1 (string "error") -> mensaje debe decir '2'
        input_err = [10, "error", 5]
        self.assertRaisesRegex(
            TypeError,
            "Todos los elementos deben ser números. Fallo en índices: 2",
            lista_no_negativos,
            input_err
        )
        
        # Verificamos que cálculo_mediana también hereda esta protección
        self.assertRaisesRegex(
            TypeError,
            "Fallo en índices: 2",
            calculo_mediana,
            input_err
        )

if __name__ == "__main__":
    unittest.main()