# EJERCICIOS OBLIGATORIOS - MÓDULO 1

## 1. PROGRAMACIÓN BÁSICA (5 puntos)

**Requisitos Generales:**
> * Para todas las cuestiones, deben realizarse **test unitarios** haciendo uso de la librería `testthat`.
> * Si se realiza la parte opcional de Python, puede usarse indistintamente `unittest` o `pytest`.
> * **Nota sobre R:** En R no existe el tipo de dato "diccionario". Se entenderá que la función debe devolver una **lista nombrada (list)**, donde las *keys* son los nombres de los elementos y los *values* los valores almacenados.

### Cuestión 1
Defina una función que concatene una lista de palabras en una única frase.
* **Output:** string que sea el resultado de la frase obtenida.

### Cuestión 2
A partir de una lista de palabras, defina una función que encuentre la palabra con más vocales. Si hay más de una debe devolverse todas aquellas palabras que cumplan la condición.
* **Output:** lista de strings con las palabras que cumplan la condición.

### Cuestión 3
A partir de una lista de números, defina una función que elimine todos los números negativos.
* **Output:** lista de números sin valores negativos.

### Cuestión 4
Defina una función que agrupe palabras por su primera letra.
* **Output:** diccionario cuya key es la letra y su clave es la lista de strings/palabras que cumplen la condición.

### Cuestión 5
Defina una función que, dada una lista de números, devuelva una lista solo con los múltiplos de 3.
* **Output:** lista de números sin valores negativos (según enunciado).

### Cuestión 6
Defina una función que, dada una lista de números, devuelva la mediana.
* **Output:** dato numérico.

### Cuestión 7
Defina una función que, dada una lista de números, devuelva la suma de los números positivos y la de los negativos por separado.
* **Output:** diccionario con dos keys cuyos valores sean listas de números (positivos y negativos, respectivamente).

### Cuestión 8
Defina una función que, dada una lista de números, devuelva la suma de los números positivos y la de los negativos por separado.
* **Output:** diccionario con dos keys cuyos valores sean listas de números (positivos y negativos, respectivamente).

### Cuestión 9
Defina una función que reciba una palabra y notifique las vocales existentes en dicha palabra.
* **Output:** diccionario con cinco keys (máximo número de vocales posibles). El valor de cada clave será `true` o `false` según corresponda.

### Cuestión 10
Defina una función que elimine las palabras repetidas de una lista.
* **Output:** lista con las palabras sin repetir.

---

## 2. PREPROCESADO DE DATOS (5 puntos)

**Dataset:** `water_potability.csv`

El conjunto de datos contiene mediciones y evaluaciones de la calidad del agua para determinar si es apta para el consumo humano.

**Columnas:**
* `pH`: Nivel de pH del agua.
* `Hardness`: Dureza del agua.
* `Solids`: Sólidos totales disueltos.
* `Chloramines`: Concentración de cloraminas.
* `Sulfate`: Concentración de sulfatos.
* `Conductivity`: Conductividad eléctrica.
* `Organic_carbon`: Contenido de carbono orgánico.
* `Trihalomethanes`: Concentración de trihalometanos.
* `Turbidity`: Nivel de turbidez (claridad).
* `Potability`: Variable objetivo (1 = potable, 0 = no potable).

**Se solicita:**
1.  **Análisis descriptivo y gráfico** de las variables (univariante y bivariante).
2.  **Revisión de valores atípicos** (outliers) siguiendo métodos univariantes y multivariantes.
3.  **Imputación de datos ausentes** en las variables correspondientes.
4.  **Equilibrado de la muestra** sobre la variable `Potability` utilizando la técnica de SMOTE.

**Requisitos mínimos:**
* Uso de histogramas y diagramas de caja (boxplots) para la distribución.
* Gráficos de dispersión para relaciones entre variables numéricas.
* Exclusión de variables con >15% de valores nulos.
* Uso de matriz de correlaciones para plantear la imputación.

> **Nota:** Puede usarse cualquier librería de R (tidyverse, tidymodels, mlr, caret, plotly, etc.).

---

## 3. EJERCICIOS OPCIONALES: MANEJO DE DATOS EN SQL (1 punto)

**Dataset:** `education_career_success.csv`

Datos sobre 5000 estudiantes explorando la relación entre rendimiento académico, habilidades y éxito profesional.

### Descripción de Columnas

**Información del estudiante:**
* `Student_ID`, `Age`, `Gender`.

**Rendimiento académico:**
* `High_School_GPA`, `SAT_Score`, `University_Ranking`, `University_GPA`, `Field_of_Study`.

**Habilidades:**
* `Internships_Completed`, `Projects_Completed`, `Certifications`, `Soft_Skills_Score`, `Networking_Score`.

**Resultados profesionales:**
* `Job_Offers`, `Starting_Salary`, `Career_Satisfaction`, `Years_to_Promotion`, `Current_Job_Level`, `Work_Life_Balance`, `Entrepreneurship`.

### Cuestiones a resolver

1.  ¿Cuántos estudiantes hay según género?
2.  ¿Cuántos estudiantes hay por campo de estudio? ¿Y por campo de estudio y género?
3.  ¿Cuántos estudiantes hay por año de promoción?
4.  ¿Cuántos alumnos emprendieron al acabar sus estudios? ¿Y cuántos no?
5.  ¿Cuál es la media de las ofertas de trabajo para los estudiantes con más de 2 pasantías completadas?
6.  ¿Cuál es la media de la puntuación SAT para estudiantes con más de 25 años?
7.  Filtra los estudiantes que tienen un **GPA universitario mayor que 3.5**. Selecciona, junto con el campo `Student_ID`, las columnas relacionadas con los resultados profesionales.
8.  Filtra las estudiantes **mujeres** de `Computer Science` con un **SAT-Score superior a 1200**. Selecciona exclusivamente los campos `Student_ID` y `Projects_Completed`.
9.  Crea una nueva tabla con los campos de información del estudiante y una nueva columna categórica `University_GPA_bucket`:
    * **Baja:** < 3
    * **Media:** 3 a 3.5
    * **Alta:** > 3.5