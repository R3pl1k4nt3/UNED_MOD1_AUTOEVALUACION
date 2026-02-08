#### PREPROCESADO EN R ###
##DATOS PREVIOS ###
# Columnas
# 
# pH: Nivel de pH del agua
# Hardness: Dureza del agua, una medida del contenido mineral
# Solids: Sólidos totales disueltos en el agua
# Chloramines: Concentración de cloraminas en el agua
# Sulfate: Concentración de sulfatos en el agua
# Conductivity: Conductividad eléctrica del agua
# Organic_carbon: Contenido de carbono orgánico en el agua
# Trihalomethanes: Concentración de trihalometanos en el agua
# Turbidity: Nivel de turbidez, una medida de la claridad del agua
# Potability: Variable objetivo, indica la potabilidad del agua con valores 1 (potable) y 0 (no potable)



install.packages("tidyverse")
install.packages("solitude")
install.packages("caret")
if (!require("remotes")) install.packages("remotes")

### CARGO LIBRERIAS ###

library(tidyverse)      # Manipulación de datos y gráficos (ggplot2, dplyr)
library(skimr)          # Resumen estadístico detallado
library(corrplot)       # Visualización de correlaciones
library(mice)           # Imputación de datos (Multivariate Imputation via Chained Equations)
library(solitude)       # Detección de outliers (Isolation Forest)
library(UBL)            # Balanceo de clases (SMOTE)
library(caret)          # Para preprocesado (escalado y normalización)
library(scales)         # Para formatos de porcentajes en gráficos

### CARGO DATASET (el csv de aguas) ###

setwd("C:/Users/Alex/Documents/CURSO_EST/modulo 1/UNED_MOD1_AUTOEVALUACION/datos/datos/cuestion-2")
water <- read.csv("water_potability.csv",stringsAsFactors = TRUE)

# VARIABLE OBJETIVO -- Potability: Variable objetivo, indica la potabilidad del agua con valores 1 (potable) y 0 (no potable)

water$Potability <- as.factor(water$Potability) # La transformo como factor (es booleano)
levels(water$Potability) <- c("No Potable", "Potable")

##1- Análisis descriptivo y gráfico de las variables (univariante y bivariante)
##1.1. UNIVARIANTE
## ANALISIS ESTADISTICO NUMERICO
skim(water) 
## SACAMOS N-MISSING
# ph (491 - 15%) [ELIMINO???] - sulfate (781 - 23.8%)[[ELIMINAMOS LA VARIABLE]] - trihalomethanes (162 - 4.9%) 
# Toca normalizar, hay valores muy diferentes entre las variables

#ANALISIS GRAFICO
# Tabla de frecuencias con porcentajes
datos_potabilidad <- water %>%
  group_by(Potability) %>%
  summarise(Conteo = n()) %>%
  mutate(Porcentaje = Conteo / sum(Conteo),
         Etiqueta = paste0(Conteo, "\n(", percent(Porcentaje, accuracy = 0.1), ")"))

# Gráfico de barras
ggplot(datos_potabilidad, aes(x = Potability, y = Conteo, fill = Potability)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = Etiqueta), vjust = -0.25, fontface = "bold") +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4")) +
  labs(title = "Distribución de la Variable Objetivo (Potability)",
       y = "Frecuencia", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# ANALISIS GRAFICO DE LAS VARIABLES NUMERICA

# Transformamos los datos a formato largo para graficar todo junto con 'facet_wrap'
water_long <- water %>%
  select(-Potability) %>% # Excluimos la variable categórica
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Histogramas múltiples
ggplot(water_long, aes(x = Valor)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Distribución de Variables Numéricas",
       subtitle = "La línea roja muestra la densidad estimada",
       y = "Densidad", x = "Valor") +
  theme_minimal()


## 2. ANALISIS BIVARIANTE

water_long_con_clase <- water %>%
  pivot_longer(cols = -Potability, names_to = "Variable", values_to = "Valor")

ggplot(water_long_con_clase, aes(x = Potability, y = Valor, fill = Potability)) +
  geom_boxplot(alpha = 0.8, outlier.colour = "black", outlier.size = 1.5) +
  facet_wrap(~Variable, scales = "free_y") +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4")) +
  labs(title = "Comportamiento de las Variables según Potabilidad",
       subtitle = "Comparación de medianas y detección de outliers",
       x = "", y = "Valor") +
  theme_minimal() +
  theme(legend.position = "none")
## VALORES MUY SIMILARES no me aportan nada. No existe una "variable" que separe el agua potable de la que no lo es

## BUSQUEDA DE CORRELACIONES ENTRE VARIABLES

# Matriz de correlación
cor_matrix <- cor(water %>% select_if(is.numeric), use = "complete.obs")
##Las variables químicas son muy independientes entre sí

# Visualización gráfica con corrplot
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black", # Añadir coeficientes numéricos
         tl.col = "black", 
         number.cex = 0.7,
         title = "Matriz de Correlaciones", 
         mar = c(0,0,2,0))

## Grafico de dispersion

ggplot(water, aes(x = ph, y = Hardness, color = Potability)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("#FF6B6B", "#4ECDC4")) +
  labs(title = "Relación entre pH y Dureza",
       subtitle = "¿Existen agrupaciones naturales separadas por potabilidad?") +
  theme_minimal()



#### Parte 2: Tratamiento de Datos Ausentes y Outliers ######

# --- 1.1 Eliminación de Variables con >15% de Nulos ---
# Eliminamos 'Sulfate' (tenía ~23.8% de nulos)
water_clean <- water %>% 
  select(-Sulfate)

cat("Variable 'Sulfate' eliminada.\n")

# --- 1.2 Imputación de Variables Restantes ---
# Usamos MICE con el método PMM (Predictive Mean Matching)
# Esto rellena los huecos en 'ph' y 'Trihalomethanes' respetando su distribución original.

# m=1 genera un solo dataset imputado (suficiente para este ejercicio)
# seed=123 asegura que el resultado sea reproducible
imputacion <- mice(water_clean, m = 1, method = 'pmm', seed = 123, print = FALSE)

# Generamos el dataset completo
water_imputed <- complete(imputacion)

# Verificacion
cat("Número total de nulos tras imputación:", sum(is.na(water_imputed)), "\n")

## 2. Revisión de Valores Atípicos (Outliers)

# nº outliers tiene cada columna numérica
detectar_outliers_iqr <- function(x) {
  return(length(boxplot.stats(x)$out))
}

# Aplicamos la función a todas las columnas numéricas
outliers_por_columna <- sapply(water_imputed %>% select_if(is.numeric), detectar_outliers_iqr)
print(outliers_por_columna)

# Detección Multivariante (Isolation Forest)
# Busco filas raras considerando todas las variables a la vez 

# 1. Inicio el algoritmo Isolation Forest
# (Tengo que excluir la variable objetivo 'Potability' del análisis)
iso <- isolationForest$new(sample_size = nrow(water_imputed))
iso$fit(water_imputed %>% select(-Potability))

# 2. Predecimos los "scores" de anomalía
# Un score cercano a 1 indica alta probabilidad de ser una anomalía
scores <- iso$predict(water_imputed %>% select(-Potability))

# 3. Visualizamos el histograma para decidir dónde cortar
hist(scores$anomaly_score, 
     main = "Distribución de Scores de Anomalía", 
     xlab = "Score de Anomalía (Más alto = Más raro)", 
     col = "steelblue", border = "white")

# 4. Filtrado de Outliers
# Definimos un umbral (ej. 0.65). Los registros con score mayor se consideran ruido.
water_no_outliers <- water_imputed %>%
  mutate(anomaly_score = scores$anomaly_score) %>%
  filter(anomaly_score < 0.65) %>% # Nos quedamos con los datos "normales"
  select(-anomaly_score) # Eliminamos la columna auxiliar

cat("Registros originales:", nrow(water), "\n")
cat("Registros tras limpieza:", nrow(water_no_outliers), "\n")
cat("Se han eliminado", nrow(water) - nrow(water_no_outliers), "registros anómalos.\n")

### Conclusión: Esto confirma que tener muchos sólidos disueltos o una dureza alta no es necesariamente un error, sino una característica del agua en esa zona.
# Los 13 registros eliminados son probablemente errores de medición o combinaciones químicamente imposibles (ej. un pH extremadamente ácido con una alcalinidad incompatible) que sí podrían confundir al modelo matemático.


################# SMOTE ###############
########################################
#######################################

# Definimos el pre-procesado (centrar y escalar)
# Solo aplicamos esto a las columnas numéricas, excluimos la variable objetivo 'Potability'
preproc_params <- preProcess(water_no_outliers %>% select(-Potability), 
                             method = c("center", "scale"))

# Aplicamos la transformación a nuestros datos limpios
water_scaled <- predict(preproc_params, water_no_outliers)

# Volvemos a añadir la columna Potability (que caret a veces excluye o desordena)
water_scaled$Potability <- water_no_outliers$Potability

# Verificación rápida: Ahora la media de Solids debería ser casi 0
summary(water_scaled$Solids)

# Verificamos el desbalanceo ANTES de SMOTE
print("Conteo antes de SMOTE:")
print(table(water_scaled$Potability))

# Aplicamos SMOTE
# C.perc = "balance" indica que queremos equilibrar totalmente las clases
# dist = "Euclidean" es la métrica de distancia estándar
set.seed(123) # Para que el resultado sea reproducible
water_balanced <- SmoteClassif(Potability ~ ., 
                               water_scaled, 
                               C.perc = "balance", 
                               dist = "Euclidean")

# Verificamos el desbalanceo DESPUÉS de SMOTE
print("Conteo después de SMOTE:")
print(table(water_balanced$Potability))

# Gráfico final de equilibrio
ggplot(water_balanced, aes(x = Potability, fill = Potability)) +
  geom_bar() +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4")) +
  labs(title = "Distribución Final tras SMOTE",
       subtitle = "El dataset está ahora limpio, normalizado y equilibrado",
       y = "Número de Muestras") +
  theme_minimal() +
  theme(legend.position = "none")


write.csv(water_balanced, "water_potability_balanced.csv", row.names = FALSE)