install.packages("sqldf")
install.packages("data.table")
library(sqldf)
library(data.table)

setwd("C:/Users/Alex/Documents/CURSO_EST/modulo 1/UNED_MOD1_AUTOEVALUACION/datos/datos/cuestion-3")

estudiantes <- fread("info_estudiante.csv")
rendimiento   <- fread("info_rendimiento_academico.csv")
habilidades <- fread("info_habilidades_actividades_extracurriculares.csv")
resultados <- fread("info_resultados_profesionales.csv")

# --- Q1: ¿Cuántos estudiantes hay según género? ---
# Tabla: estudiantes
q1 <- sqldf('SELECT Gender, COUNT(*) as Total
             FROM estudiantes
             GROUP BY Gender')
print("--- Q1: Estudiantes por género ---")
print(q1)

# --- Q2: ¿Cuántos estudiantes por campo de estudio? ¿Y por campo y género? ---
# Parte A: Solo campo (Tabla: rendimiento)
q2_a <- sqldf('SELECT Field_of_Study, COUNT(*) as Total
               FROM rendimiento
               GROUP BY Field_of_Study')

# Parte B: Campo y Género (JOIN rendimiento + estudiantes)
q2_b <- sqldf('SELECT r.Field_of_Study, e.Gender, COUNT(*) as Total
               FROM rendimiento AS r
               INNER JOIN estudiantes AS e ON r.Student_ID = e.Student_ID
               GROUP BY r.Field_of_Study, e.Gender')

print("--- Q2: Estudiantes por campo y género (Muestra) ---")
head(q2_b)


# --- Q3: ¿Cuántos estudiantes hay por año de promoción? ---
# Tabla: resultados (variable 'resultados' en vez de 'profesional')
q3 <- sqldf('SELECT Years_to_Promotion, COUNT(*) as Total
             FROM resultados
             GROUP BY Years_to_Promotion
             ORDER BY Years_to_Promotion')
print("--- Q3: Estudiantes por años hasta la promoción ---")
print(q3)


# --- Q4: ¿Cuántos alumnos emprendieron? ¿Y cuántos no? ---
# Tabla: resultados
q4 <- sqldf('SELECT Entrepreneurship, COUNT(*) as Total
             FROM resultados
             GROUP BY Entrepreneurship')
print("--- Q4: Emprendimiento ---")
print(q4)


# --- Q5: Media de ofertas de trabajo para estudiantes con > 2 pasantías ---
# Tablas: resultados (Job_Offers) y habilidades (Internships_Completed)
q5 <- sqldf('SELECT AVG(r.Job_Offers) as Media_Ofertas
             FROM resultados AS r
             INNER JOIN habilidades AS h ON r.Student_ID = h.Student_ID
             WHERE h.Internships_Completed > 2')
print("--- Q5: Media ofertas (>2 pasantías) ---")
print(q5)


# --- Q6: Media de puntuación SAT para estudiantes con más de 25 años ---
# Tablas: rendimiento (SAT_Score) y estudiantes (Age)
q6 <- sqldf('SELECT AVG(r.SAT_Score) as Media_SAT
             FROM rendimiento AS r
             INNER JOIN estudiantes AS e ON r.Student_ID = e.Student_ID
             WHERE e.Age > 25')
print("--- Q6: Media SAT (>25 años) ---")
print(q6)


# --- Q7: Filtra GPA > 3.5 y selecciona columnas profesionales ---
# Tablas: resultados (todas las columnas) y rendimiento (University_GPA)
q7 <- sqldf('SELECT res.*
             FROM resultados AS res
             INNER JOIN rendimiento AS ren ON res.Student_ID = ren.Student_ID
             WHERE ren.University_GPA > 3.5')
print("--- Q7: Perfil profesional de estudiantes con GPA > 3.5 (Muestra) ---")
head(q7)


# --- Q8: Mujeres, Computer Science, SAT > 1200. Output: ID y Projects ---
# Tablas: estudiantes, rendimiento, habilidades
q8 <- sqldf('SELECT e.Student_ID, h.Projects_Completed
             FROM estudiantes AS e
             INNER JOIN rendimiento AS r ON e.Student_ID = r.Student_ID
             INNER JOIN habilidades AS h ON e.Student_ID = h.Student_ID
             WHERE e.Gender = "Female" 
               AND r.Field_of_Study = "Computer Science"
               AND r.SAT_Score > 1200')
print("--- Q8: Mujeres CS con SAT > 1200 ---")
head(q8)


# --- Q9: Nueva tabla con Buckets de GPA ---
# Tablas: estudiantes y rendimiento
nueva_tabla_gpa <- sqldf('SELECT e.*, 
                                 r.University_GPA,
                                 CASE 
                                    WHEN r.University_GPA < 3 THEN "Baja"
                                    WHEN r.University_GPA >= 3 AND r.University_GPA <= 3.5 THEN "Media"
                                    WHEN r.University_GPA > 3.5 THEN "Alta"
                                 END as University_GPA_bucket
                          FROM estudiantes AS e
                          INNER JOIN rendimiento AS r ON e.Student_ID = r.Student_ID')

print("--- Q9: Nueva tabla con Buckets (Muestra) ---")
head(nueva_tabla_gpa)