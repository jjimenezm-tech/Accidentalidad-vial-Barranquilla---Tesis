# ============================================================
# ANÁLISIS DE MULTICOLINEALIDAD
# VIF + GVIF
# VARIABLES CANDIDATAS AL MODELO
# ============================================================


# ============================================================
# 1. PAQUETES
# ============================================================

library(readxl)
library(dplyr)
library(car)


# ============================================================
# 2. CARGA DE LA BASE DE DATOS
# ============================================================

if (exists("BASE_DATOS_ACCIDENTES_TRANSITO_BARRANQUILLA_2018_2025")) {
  
  df <- BASE_DATOS_ACCIDENTES_TRANSITO_BARRANQUILLA_2018_2025
  
} else {
  
  df <- read_excel(
    "BASE_DATOS_ACCIDENTES_TRANSITO_BARRANQUILLA_2018_2025.xlsx"
  )
}


cat("============================================\n")
cat("BASE DE DATOS\n")
cat("============================================\n")

cat(
  "Filas:",
  nrow(df),
  "\nColumnas:",
  ncol(df),
  "\n"
)


# ============================================================
# 3. TRATAMIENTO DE LA HORA
# ============================================================

df <- df %>%
  mutate(
    Hora_num = as.integer(
      format(Hora, "%H")
    )
  )


cat("\n")
cat("============================================\n")
cat("VERIFICACIÓN DE HORA\n")
cat("============================================\n")

cat("Primeras horas registradas:\n")
print(head(df$Hora_num, 20))

cat("\nRango de horas:\n")
print(range(df$Hora_num, na.rm = TRUE))


# ============================================================
# 4. CREACIÓN DE FRANJA HORARIA
# ============================================================

df <- df %>%
  mutate(
    Franja_Horaria = case_when(
      
      Hora_num >= 0 & Hora_num <= 5 ~
        "Madrugada",
      
      Hora_num >= 6 & Hora_num <= 11 ~
        "Mañana",
      
      Hora_num >= 12 & Hora_num <= 17 ~
        "Tarde",
      
      Hora_num >= 18 & Hora_num <= 23 ~
        "Noche",
      
      TRUE ~ NA_character_
    )
  )


df$Franja_Horaria <- factor(
  df$Franja_Horaria,
  levels = c(
    "Madrugada",
    "Mañana",
    "Tarde",
    "Noche"
  )
)


cat("\n")
cat("============================================\n")
cat("FRANJA HORARIA\n")
cat("============================================\n")

print(
  table(
    df$Franja_Horaria,
    useNA = "ifany"
  )
)


# ============================================================
# 5. AGRUPACIÓN DE TIPO DE VÍA PARA GVIF
# ============================================================
#
# Las categorías de baja frecuencia se agrupan en:
#
# "Otra vía / no clasificada"
#
# Se agrupan:
#
# Calle 30
# Otra vía / no clasificada
# Vía Juan Mina
# Avenida del Río
# Transversal
# Avenida Murillo
# Diagonal
# Parqueadero
# Puente Pumarejo
# Vía Galapa
# Avenida Ciudad Caribe
# Vía Prosperidad
#
# La variable original Tipo_via NO se modifica.
# ============================================================

df <- df %>%
  mutate(
    
    Tipo_via_GVIF = case_when(
      
      Tipo_via %in% c(
        "Calle 30",
        "Otra vía / no clasificada",
        "Vía Juan Mina",
        "Avenida del Río",
        "Transversal",
        "Avenida Murillo",
        "Diagonal",
        "Parqueadero",
        "Puente Pumarejo",
        "Vía Galapa",
        "Avenida Ciudad Caribe",
        "Vía Prosperidad"
      ) ~ "Otra vía / no clasificada",
      
      TRUE ~ Tipo_via
    )
  )


cat("\n")
cat("============================================\n")
cat("TIPO DE VÍA AGRUPADO PARA GVIF\n")
cat("============================================\n")

print(
  sort(
    table(df$Tipo_via_GVIF),
    decreasing = TRUE
  )
)


# ============================================================
# 6. CONVERTIR VARIABLES CATEGÓRICAS A FACTOR
# ============================================================

df <- df %>%
  mutate(
    Mes = as.factor(Mes),
    Día_Semana = as.factor(Día_Semana),
    Franja_Horaria = as.factor(Franja_Horaria),
    Clase_Accidente = as.factor(Clase_Accidente),
    Tipo_via_GVIF = as.factor(Tipo_via_GVIF)
  )


# ============================================================
# 7. VARIABLES CUANTITATIVAS CANDIDATAS AL MODELO
# ============================================================

variables_numericas <- c(
  "Año",
  "Veh_Motocicleta",
  "Veh_Automovil",
  "Veh_TransportePublico",
  "Veh_Carga",
  "Veh_NoMotorizado",
  "Veh_Otros"
)


# ============================================================
# 8. VARIABLES CATEGÓRICAS CANDIDATAS AL MODELO
# ============================================================

variables_categoricas <- c(
  "Mes",
  "Día_Semana",
  "Franja_Horaria",
  "Clase_Accidente",
  "Tipo_via_GVIF"
)


# ============================================================
# 9. TODAS LAS VARIABLES CANDIDATAS
# ============================================================

variables_multicolinealidad <- c(
  variables_numericas,
  variables_categoricas
)


cat("\n")
cat("============================================\n")
cat("VARIABLES CANDIDATAS AL MODELO\n")
cat("============================================\n")

print(variables_multicolinealidad)


# ============================================================
# 10. REVISAR DATOS FALTANTES
# ============================================================

cat("\n")
cat("============================================\n")
cat("DATOS FALTANTES\n")
cat("============================================\n")

tabla_na <- data.frame(
  Variable = variables_multicolinealidad,
  Cantidad_NA = sapply(
    df[variables_multicolinealidad],
    function(x) sum(is.na(x))
  )
)

print(tabla_na)


# ============================================================
# 11. CREAR BASE FINAL PARA VIF + GVIF
# ============================================================

datos_multi <- df %>%
  select(
    all_of(variables_multicolinealidad)
  ) %>%
  na.omit()


cat("\n")
cat("============================================\n")
cat("BASE FINAL PARA VIF + GVIF\n")
cat("============================================\n")

cat(
  "Observaciones utilizadas:",
  nrow(datos_multi),
  "\n"
)

cat(
  "Variables utilizadas:",
  ncol(datos_multi),
  "\n"
)


# ============================================================
# 12. PORCENTAJE DE OBSERVACIONES UTILIZADAS
# ============================================================

porcentaje_utilizado <-
  100 * nrow(datos_multi) / nrow(df)

porcentaje_excluido <-
  100 - porcentaje_utilizado


cat("\n")
cat("============================================\n")
cat("COBERTURA DE LA BASE\n")
cat("============================================\n")

cat(
  "Observaciones originales:",
  nrow(df),
  "\n"
)

cat(
  "Observaciones utilizadas:",
  nrow(datos_multi),
  "\n"
)

cat(
  "Porcentaje utilizado:",
  round(porcentaje_utilizado, 2),
  "%\n"
)

cat(
  "Porcentaje excluido:",
  round(porcentaje_excluido, 2),
  "%\n"
)


# ============================================================
# 13. VERIFICAR ESTRUCTURA DE LAS VARIABLES
# ============================================================

cat("\n")
cat("============================================\n")
cat("ESTRUCTURA DE LAS VARIABLES\n")
cat("============================================\n")

str(datos_multi)


# ============================================================
# 14. MODELO AUXILIAR PARA LA EVALUACIÓN INTEGRAL
# ============================================================

datos_multi$Respuesta_aux <- seq_len(
  nrow(datos_multi)
)


formula_multi <- as.formula(
  paste(
    "Respuesta_aux ~",
    paste(
      paste0(
        "`",
        variables_multicolinealidad,
        "`"
      ),
      collapse = " + "
    )
  )
)


modelo_multi <- lm(
  formula_multi,
  data = datos_multi
)


# ============================================================
# 15. CALCULAR VIF / GVIF
# ============================================================

resultado_vif_gvif <- car::vif(
  modelo_multi
)


cat("\n")
cat("============================================\n")
cat("RESULTADOS VIF / GVIF\n")
cat("============================================\n")

print(resultado_vif_gvif)


# ============================================================
# 16. CONVERTIR RESULTADOS A TABLA
# ============================================================

tabla_vif_gvif <- data.frame(
  Variable = rownames(resultado_vif_gvif),
  resultado_vif_gvif,
  row.names = NULL,
  check.names = FALSE
)


# ============================================================
# 17. IDENTIFICAR TIPO DE VARIABLE
# ============================================================

tabla_vif_gvif <- tabla_vif_gvif %>%
  mutate(
    Tipo = case_when(
      
      Variable %in% variables_numericas ~
        "Cuantitativa",
      
      Variable %in% variables_categoricas ~
        "Categórica",
      
      TRUE ~
        "No evaluada"
    )
  )


# ============================================================
# 18. CREAR TABLA UNIFICADA
# ============================================================

tabla_final_multi <- data.frame(
  Variable = tabla_vif_gvif$Variable,
  Tipo = tabla_vif_gvif$Tipo,
  VIF = NA_real_,
  GVIF = NA_real_,
  Df = NA_integer_,
  GVIF_ajustado = NA_real_
)


# ============================================================
# 19. COLOCAR VIF PARA VARIABLES CUANTITATIVAS
# ============================================================

filas_vif <- tabla_vif_gvif$Tipo == "Cuantitativa"

tabla_final_multi$VIF[filas_vif] <-
  tabla_vif_gvif$GVIF[filas_vif]


# ============================================================
# 20. COLOCAR GVIF PARA VARIABLES CATEGÓRICAS
# ============================================================

filas_gvif <- tabla_vif_gvif$Tipo == "Categórica"

tabla_final_multi$GVIF[filas_gvif] <-
  tabla_vif_gvif$GVIF[filas_gvif]

tabla_final_multi$Df[filas_gvif] <-
  tabla_vif_gvif$Df[filas_gvif]

tabla_final_multi$GVIF_ajustado[filas_gvif] <-
  tabla_vif_gvif$`GVIF^(1/(2*Df))`[filas_gvif]


# ============================================================
# 21. REDONDEAR RESULTADOS
# ============================================================

tabla_final_multi <- tabla_final_multi %>%
  mutate(
    VIF = round(VIF, 3),
    GVIF = round(GVIF, 3),
    GVIF_ajustado = round(GVIF_ajustado, 3)
  )


# ============================================================
# 22. CAMBIAR NOMBRES PARA PRESENTACIÓN
# ============================================================

tabla_final_multi$Variable[
  tabla_final_multi$Variable == "Día_Semana"
] <- "Día de la semana"

tabla_final_multi$Variable[
  tabla_final_multi$Variable == "Franja_Horaria"
] <- "Franja horaria"

tabla_final_multi$Variable[
  tabla_final_multi$Variable == "Clase_Accidente"
] <- "Clase de accidente"

tabla_final_multi$Variable[
  tabla_final_multi$Variable == "Tipo_via_GVIF"
] <- "Tipo de vía"

tabla_final_multi$Variable[
  tabla_final_multi$Variable == "Veh_Motocicleta"
] <- "Motocicleta"

tabla_final_multi$Variable[
  tabla_final_multi$Variable == "Veh_Automovil"
] <- "Automóvil"

tabla_final_multi$Variable[
  tabla_final_multi$Variable == "Veh_TransportePublico"
] <- "Transporte público"

tabla_final_multi$Variable[
  tabla_final_multi$Variable == "Veh_Carga"
] <- "Vehículos de carga"

tabla_final_multi$Variable[
  tabla_final_multi$Variable == "Veh_NoMotorizado"
] <- "Vehículos no motorizados"

tabla_final_multi$Variable[
  tabla_final_multi$Variable == "Veh_Otros"
] <- "Otros vehículos"


# ============================================================
# 23. ORDENAR TABLA FINAL
# ============================================================

orden_presentacion <- c(
  "Año",
  "Motocicleta",
  "Automóvil",
  "Transporte público",
  "Vehículos de carga",
  "Vehículos no motorizados",
  "Otros vehículos",
  "Mes",
  "Día de la semana",
  "Franja horaria",
  "Clase de accidente",
  "Tipo de vía"
)


tabla_final_multi <- tabla_final_multi %>%
  mutate(
    Variable = factor(
      Variable,
      levels = orden_presentacion
    )
  ) %>%
  arrange(Variable) %>%
  mutate(
    Variable = as.character(Variable)
  )


# ============================================================
# 24. TABLA FINAL DE MULTICOLINEALIDAD
# ============================================================

cat("\n")
cat("====================================================\n")
cat("TABLA FINAL DE MULTICOLINEALIDAD\n")
cat("====================================================\n")

print(tabla_final_multi)


# ============================================================
# 25. TABLA VIF - VARIABLES CUANTITATIVAS
# ============================================================

tabla_vif_final <- tabla_final_multi %>%
  filter(
    Tipo == "Cuantitativa"
  ) %>%
  select(
    Variable,
    VIF
  )


cat("\n")
cat("====================================================\n")
cat("TABLA VIF - VARIABLES CUANTITATIVAS\n")
cat("====================================================\n")

print(tabla_vif_final)


# ============================================================
# 26. TABLA GVIF - VARIABLES CATEGÓRICAS
# ============================================================

tabla_gvif_final <- tabla_final_multi %>%
  filter(
    Tipo == "Categórica"
  ) %>%
  select(
    Variable,
    GVIF,
    Df,
    GVIF_ajustado
  )


cat("\n")
cat("====================================================\n")
cat("TABLA GVIF - VARIABLES CATEGÓRICAS\n")
cat("====================================================\n")

print(tabla_gvif_final)


# ============================================================
# 27. VERIFICACIÓN DE MULTICOLINEALIDAD
# ============================================================

cat("\n")
cat("====================================================\n")
cat("VERIFICACIÓN DE MULTICOLINEALIDAD\n")
cat("====================================================\n")

cat("\nVariables cuantitativas con VIF >= 5:\n")

print(
  tabla_vif_final %>%
    filter(
      VIF >= 5
    )
)


cat("\nVariables categóricas con GVIF ajustado >= 5:\n")

print(
  tabla_gvif_final %>%
    filter(
      GVIF_ajustado >= 5
    )
)


# ============================================================
# 28. VARIABLES SIN INDICIOS DE MULTICOLINEALIDAD
# ============================================================

variables_vif_sin_problema <- tabla_vif_final %>%
  filter(
    VIF < 5
  )


variables_gvif_sin_problema <- tabla_gvif_final %>%
  filter(
    GVIF_ajustado < 5
  )


cat("\n")
cat("====================================================\n")
cat("VARIABLES SIN INDICIOS DE MULTICOLINEALIDAD\n")
cat("====================================================\n")

cat("\nVariables cuantitativas:\n")
print(variables_vif_sin_problema)

cat("\nVariables categóricas:\n")
print(variables_gvif_sin_problema)


# ============================================================
# 29. RESUMEN FINAL
# ============================================================

cat("\n")
cat("====================================================\n")
cat("RESUMEN FINAL\n")
cat("====================================================\n")

cat(
  "Observaciones originales:",
  nrow(df),
  "\n"
)

cat(
  "Observaciones analizadas:",
  nrow(datos_multi),
  "\n"
)

cat(
  "Porcentaje utilizado:",
  round(porcentaje_utilizado, 2),
  "%\n"
)

cat(
  "Variables candidatas evaluadas:",
  length(variables_multicolinealidad),
  "\n"
)

cat(
  "Variables cuantitativas evaluadas:",
  nrow(tabla_vif_final),
  "\n"
)

cat(
  "Variables categóricas evaluadas:",
  nrow(tabla_gvif_final),
  "\n"
)

cat(
  "Variables con VIF >= 5:",
  sum(
    tabla_vif_final$VIF >= 5,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Variables con GVIF ajustado >= 5:",
  sum(
    tabla_gvif_final$GVIF_ajustado >= 5,
    na.rm = TRUE
  ),
  "\n"
)


# ============================================================
# 30. MAYORES VALORES DE MULTICOLINEALIDAD
# ============================================================

cat("\n")
cat("====================================================\n")
cat("MAYORES VALORES DE MULTICOLINEALIDAD\n")
cat("====================================================\n")

cat("\nMayor VIF:\n")

print(
  tabla_vif_final %>%
    arrange(desc(VIF)) %>%
    slice(1)
)


cat("\nMayor GVIF ajustado:\n")

print(
  tabla_gvif_final %>%
    arrange(desc(GVIF_ajustado)) %>%
    slice(1)
)


# ============================================================
# 31. TABLAS DE RESULTADOS
# ============================================================
#
# ============================================================

cat("\n\n")
cat("####################################################\n")
cat("# TABLA PARA TESIS: VIF VARIABLES CUANTITATIVAS\n")
cat("####################################################\n\n")

print(
  tabla_vif_final
)


cat("\n\n")
cat("####################################################\n")
cat("# TABLA PARA TESIS: GVIF VARIABLES CATEGÓRICAS\n")
cat("####################################################\n\n")

print(
  tabla_gvif_final
)


# ============================================================
# FIN DEL ANÁLISIS
# ============================================================