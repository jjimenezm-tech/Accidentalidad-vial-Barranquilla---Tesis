# ============================================================
# RANDOM FOREST
# PREDICCIÓN DE LA SEVERIDAD DE ACCIDENTES DE TRÁNSITO
# BARRANQUILLA, 2018-2025
# ============================================================

# Modelo:
#   Random Forest mediante ranger y tidymodels
#
# Variable objetivo:
#   Gravedad_Accidente
#
# Categorías:
#   - Solo daños
#   - Con heridos
#   - Con muertos
#
# Flujo:
#   1. Carga de datos
#   2. Preparación de variables
#   3. División entrenamiento/prueba
#   4. Ponderación de clases
#   5. Validación cruzada
#   6. Búsqueda de hiperparámetros
#   7. Selección del mejor modelo
#   8. Ajuste del modelo final
#   9. Evaluación sobre TEST
#  10. Selección del umbral mediante OOF
#  11. Evaluación final con umbral ajustado
#
# Semilla de reproducibilidad:
#   2026
#
# Configuración de ranger:
#   500 árboles
#   2 hilos
#   semilla 2026
# ============================================================


# ============================================================
# 1. LIBRERÍAS
# ============================================================

library(tidyverse)
library(tidymodels)
library(ranger)
library(lubridate)
library(readxl)


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


# ============================================================
# 3. VALIDACIÓN DE VARIABLES NECESARIAS
# ============================================================

columnas_requeridas <- c(
  "Hora",
  "Día_Semana",
  "Gravedad_Accidente",
  "Clase_Accidente",
  "Año",
  "Mes",
  "Tipo_via",
  "Veh_Motocicleta",
  "Veh_Automovil",
  "Veh_TransportePublico",
  "Veh_Carga",
  "Veh_NoMotorizado",
  "Veh_Otros",
  "Número_total_vehiculos"
)

faltantes <- setdiff(
  columnas_requeridas,
  names(df)
)

if (length(faltantes) > 0) {
  
  stop(
    paste(
      "Faltan las siguientes columnas en la base:",
      paste(faltantes, collapse = ", ")
    )
  )
}


# ============================================================
# 4. PREPARACIÓN DE VARIABLES TEMPORALES
# ============================================================

df <- df %>%
  mutate(
    
    Hora_hh = hour(Hora),
    
    Hora_mm = minute(Hora),
    
    Hora_Num = Hora_hh + (Hora_mm / 60),
    
    Hora_texto = sprintf(
      "%02d:%02d",
      Hora_hh,
      Hora_mm
    ),
    
    Franja_Horaria = case_when(
      
      Hora_hh >= 0 & Hora_hh < 6 ~ "Madrugada",
      
      Hora_hh >= 6 & Hora_hh < 12 ~ "Mañana",
      
      Hora_hh >= 12 & Hora_hh < 18 ~ "Tarde",
      
      Hora_hh >= 18 & Hora_hh <= 23 ~ "Noche",
      
      TRUE ~ NA_character_
    ),
    
    Fin_de_Semana = case_when(
      
      Día_Semana %in% c("Sábado", "Domingo") ~ "Sí",
      
      TRUE ~ "No"
    )
  )


# ============================================================
# 5. DEFINICIÓN DE LA VARIABLE OBJETIVO
# ============================================================

sev_orden <- c(
  "Solo daños",
  "Con heridos",
  "Con muertos"
)


# ============================================================
# 6. DEFINICIÓN DE VARIABLES PREDICTORAS
# ============================================================

predictoras <- c(
  "Año",
  "Mes",
  "Día_Semana",
  "Franja_Horaria",
  "Clase_Accidente",
  "Tipo_via",
  "Veh_Motocicleta",
  "Veh_Automovil",
  "Veh_TransportePublico",
  "Veh_Carga",
  "Veh_NoMotorizado",
  "Veh_Otros",
  "Número_total_vehiculos"
)


# ============================================================
# 7. PREPARACIÓN DEL DATASET PARA EL MODELO
# ============================================================

df_modelo <- df %>%
  select(
    all_of(predictoras),
    Gravedad_Accidente
  ) %>%
  mutate(
    
    Gravedad_Accidente = factor(
      Gravedad_Accidente,
      levels = sev_orden
    ),
    
    Año = as.numeric(Año),
    
    Mes = factor(Mes),
    
    Día_Semana = factor(Día_Semana),
    
    Franja_Horaria = factor(Franja_Horaria),
    
    Clase_Accidente = factor(Clase_Accidente),
    
    Tipo_via = factor(Tipo_via)
  )


# ============================================================
# 8. DIVISIÓN ENTRENAMIENTO / PRUEBA
# ============================================================

set.seed(2026)

split <- initial_split(
  df_modelo,
  prop = 0.80,
  strata = Gravedad_Accidente
)

train_data <- training(split)

test_data <- testing(split)


# ============================================================
# 9. CÁLCULO DE PESOS DE CLASE
# ============================================================

conteos_clase <- table(
  train_data$Gravedad_Accidente
)

pesos_clase <- sum(conteos_clase) /
  (length(conteos_clase) * conteos_clase)

pesos_clase <- as.numeric(
  pesos_clase
)

names(pesos_clase) <- names(
  conteos_clase
)


# ============================================================
# 10. ESPECIFICACIÓN DEL RANDOM FOREST
# ============================================================

rf_spec <- rand_forest(
  
  mode = "classification",
  
  trees = 500,
  
  mtry = tune(),
  
  min_n = tune()
  
) %>%
  
  set_engine(
    "ranger",
    importance = "impurity",
    class.weights = pesos_clase,
    num.threads = 2,
    seed = 2026
  )


# ============================================================
# 11. RECETA Y WORKFLOW
# ============================================================

receta_rf <- recipe(
  Gravedad_Accidente ~ .,
  data = train_data
)

workflow_rf <- workflow() %>%
  add_recipe(receta_rf) %>%
  add_model(rf_spec)


# ============================================================
# 12. VALIDACIÓN CRUZADA
# ============================================================

set.seed(2026)

folds_rf <- vfold_cv(
  train_data,
  v = 5,
  strata = Gravedad_Accidente
)


# ============================================================
# 13. GRID ALEATORIO DE HIPERPARÁMETROS
# ============================================================

set.seed(2026)

grid_rf <- grid_random(
  
  mtry(
    range = c(
      1L,
      length(predictoras)
    )
  ),
  
  min_n(
    range = c(
      2L,
      40L
    )
  ),
  
  size = 20
)


# ============================================================
# 14. MÉTRICAS Y CONTROL DE ENTRENAMIENTO
# ============================================================

metricas_rf <- metric_set(
  accuracy,
  kap,
  recall,
  precision,
  roc_auc
)

control_rf <- control_grid(
  save_pred = TRUE,
  save_workflow = TRUE,
  verbose = TRUE
)


# ============================================================
# 15. AJUSTE Y EVALUACIÓN DE HIPERPARÁMETROS
# ============================================================

set.seed(2026)

resultados_rf <- tune_grid(
  workflow_rf,
  resamples = folds_rf,
  grid = grid_rf,
  metrics = metricas_rf,
  control = control_rf
)


# ============================================================
# 16. SELECCIÓN DE LA MEJOR CONFIGURACIÓN POR F1 MACRO
# ============================================================

# Recuperar las predicciones de validación de todas las
# configuraciones y de los cinco pliegues.

predicciones_tuning_rf <- collect_predictions(resultados_rf)


# Calcular el F1 macro de cada configuración en cada pliegue.

f1_macro_config_rf <- predicciones_tuning_rf %>%
  group_by(mtry, min_n, .config, id) %>%
  group_modify(~ {
    
    metricas <- calcular_metricas_multiclase(
      truth = .x$Gravedad_Accidente,
      estimate = .x$.pred_class,
      levels_clase = sev_orden
    )
    
    tibble(
      F1_macro = mean(metricas$F1)
    )
    
  }) %>%
  ungroup()


# Promediar el F1 macro de los cinco pliegues para cada
# configuración.

resumen_f1_macro_rf <- f1_macro_config_rf %>%
  group_by(mtry, min_n, .config) %>%
  summarise(
    F1_macro_promedio = mean(F1_macro),
    Desviacion_F1_macro = sd(F1_macro),
    .groups = "drop"
  ) %>%
  arrange(desc(F1_macro_promedio))


# Seleccionar la configuración con mayor F1 macro promedio.

mejor_combo_rf <- resumen_f1_macro_rf %>%
  slice_head(n = 1) %>%
  select(mtry, min_n)


# Mostrar las cinco mejores configuraciones.

print(
  resumen_f1_macro_rf %>%
    slice_head(n = 5)
)

# Mostrar la configuración seleccionada.

print(mejor_combo_rf)
# 26. PREDICCIONES OUT-OF-FOLD PARA SELECCIÓN DEL UMBRAL
# ============================================================

# Se generan nuevas predicciones OOF usando la configuración
# seleccionada, exclusivamente dentro del conjunto de entrenamiento.

pred_cv_rf <- purrr::map2_dfr(
  seq_along(folds_rf$splits),
  folds_rf$splits,
  function(i, split_cv) {
    
    datos_entrenamiento_cv <- rsample::analysis(split_cv)
    datos_validacion_cv <- rsample::assessment(split_cv)
    
    # Entrenar con cuatro pliegues
    modelo_cv <- fit(
      wf_final_rf,
      data = datos_entrenamiento_cv
    )
    
    # Predecir probabilidades en el pliegue excluido
    probabilidades_cv <- predict(
      modelo_cv,
      new_data = datos_validacion_cv,
      type = "prob"
    )
    
    # Conservar la clase real y las probabilidades
    bind_cols(
      datos_validacion_cv %>%
        select(Gravedad_Accidente),
      probabilidades_cv
    ) %>%
      mutate(pliegue = i)
  }
)

if (nrow(pred_cv_rf) != nrow(train_data)) {
  stop("Las predicciones OOF no coinciden con el tamaño del entrenamiento.")
  
}

# ============================================================
# 27. FUNCIÓN PARA APLICAR EL UMBRAL
# ============================================================

aplicar_umbral_rf <- function(
    datos,
    umbral,
    niveles = sev_orden
) {
  
  pred_nueva <- case_when(
    
    datos$`.pred_Con muertos` >= umbral ~ "Con muertos",
    
    datos$`.pred_Solo daños` >=
      datos$`.pred_Con heridos` ~ "Solo daños",
    
    TRUE ~ "Con heridos"
  )
  
  factor(
    pred_nueva,
    levels = niveles
  )
}


# ============================================================
# 28. BÚSQUEDA DE 50 UMBRALES CON PREDICCIONES OOF
# ============================================================

umbrales_rf <- seq(
  from = 0.01,
  to = 0.50,
  by = 0.01
)

resultados_umbral_rf <- map_dfr(
  
  umbrales_rf,
  
  function(u) {
    
    pred_nueva <- aplicar_umbral_rf(
      datos = pred_cv_rf,
      umbral = u
    )
    
    metricas <- calcular_metricas_multiclase(
      truth = pred_cv_rf$Gravedad_Accidente,
      estimate = pred_nueva,
      levels_clase = sev_orden
    )
    
    clase_muertos <- metricas %>%
      filter(
        Clase == "Con muertos"
      )
    
    precision <- clase_muertos$Precision
    
    recall <- clase_muertos$Recall
    
    f2 <- ifelse(
      precision + recall > 0,
      5 * precision * recall /
        (4 * precision + recall),
      0
    )
    
    tibble(
      umbral = u,
      TP_muertos = clase_muertos$TP,
      FP_muertos = clase_muertos$FP,
      FN_muertos = clase_muertos$FN,
      precision_muertos = precision,
      recall_muertos = recall,
      F1_muertos = clase_muertos$F1,
      F2_muertos = f2,
      accuracy = mean(
        pred_nueva ==
          pred_cv_rf$Gravedad_Accidente
      ),
      precision_macro = mean(
        metricas$Precision
      ),
      recall_macro = mean(
        metricas$Recall
      ),
      F1_macro = mean(
        metricas$F1
      ),
      casos_predichos_muertos = sum(
        pred_nueva == "Con muertos"
      )
    )
  }
)


# ============================================================
# 29. SELECCIÓN DEL MEJOR UMBRAL
# ============================================================

mejor_umbral_rf <- resultados_umbral_rf %>%
  arrange(
    desc(F2_muertos),
    desc(recall_muertos),
    umbral
  ) %>%
  slice_head(n = 1)

umbral_final_rf <- mejor_umbral_rf$umbral


# ============================================================
# 30. APLICACIÓN DEL UMBRAL SELECCIONADO AL TEST
# ============================================================

pred_test_umbral_rf <- aplicar_umbral_rf(
  datos = resultados_test_rf,
  umbral = umbral_final_rf
)

resultados_test_umbral_rf <- resultados_test_rf %>%
  mutate(
    .pred_class_umbral = pred_test_umbral_rf
  )


# ============================================================
# 31. MATRIZ DE CONFUSIÓN DEL TEST CON UMBRAL AJUSTADO
# ============================================================

matriz_confusion_umbral_rf <- conf_mat(
  resultados_test_umbral_rf,
  truth = Gravedad_Accidente,
  estimate = .pred_class_umbral
)


# ============================================================
# 32. MÉTRICAS POR CLASE DEL TEST CON UMBRAL AJUSTADO
# ============================================================

metricas_clase_umbral_rf <- calcular_metricas_multiclase(
  truth = resultados_test_umbral_rf$Gravedad_Accidente,
  estimate = resultados_test_umbral_rf$.pred_class_umbral,
  levels_clase = sev_orden
)


# ============================================================
# 33. MÉTRICAS MACRO DEL TEST CON UMBRAL AJUSTADO
# ============================================================

metricas_macro_umbral_rf <- metricas_clase_umbral_rf %>%
  summarise(
    Precision_macro = mean(Precision),
    Recall_macro = mean(Recall),
    F1_macro = mean(F1)
  )


# ============================================================
# 34. MÉTRICAS GLOBALES DEL TEST CON UMBRAL AJUSTADO
# ============================================================

metricas_globales_umbral_rf <- resultados_test_umbral_rf %>%
  summarise(
    Accuracy = accuracy_vec(
      truth = Gravedad_Accidente,
      estimate = .pred_class_umbral
    ),
    
    Kappa = kap_vec(
      truth = Gravedad_Accidente,
      estimate = .pred_class_umbral
    )
  )


# ============================================================
# 35. F2 FINAL EN TEST PARA "CON MUERTOS"
# ============================================================

metricas_muertos_umbral_rf <- metricas_clase_umbral_rf %>%
  filter(
    Clase == "Con muertos"
  )

precision_muertos_test_rf <-
  metricas_muertos_umbral_rf$Precision

recall_muertos_test_rf <-
  metricas_muertos_umbral_rf$Recall

F2_muertos_test_rf <- ifelse(
  precision_muertos_test_rf +
    recall_muertos_test_rf > 0,
  5 *
    precision_muertos_test_rf *
    recall_muertos_test_rf /
    (
      4 * precision_muertos_test_rf +
        recall_muertos_test_rf
    ),
  0
)


# ============================================================
# 36. CONSOLIDACIÓN DE MÉTRICAS FINALES
# ============================================================

metricas_finales_rf <- tibble(
  
  Modelo = paste0(
    "Random Forest - Umbral ",
    sprintf("%.2f", umbral_final_rf)
  ),
  
  mtry = mejor_combo_rf$mtry,
  
  min_n = mejor_combo_rf$min_n,
  
  Arboles = 500,
  
  Umbral = umbral_final_rf,
  
  Accuracy = metricas_globales_umbral_rf$Accuracy,
  
  Kappa = metricas_globales_umbral_rf$Kappa,
  
  Precision_macro =
    metricas_macro_umbral_rf$Precision_macro,
  
  Recall_macro =
    metricas_macro_umbral_rf$Recall_macro,
  
  F1_macro =
    metricas_macro_umbral_rf$F1_macro,
  
  ROC_AUC =
    roc_auc_base_rf$.estimate,
  
  Precision_Solo_daños =
    metricas_clase_umbral_rf %>%
    filter(Clase == "Solo daños") %>%
    pull(Precision),
  
  Recall_Solo_daños =
    metricas_clase_umbral_rf %>%
    filter(Clase == "Solo daños") %>%
    pull(Recall),
  
  F1_Solo_daños =
    metricas_clase_umbral_rf %>%
    filter(Clase == "Solo daños") %>%
    pull(F1),
  
  Precision_Con_heridos =
    metricas_clase_umbral_rf %>%
    filter(Clase == "Con heridos") %>%
    pull(Precision),
  
  Recall_Con_heridos =
    metricas_clase_umbral_rf %>%
    filter(Clase == "Con heridos") %>%
    pull(Recall),
  
  F1_Con_heridos =
    metricas_clase_umbral_rf %>%
    filter(Clase == "Con heridos") %>%
    pull(F1),
  
  Precision_Con_muertos =
    metricas_clase_umbral_rf %>%
    filter(Clase == "Con muertos") %>%
    pull(Precision),
  
  Recall_Con_muertos =
    metricas_clase_umbral_rf %>%
    filter(Clase == "Con muertos") %>%
    pull(Recall),
  
  F1_Con_muertos =
    metricas_clase_umbral_rf %>%
    filter(Clase == "Con muertos") %>%
    pull(F1),
  
  F2_Con_muertos =
    F2_muertos_test_rf
)


# ============================================================
# 37. RESULTADOS FINALES
# ============================================================

print(mejor_combo_rf)

print(mejor_umbral_rf)

print(matriz_confusion_umbral_rf)

print(metricas_finales_rf)