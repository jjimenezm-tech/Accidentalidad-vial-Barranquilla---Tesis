# =============================================================================
# XGBOOST — SEVERIDAD DE ACCIDENTES DE TRÁNSITO, BARRANQUILLA 2018–2025
# Semilla general: 2026
# =============================================================================

# 0. PAQUETES ---------------------------------------------------------------
paquetes <- c("readxl","dplyr","tidyr","purrr","lubridate","rsample",
              "recipes","parsnip","workflows","yardstick","tune",
              "hardhat","tibble","xgboost")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly=TRUE)]
if(length(faltantes)>0) install.packages(faltantes)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(purrr)
  library(lubridate); library(rsample); library(recipes); library(parsnip)
  library(workflows); library(yardstick); library(tune); library(hardhat)
  library(tibble); library(xgboost)
})
set.seed(2026)
options(dplyr.summarise.inform=FALSE)

# 1. CARGA -------------------------------------------------------------------
if(exists("BASE_DATOS_ACCIDENTES_TRANSITO_BARRANQUILLA_2018_2025")) {
  df <- BASE_DATOS_ACCIDENTES_TRANSITO_BARRANQUILLA_2018_2025
} else {
  ruta_base <- file.choose()
  df <- readxl::read_excel(ruta_base)
}

# 2. PREPARACIÓN -------------------------------------------------------------
columnas_necesarias <- c(
  "Hora","Día_Semana","Gravedad_Accidente","Clase_Accidente","Año","Mes",
  "Tipo_via","Veh_Motocicleta","Veh_Automovil","Veh_TransportePublico",
  "Veh_Carga","Veh_NoMotorizado","Veh_Otros","Número_total_vehiculos"
)
faltantes <- setdiff(columnas_necesarias,names(df))
if(length(faltantes)>0) stop("Faltan columnas: ",paste(faltantes,collapse=", "))
if(!inherits(df$Hora,"POSIXct")) stop("La columna 'Hora' no está almacenada como POSIXct.")

df <- df %>% mutate(
  Hora_hh=hour(Hora),
  Hora_mm=minute(Hora),
  Hora_Num=Hora_hh+(Hora_mm/60),
  Franja_Horaria=case_when(
    Hora_hh>=0 & Hora_hh<6 ~ "Madrugada",
    Hora_hh>=6 & Hora_hh<12 ~ "Mañana",
    Hora_hh>=12 & Hora_hh<18 ~ "Tarde",
    Hora_hh>=18 & Hora_hh<=23 ~ "Noche",
    TRUE ~ NA_character_
  )
)
if(any(is.na(df$Hora_Num))) stop("Se encontraron NA en Hora_Num.")
if(any(df$Hora_Num<0 | df$Hora_Num>=24)) stop("Hora fuera del rango 0–23.")
if(any(is.na(df$Franja_Horaria))) stop("Se encontraron NA en Franja_Horaria.")

predictoras <- c("Año","Mes","Día_Semana","Franja_Horaria","Clase_Accidente",
                 "Tipo_via","Veh_Motocicleta","Veh_Automovil",
                 "Veh_TransportePublico","Veh_Carga","Veh_NoMotorizado",
                 "Veh_Otros","Número_total_vehiculos")
sev_orden <- c("Solo daños","Con heridos","Con muertos")

df_modelo <- df %>% select(all_of(c(predictoras,"Gravedad_Accidente"))) %>%
  filter(!is.na(Franja_Horaria),!is.na(Gravedad_Accidente)) %>%
  mutate(
    Año=as.numeric(Año), Mes=factor(Mes), Día_Semana=factor(Día_Semana),
    Franja_Horaria=factor(Franja_Horaria,levels=c("Madrugada","Mañana","Tarde","Noche")),
    Clase_Accidente=factor(Clase_Accidente), Tipo_via=factor(Tipo_via),
    Gravedad_Accidente=factor(Gravedad_Accidente,levels=sev_orden)
  )
if(nrow(df_modelo)!=28004) warning("La base de modelado tiene ",nrow(df_modelo)," filas; se esperaban 28004.")
if(ncol(df_modelo)!=14) stop("Se esperaban 14 columnas en df_modelo.")
if(any(is.na(df_modelo))) stop("df_modelo contiene valores NA.")

# 3. TRAIN / TEST ------------------------------------------------------------
set.seed(2026)
split <- initial_split(df_modelo,prop=0.80,strata=Gravedad_Accidente)
train_data <- training(split)
test_data <- testing(split)

# 4. PESOS -------------------------------------------------------------------
conteos_clase <- table(train_data$Gravedad_Accidente)
pesos_clase <- nrow(train_data)/(length(conteos_clase)*conteos_clase)

# 5. FOLDS -------------------------------------------------------------------
set.seed(2026)
folds_xgb <- vfold_cv(train_data,v=5,strata=Gravedad_Accidente)

# 6. RECETA Y PESOS DE TRAIN -------------------------------------------------
pesos_clase_vector <- as.numeric(pesos_clase)
names(pesos_clase_vector) <- names(pesos_clase)
peso_clase <- pesos_clase_vector[as.character(train_data$Gravedad_Accidente)]
peso_clase <- hardhat::importance_weights(as.numeric(peso_clase))
train_data_xgb <- train_data %>% mutate(peso_clase=peso_clase)

receta_xgb <- recipe(Gravedad_Accidente~.,data=train_data_xgb) %>%
  step_dummy(all_nominal_predictors(),one_hot=TRUE)

xgb_spec <- boost_tree(trees=tune(),tree_depth=tune(),learn_rate=tune(),
                       min_n=tune(),loss_reduction=tune(),sample_size=tune()) %>%
  set_engine("xgboost",objective="multi:softprob",eval_metric="mlogloss",
             seed=2026,nthread=1) %>% set_mode("classification")

# 7. FUNCIONES DE TUNING -----------------------------------------------------
calcular_pesos_fold <- function(datos){
  conteos <- table(datos$Gravedad_Accidente)
  pesos <- nrow(datos)/(length(conteos)*conteos)
  pesos_vector <- as.numeric(pesos); names(pesos_vector) <- names(pesos)
  pesos_vector
}

# Métricas macro manuales para mantener las tres clases en el promedio.
# Si una clase no tiene predicciones positivas, su precisión se define como 0.
calcular_metricas_macro_manual <- function(truth, estimate,
                                           levels_clase = sev_orden) {
  verdad <- factor(truth, levels = levels_clase)
  prediccion <- factor(estimate, levels = levels_clase)
  matriz <- table(Prediccion = prediccion, Real = verdad)
  TP <- diag(matriz)
  FP <- rowSums(matriz) - TP
  FN <- colSums(matriz) - TP
  
  precision_clase <- ifelse(TP + FP > 0, TP / (TP + FP), 0)
  recall_clase <- ifelse(TP + FN > 0, TP / (TP + FN), 0)
  
  f1_clase <- ifelse(
    precision_clase + recall_clase > 0,
    2 * precision_clase * recall_clase / (precision_clase + recall_clase),
    0
  )
  
  tibble(
    precision_macro = mean(precision_clase),
    recall_macro = mean(recall_clase),
    f1_macro = mean(f1_clase)
  )
}

evaluar_configuracion_fold <- function(parametros, split_fold) {
  
  datos_analysis <- analysis(split_fold)
  datos_assessment <- assessment(split_fold)
  
  # Pesos de clase calculados únicamente con el pliegue de entrenamiento
  pesos_fold <- calcular_pesos_fold(datos_analysis)
  
  pesos_obs <- pesos_fold[
    as.character(datos_analysis$Gravedad_Accidente)
  ]
  
  pesos_obs <- hardhat::importance_weights(
    as.numeric(pesos_obs)
  )
  
  datos_analysis_xgb <- datos_analysis %>%
    mutate(peso_clase = pesos_obs)
  
  receta_fold <- recipe(
    Gravedad_Accidente ~ .,
    data = datos_analysis_xgb
  ) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
  
  spec_fold <- boost_tree(
    trees = parametros$trees,
    tree_depth = parametros$tree_depth,
    learn_rate = parametros$learn_rate,
    min_n = parametros$min_n,
    loss_reduction = parametros$loss_reduction,
    sample_size = parametros$sample_size
  ) %>%
    set_engine(
      "xgboost",
      objective = "multi:softprob",
      eval_metric = "mlogloss",
      seed = 2026,
      nthread = 1
    ) %>%
    set_mode("classification")
  
  workflow_fold <- workflow() %>%
    add_recipe(receta_fold) %>%
    add_model(spec_fold)
  
  modelo_fold <- fit(
    workflow_fold,
    data = datos_analysis_xgb
  )
  
  pred_clase <- predict(
    modelo_fold,
    new_data = datos_assessment,
    type = "class"
  )
  
  pred_prob <- predict(
    modelo_fold,
    new_data = datos_assessment,
    type = "prob"
  )
  
  resultados <- bind_cols(
    datos_assessment %>% select(Gravedad_Accidente),
    pred_clase,
    pred_prob
  )
  
  # Métricas macro calculadas con el mismo criterio para las tres clases
  metricas_macro <- calcular_metricas_macro_manual(
    truth = resultados$Gravedad_Accidente,
    estimate = resultados$.pred_class
  )
  
  tibble(
    accuracy = accuracy(
      resultados,
      truth = Gravedad_Accidente,
      estimate = .pred_class
    )$.estimate,
    
    precision_macro = metricas_macro$precision_macro,
    recall_macro = metricas_macro$recall_macro,
    f1_macro = metricas_macro$f1_macro,
    
    kappa = kap(
      resultados,
      truth = Gravedad_Accidente,
      estimate = .pred_class
    )$.estimate
  )
}

# 8. GRID --------------------------------------------------------------------
set.seed(2026)
grid_xgb <- tibble(
  trees=sample(seq(200,600,by=25),20,replace=TRUE),
  tree_depth=sample(3:10,20,replace=TRUE),
  learn_rate=round(runif(20,0.02,0.10),3),
  min_n=sample(seq(5,30,by=5),20,replace=TRUE),
  loss_reduction=round(runif(20,0,1),3),
  sample_size=round(runif(20,0.70,1.00),3)
)

# 9. TUNING 20 x 5 -----------------------------------------------------------
set.seed(2026)
resultados_xgb <- vector("list",nrow(grid_xgb))
for(i in seq_len(nrow(grid_xgb))){
  resultados_folds <- vector("list",nrow(folds_xgb))
  for(j in seq_len(nrow(folds_xgb))){
    resultados_folds[[j]] <- evaluar_configuracion_fold(grid_xgb[i,],folds_xgb$splits[[j]])
  }
  resultados_xgb[[i]] <- bind_rows(resultados_folds) %>% mutate(
    configuracion=i,trees=grid_xgb$trees[i],tree_depth=grid_xgb$tree_depth[i],
    learn_rate=grid_xgb$learn_rate[i],min_n=grid_xgb$min_n[i],
    loss_reduction=grid_xgb$loss_reduction[i],sample_size=grid_xgb$sample_size[i],
    fold=seq_len(nrow(folds_xgb))
  )
}
resultados_xgb <- bind_rows(resultados_xgb)

resumen_tuning_xgb <- resultados_xgb %>% group_by(configuracion,trees,tree_depth,
                                                  learn_rate,min_n,loss_reduction,sample_size) %>% summarise(
                                                    accuracy_promedio=mean(accuracy,na.rm=TRUE),
                                                    precision_macro_promedio=mean(precision_macro,na.rm=TRUE),
                                                    recall_macro_promedio=mean(recall_macro,na.rm=TRUE),
                                                    f1_macro_promedio=mean(f1_macro,na.rm=TRUE),
                                                    kappa_promedio=mean(kappa,na.rm=TRUE),.groups="drop") %>%
  arrange(desc(f1_macro_promedio))
mejor_config_xgb <- slice_head(resumen_tuning_xgb,n=1)

# 10. MODELO FINAL -----------------------------------------------------------
xgb_spec_final <- boost_tree(
  trees=mejor_config_xgb$trees,tree_depth=mejor_config_xgb$tree_depth,
  learn_rate=mejor_config_xgb$learn_rate,min_n=mejor_config_xgb$min_n,
  loss_reduction=mejor_config_xgb$loss_reduction,sample_size=mejor_config_xgb$sample_size
) %>% set_engine("xgboost",objective="multi:softprob",eval_metric="mlogloss",
                 seed=2026,nthread=1) %>% set_mode("classification")
workflow_xgb_final <- workflow() %>% add_recipe(receta_xgb) %>% add_model(xgb_spec_final)
set.seed(2026)
modelo_xgb_final <- fit(workflow_xgb_final,data=train_data_xgb)

# 11. PREDICCIONES TEST ------------------------------------------------------
pred_clase_xgb <- predict(modelo_xgb_final,new_data=test_data,type="class")
pred_prob_xgb <- predict(modelo_xgb_final,new_data=test_data,type="prob")
resultados_test_xgb <- test_data %>% select(Gravedad_Accidente) %>% bind_cols(pred_clase_xgb,pred_prob_xgb)

# 12. EVALUACIÓN BASE --------------------------------------------------------
matriz_confusion_base_xgb <- conf_mat(resultados_test_xgb,truth=Gravedad_Accidente,estimate=.pred_class)
accuracy_base_xgb <- accuracy(resultados_test_xgb,truth=Gravedad_Accidente,estimate=.pred_class)
kappa_base_xgb <- kap(resultados_test_xgb,truth=Gravedad_Accidente,estimate=.pred_class)

metricas_macro_base_manual_xgb <- calcular_metricas_macro_manual(
  truth = resultados_test_xgb$Gravedad_Accidente,
  estimate = resultados_test_xgb$.pred_class
)

roc_auc_base_xgb <- roc_auc(resultados_test_xgb,truth=Gravedad_Accidente,
                            `.pred_Solo daños`,`.pred_Con heridos`,`.pred_Con muertos`,estimator="hand_till")

metricas_globales_base_xgb <- tibble(
  Accuracy=accuracy_base_xgb$.estimate,Kappa=kappa_base_xgb$.estimate,
  Precision_macro=metricas_macro_base_manual_xgb$precision_macro,
  Recall_macro=metricas_macro_base_manual_xgb$recall_macro,
  F1_macro=metricas_macro_base_manual_xgb$f1_macro,
  ROC_AUC=roc_auc_base_xgb$.estimate)

calcular_metricas_clase_xgb <- function(datos,clase){
  datos_binarios <- datos %>% mutate(
    verdad_binaria=factor(ifelse(Gravedad_Accidente==clase,clase,paste0("No ",clase)),
                          levels=c(clase,paste0("No ",clase))),
    prediccion_binaria=factor(ifelse(.pred_class==clase,clase,paste0("No ",clase)),
                              levels=c(clase,paste0("No ",clase))))
  tibble(Clase=clase,
         Precision=precision(datos_binarios,truth=verdad_binaria,estimate=prediccion_binaria,event_level="first")$.estimate,
         Recall=recall(datos_binarios,truth=verdad_binaria,estimate=prediccion_binaria,event_level="first")$.estimate,
         F1=f_meas(datos_binarios,truth=verdad_binaria,estimate=prediccion_binaria,event_level="first")$.estimate)
}
metricas_clase_base_xgb <- bind_rows(
  calcular_metricas_clase_xgb(resultados_test_xgb,"Solo daños"),
  calcular_metricas_clase_xgb(resultados_test_xgb,"Con heridos"),
  calcular_metricas_clase_xgb(resultados_test_xgb,"Con muertos"))

# 13. PREDICCIONES OOF PARA SELECCIÓN DEL UMBRAL -------------------------------
#
# Las predicciones OOF se generan exclusivamente dentro del conjunto
# de entrenamiento. El conjunto de prueba no participa en la selección
# del umbral.
#
# Se utilizan los hiperparámetros seleccionados mediante validación
# cruzada en el bloque de tuning anterior.

aplicar_umbral_xgb <- function(datos, umbral, niveles = sev_orden) {
  
  pred_nueva <- case_when(
    datos$`.pred_Con muertos` >= umbral ~ "Con muertos",
    datos$`.pred_Solo daños` >= datos$`.pred_Con heridos` ~ "Solo daños",
    TRUE ~ "Con heridos"
  )
  
  factor(pred_nueva, levels = niveles)
}

# -------------------------------------------------------------------------
# Generar predicciones OOF sobre el conjunto de entrenamiento
# -------------------------------------------------------------------------

set.seed(2026)

predicciones_oof_xgb <- vector("list", nrow(folds_xgb))

for (j in seq_len(nrow(folds_xgb))) {
  
  datos_analysis <- analysis(folds_xgb$splits[[j]])
  datos_assessment <- assessment(folds_xgb$splits[[j]])
  
  # Pesos calculados únicamente con el subconjunto de análisis
  pesos_fold <- calcular_pesos_fold(datos_analysis)
  
  pesos_obs <- pesos_fold[
    as.character(datos_analysis$Gravedad_Accidente)
  ]
  
  pesos_obs <- hardhat::importance_weights(
    as.numeric(pesos_obs)
  )
  
  datos_analysis_xgb <- datos_analysis %>%
    mutate(peso_clase = pesos_obs)
  
  
  # Receta ajustada únicamente con los datos de análisis del fold
  receta_fold <- recipe(
    Gravedad_Accidente ~ .,
    data = datos_analysis_xgb
  ) %>%
    step_dummy(
      all_nominal_predictors(),
      one_hot = TRUE
    )
  
  
  # Modelo con los hiperparámetros seleccionados
  spec_fold <- boost_tree(
    trees = mejor_config_xgb$trees,
    tree_depth = mejor_config_xgb$tree_depth,
    learn_rate = mejor_config_xgb$learn_rate,
    min_n = mejor_config_xgb$min_n,
    loss_reduction = mejor_config_xgb$loss_reduction,
    sample_size = mejor_config_xgb$sample_size
  ) %>%
    set_engine(
      "xgboost",
      objective = "multi:softprob",
      eval_metric = "mlogloss",
      seed = 2026,
      nthread = 1
    ) %>%
    set_mode("classification")
  
  
  workflow_fold <- workflow() %>%
    add_recipe(receta_fold) %>%
    add_model(spec_fold)
  
  
  # Ajuste utilizando únicamente el análisis del fold
  set.seed(2026 + j)
  
  modelo_fold <- fit(
    workflow_fold,
    data = datos_analysis_xgb
  )
  
  
  # Predicciones sobre el assessment del fold
  pred_clase_fold <- predict(
    modelo_fold,
    new_data = datos_assessment,
    type = "class"
  )
  
  pred_prob_fold <- predict(
    modelo_fold,
    new_data = datos_assessment,
    type = "prob"
  )
  
  
  predicciones_oof_xgb[[j]] <- datos_assessment %>%
    select(Gravedad_Accidente) %>%
    bind_cols(pred_clase_fold, pred_prob_fold) %>%
    mutate(fold = j)
}


predicciones_oof_xgb <- bind_rows(predicciones_oof_xgb)


# Verificación: cada observación del conjunto de entrenamiento
# debe tener exactamente una predicción OOF.

if (nrow(predicciones_oof_xgb) != nrow(train_data)) {
  stop(
    "El número de predicciones OOF no coincide con el conjunto de entrenamiento."
  )
}

if (anyDuplicated(
  predicciones_oof_xgb %>%
  mutate(id_oof = row_number()) %>%
  pull(id_oof)
) > 0) {
  warning("Se detectaron posibles duplicados en las predicciones OOF.")
}


# -------------------------------------------------------------------------
# 14. SELECCIÓN DEL UMBRAL SOBRE LAS PREDICCIONES OOF -----------------------
#
# Se mantienen exactamente 50 pruebas:
# 0.01, 0.02, ..., 0.50.
#
# El conjunto de prueba NO participa en este proceso.

umbrales_xgb <- seq(
  from = 0.01,
  to = 0.50,
  by = 0.01
)

resultados_umbral_xgb <- map_dfr(
  umbrales_xgb,
  function(umbral_actual) {
    
    pred_umbral <- aplicar_umbral_xgb(
      predicciones_oof_xgb,
      umbral_actual
    )
    
    verdad_muertos <- (
      predicciones_oof_xgb$Gravedad_Accidente == "Con muertos"
    )
    
    pred_muertos <- (
      pred_umbral == "Con muertos"
    )
    
    TP <- sum(verdad_muertos & pred_muertos)
    FP <- sum(!verdad_muertos & pred_muertos)
    FN <- sum(verdad_muertos & !pred_muertos)
    TN <- sum(!verdad_muertos & !pred_muertos)
    
    precision_val <- ifelse(
      TP + FP > 0,
      TP / (TP + FP),
      0
    )
    
    recall_val <- ifelse(
      TP + FN > 0,
      TP / (TP + FN),
      0
    )
    
    f1_val <- ifelse(
      precision_val + recall_val > 0,
      2 * precision_val * recall_val /
        (precision_val + recall_val),
      0
    )
    
    f2_val <- ifelse(
      4 * precision_val + recall_val > 0,
      5 * precision_val * recall_val /
        (4 * precision_val + recall_val),
      0
    )
    
    tibble(
      umbral = umbral_actual,
      TP = TP,
      FP = FP,
      FN = FN,
      TN = TN,
      Precision = precision_val,
      Recall = recall_val,
      F1 = f1_val,
      F2 = f2_val
    )
  }
)


# Selección:
# 1. mayor F2
# 2. mayor Recall
# 3. menor umbral

mejor_umbral_xgb <- resultados_umbral_xgb %>%
  arrange(
    desc(F2),
    desc(Recall),
    umbral
  ) %>%
  slice_head(n = 1)

umbral_final_xgb <- mejor_umbral_xgb$umbral


# -------------------------------------------------------------------------
# 15. PREDICCIONES DEL CONJUNTO DE PRUEBA -------------------------------
#
# El test se utiliza únicamente después de haber seleccionado
# el umbral con las predicciones OOF.

pred_clase_xgb <- predict(
  modelo_xgb_final,
  new_data = test_data,
  type = "class"
)

pred_prob_xgb <- predict(
  modelo_xgb_final,
  new_data = test_data,
  type = "prob"
)

resultados_test_xgb <- test_data %>%
  select(Gravedad_Accidente) %>%
  bind_cols(
    pred_clase_xgb,
    pred_prob_xgb
  )


# -------------------------------------------------------------------------
# 16. EVALUACIÓN BASE SOBRE TEST ------------------------------------------
# -------------------------------------------------------------------------

matriz_confusion_base_xgb <- conf_mat(
  resultados_test_xgb,
  truth = Gravedad_Accidente,
  estimate = .pred_class
)

accuracy_base_xgb <- accuracy(
  resultados_test_xgb,
  truth = Gravedad_Accidente,
  estimate = .pred_class
)

kappa_base_xgb <- kap(
  resultados_test_xgb,
  truth = Gravedad_Accidente,
  estimate = .pred_class
)

metricas_macro_base_manual_xgb <- calcular_metricas_macro_manual(
  truth = resultados_test_xgb$Gravedad_Accidente,
  estimate = resultados_test_xgb$.pred_class
)

roc_auc_base_xgb <- roc_auc(
  resultados_test_xgb,
  truth = Gravedad_Accidente,
  `.pred_Solo daños`,
  `.pred_Con heridos`,
  `.pred_Con muertos`,
  estimator = "hand_till"
)

metricas_globales_base_xgb <- tibble(
  Accuracy = accuracy_base_xgb$.estimate,
  Kappa = kappa_base_xgb$.estimate,
  Precision_macro = metricas_macro_base_manual_xgb$precision_macro,
  Recall_macro = metricas_macro_base_manual_xgb$recall_macro,
  F1_macro = metricas_macro_base_manual_xgb$f1_macro,
  ROC_AUC = roc_auc_base_xgb$.estimate
)


# -------------------------------------------------------------------------
# 17. MÉTRICAS POR CLASE — MODELO BASE -------------------------------
# -------------------------------------------------------------------------

calcular_metricas_clase_xgb <- function(datos, clase) {
  
  datos_binarios <- datos %>%
    mutate(
      verdad_binaria = factor(
        ifelse(
          Gravedad_Accidente == clase,
          clase,
          paste0("No ", clase)
        ),
        levels = c(clase, paste0("No ", clase))
      ),
      prediccion_binaria = factor(
        ifelse(
          .pred_class == clase,
          clase,
          paste0("No ", clase)
        ),
        levels = c(clase, paste0("No ", clase))
      )
    )
  
  tibble(
    Clase = clase,
    Precision = precision(
      datos_binarios,
      truth = verdad_binaria,
      estimate = prediccion_binaria,
      event_level = "first"
    )$.estimate,
    Recall = recall(
      datos_binarios,
      truth = verdad_binaria,
      estimate = prediccion_binaria,
      event_level = "first"
    )$.estimate,
    F1 = f_meas(
      datos_binarios,
      truth = verdad_binaria,
      estimate = prediccion_binaria,
      event_level = "first"
    )$.estimate
  )
}

metricas_clase_base_xgb <- bind_rows(
  calcular_metricas_clase_xgb(
    resultados_test_xgb,
    "Solo daños"
  ),
  calcular_metricas_clase_xgb(
    resultados_test_xgb,
    "Con heridos"
  ),
  calcular_metricas_clase_xgb(
    resultados_test_xgb,
    "Con muertos"
  )
)


# -------------------------------------------------------------------------
# 18. APLICACIÓN DEL UMBRAL SELECCIONADO SOBRE TEST -----------------------
# -------------------------------------------------------------------------

pred_clase_umbral_xgb <- aplicar_umbral_xgb(
  resultados_test_xgb,
  umbral_final_xgb
)

resultados_test_umbral_xgb <- resultados_test_xgb %>%
  select(
    Gravedad_Accidente,
    `.pred_Solo daños`,
    `.pred_Con heridos`,
    `.pred_Con muertos`
  ) %>%
  mutate(
    .pred_class = pred_clase_umbral_xgb
  )


# -------------------------------------------------------------------------
# 19. EVALUACIÓN AJUSTADA SOBRE TEST --------------------------------------
# -------------------------------------------------------------------------

matriz_confusion_umbral_xgb <- conf_mat(
  resultados_test_umbral_xgb,
  truth = Gravedad_Accidente,
  estimate = .pred_class
)

accuracy_umbral_xgb <- accuracy(
  resultados_test_umbral_xgb,
  truth = Gravedad_Accidente,
  estimate = .pred_class
)

kappa_umbral_xgb <- kap(
  resultados_test_umbral_xgb,
  truth = Gravedad_Accidente,
  estimate = .pred_class
)

metricas_macro_umbral_manual_xgb <- calcular_metricas_macro_manual(
  truth = resultados_test_umbral_xgb$Gravedad_Accidente,
  estimate = resultados_test_umbral_xgb$.pred_class
)

metricas_globales_umbral_xgb <- tibble(
  Accuracy = accuracy_umbral_xgb$.estimate,
  Kappa = kappa_umbral_xgb$.estimate,
  Precision_macro = metricas_macro_umbral_manual_xgb$precision_macro,
  Recall_macro = metricas_macro_umbral_manual_xgb$recall_macro,
  F1_macro = metricas_macro_umbral_manual_xgb$f1_macro,
  ROC_AUC = roc_auc_base_xgb$.estimate
)


# -------------------------------------------------------------------------
# 20. MÉTRICAS POR CLASE — MODELO AJUSTADO -------------------------------
# -------------------------------------------------------------------------

calcular_metricas_clase_umbral_xgb <- function(datos, clase) {
  
  datos_binarios <- datos %>%
    mutate(
      verdad_binaria = factor(
        ifelse(
          Gravedad_Accidente == clase,
          clase,
          paste0("No ", clase)
        ),
        levels = c(clase, paste0("No ", clase))
      ),
      prediccion_binaria = factor(
        ifelse(
          .pred_class == clase,
          clase,
          paste0("No ", clase)
        ),
        levels = c(clase, paste0("No ", clase))
      )
    )
  
  tibble(
    Clase = clase,
    Precision = precision(
      datos_binarios,
      truth = verdad_binaria,
      estimate = prediccion_binaria,
      event_level = "first"
    )$.estimate,
    Recall = recall(
      datos_binarios,
      truth = verdad_binaria,
      estimate = prediccion_binaria,
      event_level = "first"
    )$.estimate,
    F1 = f_meas(
      datos_binarios,
      truth = verdad_binaria,
      estimate = prediccion_binaria,
      event_level = "first"
    )$.estimate
  )
}

metricas_clase_umbral_xgb <- bind_rows(
  calcular_metricas_clase_umbral_xgb(
    resultados_test_umbral_xgb,
    "Solo daños"
  ),
  calcular_metricas_clase_umbral_xgb(
    resultados_test_umbral_xgb,
    "Con heridos"
  ),
  calcular_metricas_clase_umbral_xgb(
    resultados_test_umbral_xgb,
    "Con muertos"
  )
)

metricas_muertos_umbral_xgb <- resultados_umbral_xgb %>%
  filter(umbral == umbral_final_xgb)


# -------------------------------------------------------------------------
# 21. RESULTADO CONSOLIDADO -----------------------------------------------
# -------------------------------------------------------------------------

metricas_finales_xgb <- tibble(
  Modelo = "XGBoost",
  trees = mejor_config_xgb$trees,
  tree_depth = mejor_config_xgb$tree_depth,
  learn_rate = mejor_config_xgb$learn_rate,
  min_n = mejor_config_xgb$min_n,
  loss_reduction = mejor_config_xgb$loss_reduction,
  sample_size = mejor_config_xgb$sample_size,
  Umbral = umbral_final_xgb,
  Accuracy = metricas_globales_umbral_xgb$Accuracy,
  Kappa = metricas_globales_umbral_xgb$Kappa,
  Precision_macro = metricas_globales_umbral_xgb$Precision_macro,
  Recall_macro = metricas_globales_umbral_xgb$Recall_macro,
  F1_macro = metricas_globales_umbral_xgb$F1_macro,
  ROC_AUC = metricas_globales_umbral_xgb$ROC_AUC
)


# -------------------------------------------------------------------------
# 22. VERIFICACIONES -------------------------------------------------------
# -------------------------------------------------------------------------

stopifnot(
  nrow(df_modelo) == 28004,
  nrow(train_data) == 22402,
  nrow(test_data) == 5602,
  nrow(folds_xgb) == 5,
  nrow(grid_xgb) == 20,
  nrow(resultados_xgb) == 100,
  nrow(resumen_tuning_xgb) == 20,
  nrow(predicciones_oof_xgb) == 22402,
  nrow(resultados_test_xgb) == 5602,
  nrow(resultados_umbral_xgb) == 50,
  length(umbrales_xgb) == 50,
  min(umbrales_xgb) == 0.01,
  max(umbrales_xgb) == 0.50,
  nrow(metricas_clase_base_xgb) == 3,
  nrow(metricas_clase_umbral_xgb) == 3,
  !is.na(umbral_final_xgb)
)


# -------------------------------------------------------------------------
# 23. RESULTADOS EN CONSOLA -----------------------------------------------
# -------------------------------------------------------------------------

cat("\n================ XGBOOST BASE ================\n")
print(mejor_config_xgb)
print(metricas_globales_base_xgb)
print(metricas_clase_base_xgb)
print(roc_auc_base_xgb)

cat("\n========== PREDICCIONES OOF =================\n")
cat("Observaciones OOF:", nrow(predicciones_oof_xgb), "\n")
cat("Umbrales evaluados:", length(umbrales_xgb), "\n")
cat("Rango de umbrales: 0.01 - 0.50\n")

cat("\n=========== UMBRAL SELECCIONADO =============\n")
print(mejor_umbral_xgb)

cat("\n======== XGBOOST UMBRAL AJUSTADO ============\n")
print(matriz_confusion_umbral_xgb)
print(metricas_globales_umbral_xgb)
print(metricas_clase_umbral_xgb)
print(metricas_muertos_umbral_xgb)

cat("\nScript XGBoost finalizado correctamente.\n")