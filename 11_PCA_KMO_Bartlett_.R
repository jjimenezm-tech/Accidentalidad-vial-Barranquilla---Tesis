# ============================================================
# ANÁLISIS DE ADECUACIÓN PARA PCA
# MATRIZ DE CORRELACIONES + KMO + BARTLETT + PCA
# VARIABLES CANDIDATAS AL MODELADO
# ============================================================


# ============================================================
# 1. PAQUETES
# ============================================================

library(readxl)
library(dplyr)
library(psych)


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
  "Observaciones:",
  nrow(df),
  "\n"
)


# ============================================================
# 3. VARIABLES CUANTITATIVAS CANDIDATAS AL MODELADO
# ============================================================
#
# Se excluyen:
#
# - Número_total_vehiculos:
#   dependencia lineal exacta con los tipos de vehículos.
#
# - Num_Heridos
# - Num_Fallecidos
# - Edad_Promedio_De_Las_Victimas
#   variables relacionadas con víctimas y no candidatas
#   al modelo.
#
# ============================================================

variables_pca_completas <- c(
  "Año",
  "Veh_Motocicleta",
  "Veh_Automovil",
  "Veh_TransportePublico",
  "Veh_Carga",
  "Veh_NoMotorizado",
  "Veh_Otros"
)


# ============================================================
# 4. FUNCIÓN PARA REALIZAR EL ANÁLISIS DE ADECUACIÓN
# ============================================================

evaluar_pca <- function(df, variables, nombre_analisis) {
  
  
  cat("\n\n")
  cat("====================================================\n")
  cat(nombre_analisis, "\n")
  cat("====================================================\n")
  
  
  # ----------------------------------------------------------
  # 5.1 CREAR BASE
  # ----------------------------------------------------------
  
  datos <- df %>%
    select(all_of(variables)) %>%
    na.omit()
  
  
  cat("\nObservaciones utilizadas:",
      nrow(datos), "\n")
  
  cat("Variables:",
      ncol(datos), "\n")
  
  
  # ----------------------------------------------------------
  # 4.2 MATRIZ DE CORRELACIONES DE SPEARMAN
  # ----------------------------------------------------------
  
  matriz_cor <- cor(
    datos,
    method = "spearman",
    use = "complete.obs"
  )
  
  
  cat("\n")
  cat("--------------------------------------------\n")
  cat("MATRIZ DE CORRELACIONES DE SPEARMAN\n")
  cat("--------------------------------------------\n")
  
  print(
    round(
      matriz_cor,
      3
    )
  )
  
  
  # ----------------------------------------------------------
  # 4.3 KMO
  # ----------------------------------------------------------
  
  resultado_kmo <- KMO(
    matriz_cor
  )
  
  
  cat("\n")
  cat("--------------------------------------------\n")
  cat("ÍNDICE KMO\n")
  cat("--------------------------------------------\n")
  
  print(resultado_kmo)
  
  
  # ----------------------------------------------------------
  # 4.4 BARTLETT
  # ----------------------------------------------------------
  
  resultado_bartlett <- cortest.bartlett(
    matriz_cor,
    n = nrow(datos)
  )
  
  
  cat("\n")
  cat("--------------------------------------------\n")
  cat("PRUEBA DE ESFERICIDAD DE BARTLETT\n")
  cat("--------------------------------------------\n")
  
  print(resultado_bartlett)
  
  
  # ----------------------------------------------------------
  # 4.5 AUTOVALORES
  # ----------------------------------------------------------
  
  autovalores <- eigen(
    matriz_cor
  )$values
  
  
  tabla_autovalores <- data.frame(
    Componente = seq_along(autovalores),
    Autovalor = autovalores,
    Varianza_Porcentaje =
      100 * autovalores / sum(autovalores),
    Varianza_Acumulada =
      100 * cumsum(autovalores) / sum(autovalores)
  )
  
  
  cat("\n")
  cat("--------------------------------------------\n")
  cat("AUTOVALORES Y VARIANZA EXPLICADA\n")
  cat("--------------------------------------------\n")
  
  print(
    round(
      tabla_autovalores,
      3
    )
  )
  
  
  # ----------------------------------------------------------
  # 4.6 GRÁFICO DE SEDIMENTACIÓN
  # ----------------------------------------------------------
  
  plot(
    autovalores,
    type = "b",
    pch = 19,
    xlab = "Componente",
    ylab = "Autovalor",
    main = paste(
      "Gráfico de sedimentación -",
      nombre_analisis
    )
  )
  
  abline(
    h = 1,
    lty = 2
  )
  
  
  # ----------------------------------------------------------
  # 4.7 ANÁLISIS PARALELO
  # ----------------------------------------------------------
  #
  # Permite comparar los autovalores observados con los
  # esperados bajo datos aleatorios.
  #
  # ----------------------------------------------------------
  
  cat("\n")
  cat("--------------------------------------------\n")
  cat("ANÁLISIS PARALELO\n")
  cat("--------------------------------------------\n")
  
  resultado_paralelo <- fa.parallel(
    datos,
    fa = "pc",
    cor = "cor",
    n.iter = 100,
    main = paste(
      "Análisis paralelo -",
      nombre_analisis
    )
  )
  
  
  # ----------------------------------------------------------
  # 4.8 PCA
  # ----------------------------------------------------------
  #
  # Se ejecuta para explorar los componentes.
  # La decisión final de utilizar PCA debe basarse en la
  # adecuación observada mediante KMO, Bartlett y análisis
  # paralelo.
  #
  # ----------------------------------------------------------
  
  numero_componentes <- sum(
    autovalores > 1
  )
  
  
  cat("\n")
  cat("--------------------------------------------\n")
  cat("NÚMERO DE COMPONENTES SEGÚN KAISER\n")
  cat("--------------------------------------------\n")
  
  cat(
    "Componentes con autovalor > 1:",
    numero_componentes,
    "\n"
  )
  
  
  if (numero_componentes >= 1) {
    
    resultado_pca <- principal(
      datos,
      nfactors = numero_componentes,
      rotate = "varimax",
      scores = FALSE
    )
    
    
    cat("\n")
    cat("--------------------------------------------\n")
    cat("RESULTADOS DEL PCA\n")
    cat("--------------------------------------------\n")
    
    print(
      resultado_pca
    )
    
    
    cat("\n")
    cat("Cargas factoriales:\n")
    
    print(
      round(
        as.matrix(resultado_pca$loadings),
        3
      )
    )
    
    
  } else {
    
    resultado_pca <- NULL
    
    cat(
      "\nNo se identificaron componentes con autovalor > 1.\n"
    )
  }
  
  
  # ----------------------------------------------------------
  # 4.9 RESUMEN PARA TESIS
  # ----------------------------------------------------------
  
  cat("\n")
  cat("====================================================\n")
  cat("RESUMEN PARA TESIS\n")
  cat("====================================================\n")
  
  cat(
    "Análisis:",
    nombre_analisis,
    "\n"
  )
  
  cat(
    "Observaciones:",
    nrow(datos),
    "\n"
  )
  
  cat(
    "Variables:",
    ncol(datos),
    "\n"
  )
  
  cat(
    "KMO global:",
    round(
      resultado_kmo$MSA,
      3
    ),
    "\n"
  )
  
  cat(
    "Bartlett - Chi cuadrado:",
    round(
      resultado_bartlett$chisq,
      2
    ),
    "\n"
  )
  
  cat(
    "Bartlett - gl:",
    resultado_bartlett$df,
    "\n"
  )
  
  cat(
    "Bartlett - p:",
    format.pval(
      resultado_bartlett$p.value,
      digits = 4
    ),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # 4.10 DEVOLVER RESULTADOS
  # ----------------------------------------------------------
  
  return(
    list(
      datos = datos,
      matriz_cor = matriz_cor,
      kmo = resultado_kmo,
      bartlett = resultado_bartlett,
      autovalores = tabla_autovalores,
      paralelo = resultado_paralelo,
      pca = resultado_pca
    )
  )
}


# ============================================================
# 5. ANÁLISIS 1:
# TODAS LAS VARIABLES CUANTITATIVAS CANDIDATAS
# ============================================================

resultado_pca_completo <- evaluar_pca(
  df = df,
  variables = variables_pca_completas,
  nombre_analisis =
    "PCA - Variables cuantitativas candidatas"
)

# ============================================================
# 6. TABLA RESUMEN DE KMO Y BARTLETT
# ============================================================

tabla_adecuacion <- data.frame(
  Analisis = "Variables cuantitativas candidatas",
  
  Observaciones = nrow(resultado_pca_completo$datos),
  
  Variables = length(variables_pca_completas),
  
  KMO = resultado_pca_completo$kmo$MSA,
  
  Chi_cuadrado_Bartlett =
    resultado_pca_completo$bartlett$chisq,
  
  gl = resultado_pca_completo$bartlett$df,
  
  p_Bartlett = resultado_pca_completo$bartlett$p.value
)

tabla_adecuacion <- tabla_adecuacion %>%
  mutate(
    KMO = round(KMO, 3),
    Chi_cuadrado_Bartlett =
      round(Chi_cuadrado_Bartlett, 2),
    p_Bartlett = format.pval(
      p_Bartlett,
      digits = 4
    )
  )

cat("\n")
cat("============================================\n")
cat("TABLA FINAL DE ADECUACIÓN PARA PCA\n")
cat("============================================\n")

print(tabla_adecuacion)


# ============================================================
# 7. MATRIZ DE CORRELACIÓN PARA TESIS
# ============================================================

cat("\n")
cat("============================================\n")
cat("MATRIZ - VARIABLES CUANTITATIVAS CANDIDATAS\n")
cat("============================================\n")

print(
  round(
    resultado_pca_completo$matriz_cor,
    3
  )
)

# ============================================================
# FIN
# ============================================================
