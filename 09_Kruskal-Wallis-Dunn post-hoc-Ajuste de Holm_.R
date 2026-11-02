# ============================================================
# KRUSKAL-WALLIS
# DIFERENCIAS DE VARIABLES NUMÉRICAS SEGÚN GRAVEDAD
# AJUSTE DE HOLM
# ============================================================


# ============================================================
# 1. PAQUETES
# ============================================================

paquetes <- c(
  "dplyr",
  "tidyr",
  "rstatix",
  "ggplot2",
  "openxlsx"
)

instalar <- paquetes[
  !(paquetes %in% installed.packages()[, "Package"])
]

if (length(instalar) > 0) {
  install.packages(instalar)
}

library(dplyr)
library(tidyr)
library(rstatix)
library(ggplot2)
library(openxlsx)


# ============================================================
# 2. VERIFICAR BASE
# ============================================================

if (!exists("df")) {
  stop(
    "No existe el objeto 'df'. Carga primero tu base de datos."
  )
}

cat(
  "Número de registros:",
  nrow(df),
  "\n"
)


# ============================================================
# 3. CREAR VARIABLES DE ACTORES
# ============================================================
#
# Actor = muertos + heridos.
#
# Estas variables se mantienen como cantidades numéricas.
# ============================================================

df <- df %>%
  mutate(
    
    Actor_Acompañante =
      rowSums(
        cbind(
          coalesce(
            as.numeric(`#_Acompañante_Muerto`),
            0
          ),
          coalesce(
            as.numeric(`#_Acompañante_Herido`),
            0
          )
        )
      ),
    
    Actor_Ciclista =
      rowSums(
        cbind(
          coalesce(
            as.numeric(`#_Ciclista_Muerto`),
            0
          ),
          coalesce(
            as.numeric(`#_Ciclista_Herido`),
            0
          )
        )
      ),
    
    Actor_Conductor =
      rowSums(
        cbind(
          coalesce(
            as.numeric(`#_Conductor_Muerto`),
            0
          ),
          coalesce(
            as.numeric(`#_Conductor_Herido`),
            0
          )
        )
      ),
    
    Actor_Motociclista =
      rowSums(
        cbind(
          coalesce(
            as.numeric(`#_Motociclista_Muerto`),
            0
          ),
          coalesce(
            as.numeric(`#_Motociclista_Herido`),
            0
          )
        )
      ),
    
    Actor_Pasajero =
      rowSums(
        cbind(
          coalesce(
            as.numeric(`#_Pasajero_Muerto`),
            0
          ),
          coalesce(
            as.numeric(`#_Pasajero_Herido`),
            0
          )
        )
      ),
    
    Actor_Peaton =
      rowSums(
        cbind(
          coalesce(
            as.numeric(`#_Peaton_Muerto`),
            0
          ),
          coalesce(
            as.numeric(`#_Peaton_Herido`),
            0
          )
        )
      )
    
  )


# ============================================================
# 4. PREPARAR GRAVEDAD
# ============================================================

df <- df %>%
  mutate(
    
    Gravedad_Accidente = factor(
      Gravedad_Accidente,
      levels = c(
        "Solo daños",
        "Con heridos",
        "Con muertos"
      )
    )
    
  )


# ============================================================
# 5. SELECCIONAR VARIABLES NUMÉRICAS
# ============================================================
#
# VEHÍCULOS
# - Motocicleta
# - Automóvil
# - Transporte público
# - Carga
# - No motorizado
# - Otros
# - Total de vehículos
#
# ACTORES
# - Peatón
# - Ciclista
# - Motociclista
# - Conductor
# - Pasajero
# - Acompañante
#
# OTRAS
# - Número de heridos
# - Número de fallecidos
# - Edad promedio de las víctimas
# ============================================================

variables_kw <- c(
  
  "Veh_Motocicleta",
  "Veh_Automovil",
  "Veh_TransportePublico",
  "Veh_Carga",
  "Veh_NoMotorizado",
  "Veh_Otros",
  "Número_total_vehiculos",
  
  "Actor_Peaton",
  "Actor_Ciclista",
  "Actor_Motociclista",
  "Actor_Conductor",
  "Actor_Pasajero",
  "Actor_Acompañante",
  
  "Num_Heridos",
  "Num_Fallecidos",
  "Edad_Promedio_De_Las_Victimas"
  
)


# ============================================================
# 6. VERIFICAR QUE EXISTAN LAS VARIABLES
# ============================================================

faltantes <- setdiff(
  variables_kw,
  names(df)
)

if (length(faltantes) > 0) {
  
  cat("\nFALTAN LAS SIGUIENTES VARIABLES:\n")
  print(faltantes)
  
  stop(
    "\nCorrige los nombres de las variables antes de continuar."
  )
  
}


# ============================================================
# 7. CREAR BASE PARA KRUSKAL-WALLIS
# ============================================================

datos_kw <- df %>%
  
  select(
    Gravedad_Accidente,
    all_of(variables_kw)
  ) %>%
  
  pivot_longer(
    cols = all_of(variables_kw),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  
  mutate(
    Valor = as.numeric(Valor)
  )


# ============================================================
# 8. KRUSKAL-WALLIS
# ============================================================

resultados_kw <- datos_kw %>%
  
  group_by(Variable) %>%
  
  kruskal_test(
    Valor ~ Gravedad_Accidente
  )


# ============================================================
# 9. TAMAÑO DEL EFECTO
# ============================================================
#
# Épsilon cuadrado (ε²)
# ============================================================

resultados_efecto <- datos_kw %>%
  
  group_by(Variable) %>%
  
  kruskal_effsize(
    Valor ~ Gravedad_Accidente
  )


# ============================================================
# 10. UNIR RESULTADOS
# ============================================================

tabla_kw <- resultados_kw %>%
  
  select(
    Variable,
    statistic,
    df,
    p
  ) %>%
  
  left_join(
    
    resultados_efecto %>%
      select(
        Variable,
        effsize,
        magnitude
      ),
    
    by = "Variable"
  ) %>%
  
  rename(
    `H de Kruskal-Wallis` = statistic,
    `Grados de libertad` = df,
    `Épsilon²` = effsize,
    `Magnitud del efecto` = magnitude
  ) %>%
  
  mutate(
    
    `H de Kruskal-Wallis` =
      round(
        `H de Kruskal-Wallis`,
        3
      ),
    
    `Épsilon²` =
      round(
        `Épsilon²`,
        4
      ),
    
    p = ifelse(
      p < 0.001,
      "<0,001",
      sprintf(
        "%.3f",
        p
      )
    )
    
  )


# ============================================================
# 11. CAMBIAR NOMBRES PARA LA TABLA FINAL
# ============================================================

tabla_kw <- tabla_kw %>%
  
  mutate(
    
    Variable = case_when(
      
      Variable == "Veh_Motocicleta" ~
        "Motocicleta",
      
      Variable == "Veh_Automovil" ~
        "Automóvil",
      
      Variable == "Veh_TransportePublico" ~
        "Transporte público",
      
      Variable == "Veh_Carga" ~
        "Carga",
      
      Variable == "Veh_NoMotorizado" ~
        "No motorizado",
      
      Variable == "Veh_Otros" ~
        "Otros vehículos",
      
      Variable == "Número_total_vehiculos" ~
        "Total de vehículos",
      
      Variable == "Actor_Peaton" ~
        "Actor peatón",
      
      Variable == "Actor_Ciclista" ~
        "Actor ciclista",
      
      Variable == "Actor_Motociclista" ~
        "Actor motociclista",
      
      Variable == "Actor_Conductor" ~
        "Actor conductor",
      
      Variable == "Actor_Pasajero" ~
        "Actor pasajero",
      
      Variable == "Actor_Acompañante" ~
        "Actor acompañante",
      
      Variable == "Num_Heridos" ~
        "Número de heridos",
      
      Variable == "Num_Fallecidos" ~
        "Número de fallecidos",
      
      Variable == "Edad_Promedio_De_Las_Victimas" ~
        "Edad promedio de las víctimas",
      
      TRUE ~
        Variable
      
    )
    
  )


# ============================================================
# 12. ORDENAR POR TAMAÑO DEL EFECTO
# ============================================================

tabla_kw_ordenada <- tabla_kw %>%
  
  arrange(
    desc(`Épsilon²`)
  )


# ============================================================
# 13. MOSTRAR RESULTADOS DE KRUSKAL-WALLIS
# ============================================================

cat("\n")
cat("============================================================\n")
cat("RESULTADOS DE KRUSKAL-WALLIS\n")
cat("============================================================\n\n")

print(
  tabla_kw_ordenada,
  row.names = FALSE
)


# ============================================================
# 14. IDENTIFICAR VARIABLES SIGNIFICATIVAS
# ============================================================

variables_significativas <- resultados_kw %>%
  
  filter(
    p < 0.05
  ) %>%
  
  pull(
    Variable
  )


cat("\n")
cat("============================================================\n")
cat("VARIABLES CON DIFERENCIAS SIGNIFICATIVAS\n")
cat("============================================================\n\n")

print(
  variables_significativas
)


# ============================================================
# 15. DUNN POST-HOC CON AJUSTE DE HOLM
# ============================================================
#
# Se realiza únicamente para las variables que resultaron
# significativas en Kruskal-Wallis.
#
# Comparaciones:
#
# 1. Solo daños vs. Con heridos
# 2. Solo daños vs. Con muertos
# 3. Con heridos vs. Con muertos
#
# Se utiliza ajuste de Holm para controlar el error
# asociado a las comparaciones múltiples.
# ============================================================

if (
  length(variables_significativas) > 0
) {
  
  datos_dunn <- datos_kw %>%
    
    filter(
      Variable %in% variables_significativas
    )
  
  
  resultados_dunn <- datos_dunn %>%
    
    group_by(Variable) %>%
    
    dunn_test(
      Valor ~ Gravedad_Accidente,
      p.adjust.method = "holm"
    )
  
  
  # ==========================================================
  # 16. CAMBIAR NOMBRES DE VARIABLES EN DUNN
  # ==========================================================
  
  tabla_dunn <- resultados_dunn %>%
    
    mutate(
      
      Variable = case_when(
        
        Variable == "Veh_Motocicleta" ~
          "Motocicleta",
        
        Variable == "Veh_Automovil" ~
          "Automóvil",
        
        Variable == "Veh_TransportePublico" ~
          "Transporte público",
        
        Variable == "Veh_Carga" ~
          "Carga",
        
        Variable == "Veh_NoMotorizado" ~
          "No motorizado",
        
        Variable == "Veh_Otros" ~
          "Otros vehículos",
        
        Variable == "Número_total_vehiculos" ~
          "Total de vehículos",
        
        Variable == "Actor_Peaton" ~
          "Actor peatón",
        
        Variable == "Actor_Ciclista" ~
          "Actor ciclista",
        
        Variable == "Actor_Motociclista" ~
          "Actor motociclista",
        
        Variable == "Actor_Conductor" ~
          "Actor conductor",
        
        Variable == "Actor_Pasajero" ~
          "Actor pasajero",
        
        Variable == "Actor_Acompañante" ~
          "Actor acompañante",
        
        Variable == "Num_Heridos" ~
          "Número de heridos",
        
        Variable == "Num_Fallecidos" ~
          "Número de fallecidos",
        
        Variable == "Edad_Promedio_De_Las_Victimas" ~
          "Edad promedio de las víctimas",
        
        TRUE ~
          Variable
        
      ),
      
      p.adj = ifelse(
        p.adj < 0.001,
        "<0,001",
        sprintf(
          "%.3f",
          p.adj
        )
      )
      
    ) %>%
    
    select(
      Variable,
      group1,
      group2,
      statistic,
      p,
      p.adj
    ) %>%
    
    rename(
      `Grupo 1` = group1,
      `Grupo 2` = group2,
      `Z de Dunn` = statistic,
      `p sin ajustar` = p,
      `p ajustado (Holm)` = p.adj
    )
  
  
  # ==========================================================
  # 17. MOSTRAR DUNN
  # ==========================================================
  
  cat("\n")
  cat("============================================================\n")
  cat("PRUEBA POST-HOC DE DUNN - AJUSTE DE HOLM\n")
  cat("============================================================\n\n")
  
  print(
    tabla_dunn,
    row.names = FALSE
  )
  
  
} else {
  
  tabla_dunn <- data.frame()
  
  cat(
    "\nNo se encontraron variables significativas para Dunn.\n"
  )
  
}


# ============================================================
# 18. GUARDAR RESULTADOS EN EXCEL
# ============================================================

if (!requireNamespace(
  "openxlsx",
  quietly = TRUE
)) {
  
  install.packages("openxlsx")
  
}

library(openxlsx)


# Crear libro

wb <- createWorkbook()


# ============================================================
# HOJA 1: KRUSKAL-WALLIS
# ============================================================

addWorksheet(
  wb,
  "Kruskal-Wallis"
)

writeData(
  wb,
  "Kruskal-Wallis",
  tabla_kw_ordenada
)


# ============================================================
# HOJA 2: DUNN CON HOLM
# ============================================================

addWorksheet(
  wb,
  "Dunn post-hoc - Holm"
)

if (
  nrow(tabla_dunn) > 0
) {
  
  writeData(
    wb,
    "Dunn post-hoc - Holm",
    tabla_dunn
  )
  
} else {
  
  writeData(
    wb,
    "Dunn post-hoc - Holm",
    data.frame(
      Resultado =
        "No hubo variables significativas"
    )
  )
  
}


# ============================================================
# 19. GUARDAR ARCHIVO
# ============================================================

nombre_archivo <-
  "Resultados_Kruskal_Wallis_Actores_Vehiculos_AJUSTE DE HOLM.xlsx"


saveWorkbook(
  wb,
  nombre_archivo,
  overwrite = TRUE
)


# ============================================================
# 20. FINAL
# ============================================================

cat("\n")
cat("============================================================\n")
cat("ANÁLISIS COMPLETADO CORRECTAMENTE\n")
cat("============================================================\n\n")

cat(
  "Archivo generado:\n",
  nombre_archivo,
  "\n"
)
resultados_dunn <- datos_dunn %>%
  group_by(Variable) %>%
  dunn_test(
    Valor ~ Gravedad_Accidente,
    p.adjust.method = "holm"
  )
# ============================================================
# MAPA DE SIGNIFICANCIA DE DUNN - AJUSTE DE HOLM
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# ------------------------------------------------------------
# 1. Preparar resultados
# ------------------------------------------------------------

dunn_grafica <- resultados_dunn %>%
  
  mutate(
    
    Comparacion = case_when(
      
      group1 == "Solo daños" &
        group2 == "Con heridos" ~
        "Solo daños vs. Con heridos",
      
      group1 == "Solo daños" &
        group2 == "Con muertos" ~
        "Solo daños vs. Con muertos",
      
      group1 == "Con heridos" &
        group2 == "Con muertos" ~
        "Con heridos vs. Con muertos",
      
      TRUE ~
        paste(group1, "vs.", group2)
      
    ),
    
    Significativo = if_else(
      p.adj < 0.05,
      "Significativa",
      "No significativa"
    )
    
  )


# ------------------------------------------------------------
# 2. Comparaciones
# ------------------------------------------------------------

comparaciones <- c(
  "Solo daños vs. Con heridos",
  "Solo daños vs. Con muertos",
  "Con heridos vs. Con muertos"
)


# ------------------------------------------------------------
# 3. Crear matriz de significancia
# ------------------------------------------------------------

mapa_dunn <- dunn_grafica %>%
  
  select(
    Variable,
    Comparacion,
    Significativo
  ) %>%
  
  pivot_wider(
    names_from = Comparacion,
    values_from = Significativo
  )


# ------------------------------------------------------------
# 4. Pasar a formato largo
# ------------------------------------------------------------

mapa_dunn_largo <- mapa_dunn %>%
  
  pivot_longer(
    
    cols = all_of(comparaciones),
    
    names_to = "Comparacion",
    
    values_to = "Significancia"
    
  ) %>%
  
  mutate(
    
    Significancia = if_else(
      is.na(Significancia),
      "No evaluada",
      Significancia
    )
    
  )


# ------------------------------------------------------------
# 5. Ordenar variables
# ------------------------------------------------------------

orden_variables <- mapa_dunn_largo %>%
  
  filter(
    Significancia == "Significativa"
  ) %>%
  
  count(
    Variable,
    name = "N_significativas"
  ) %>%
  
  arrange(
    desc(N_significativas)
  ) %>%
  
  pull(
    Variable
  )


orden_variables <- c(
  
  orden_variables,
  
  setdiff(
    unique(mapa_dunn_largo$Variable),
    orden_variables
  )
  
)


mapa_dunn_largo <- mapa_dunn_largo %>%
  
  mutate(
    
    Variable = factor(
      Variable,
      levels = rev(orden_variables)
    ),
    
    Comparacion = factor(
      Comparacion,
      levels = comparaciones
    )
    
  )


# ============================================================
# 6. CREAR GRÁFICA
# ============================================================

grafica_dunn_holm <- ggplot(
  
  mapa_dunn_largo,
  
  aes(
    x = Comparacion,
    y = Variable,
    fill = Significancia
  )
  
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.8
  ) +
  
  geom_point(
    
    data = subset(
      mapa_dunn_largo,
      Significancia == "Significativa"
    ),
    
    shape = 16,
    size = 3
    
  ) +
  
  scale_fill_manual(
    
    values = c(
      
      "Significativa" = "#4472C4",
      
      "No significativa" = "#F2F2F2",
      
      "No evaluada" = "#FFFFFF"
      
    ),
    
    name = NULL
    
  ) +
  
  labs(
    
    title =
      "Comparaciones post-hoc de Dunn según gravedad del accidente",
    
    subtitle =
      "Significancia después del ajuste de Holm",
    
    x = NULL,
    
    y = NULL
    
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    
    plot.title =
      element_text(
        hjust = 0.5,
        face = "bold",
        size = 15
      ),
    
    plot.subtitle =
      element_text(
        hjust = 0.5,
        size = 11
      ),
    
    axis.text.x =
      element_text(
        size = 10
      ),
    
    axis.text.y =
      element_text(
        size = 10
      ),
    
    panel.grid =
      element_blank(),
    
    legend.position =
      "bottom"
    
  )


# ============================================================
# 7. MOSTRAR
# ============================================================

print(
  grafica_dunn_holm
)


# ============================================================
# 8. GUARDAR
# ============================================================

ggsave(
  
  "Mapa_Significancia_Dunn_AJUSTE DE HOLM.png",
  
  plot = grafica_dunn_holm,
  
  width = 14,
  
  height = 10,
  
  units = "in",
  
  dpi = 300
  
)
grafica_dunn_holm
# ============================================================
# GRÁFICA PRINCIPAL PARA LA TESIS
# EXCLUYE VARIABLES DIRECTAMENTE VINCULADAS A LA GRAVEDAD
# ============================================================

tabla_kw_grafica <- tabla_kw_ordenada %>%
  
  filter(
    !Variable %in% c(
      "Número de fallecidos",
      "Número de heridos"
    )
  ) %>%
  
  mutate(
    Variable = factor(
      Variable,
      levels = rev(Variable)
    )
  )


grafica_kw_principal <- ggplot(
  
  tabla_kw_grafica,
  
  aes(
    x = `Épsilon²`,
    y = Variable
  )
  
) +
  
  geom_col(
    fill = "#4472C4",
    width = 0.7
  ) +
  
  geom_text(
    aes(
      label = sprintf(
        "%.3f",
        `Épsilon²`
      )
    ),
    hjust = -0.15,
    size = 3.5
  ) +
  
  scale_x_continuous(
    expand = expansion(
      mult = c(0, 0.10)
    )
  ) +
  
  labs(
    
    title =
      "Tamaño del efecto de las diferencias según gravedad",
    
    subtitle =
      "Variables de vehículos y actores viales",
    
    x =
      "Épsilon cuadrado (ε²)",
    
    y =
      NULL
    
  ) +
  
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    
    plot.title =
      element_text(
        hjust = 0.5,
        face = "bold",
        size = 15
      ),
    
    plot.subtitle =
      element_text(
        hjust = 0.5,
        size = 11
      ),
    
    axis.text.y =
      element_text(
        size = 10
      ),
    
    panel.grid.major.y =
      element_blank(),
    
    panel.grid.minor =
      element_blank()
    
  )


print(
  grafica_kw_principal
)


ggsave(
  "Tamaño_del_efecto_Kruskal_Wallis_VARIABLES_PRINCIPALES.png",
  plot = grafica_kw_principal,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300
)