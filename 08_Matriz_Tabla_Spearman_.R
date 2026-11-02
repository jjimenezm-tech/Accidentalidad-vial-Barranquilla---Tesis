# ============================================================
# MATRIZ DE CORRELACIÓN DE SPEARMAN
# Y TABLA DE ASOCIACIONES RELEVANTES
# ============================================================


# ============================================================
# 1. PAQUETES
# ============================================================

paquetes <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "scales"
)

instalar <- paquetes[
  !(paquetes %in% installed.packages()[, "Package"])
]

if (length(instalar) > 0) {
  install.packages(instalar)
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


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
# Los actores se expresan como cantidad de personas
# involucradas, NO como Sí/No.
#
# Esto es lo adecuado para Spearman.
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
# 4. CREAR GRAVEDAD COMO VARIABLE ORDINAL
# ============================================================
#
# Se asigna:
#
# Solo daños  = 1
# Con heridos = 2
# Con muertos = 3
#
# Esto permite utilizar Spearman respetando el orden
# natural de la severidad.
# ============================================================

df <- df %>%
  mutate(
    
    Gravedad_num = case_when(
      
      Gravedad_Accidente == "Solo daños" ~ 1,
      
      Gravedad_Accidente == "Con heridos" ~ 2,
      
      Gravedad_Accidente == "Con muertos" ~ 3,
      
      TRUE ~ NA_real_
    )
    
  )


# ============================================================
# 5. VERIFICAR NÚMERO TOTAL DE VEHÍCULOS
# ============================================================

if (!"Número_total_vehiculos" %in% names(df)) {
  
  stop(
    paste0(
      "No se encontró 'Número_total_vehiculos'. ",
      "Revisa el nombre exacto de la variable en names(df)."
    )
  )
}


# ============================================================
# 6. VERIFICAR VARIABLES PRINCIPALES
# ============================================================

variables_necesarias <- c(
  
  # Gravedad
  "Gravedad_num",
  
  # Vehículos
  "Veh_Motocicleta",
  "Veh_Automovil",
  "Veh_TransportePublico",
  "Veh_Carga",
  "Veh_NoMotorizado",
  "Veh_Otros",
  
  # Actores
  "Actor_Peaton",
  "Actor_Ciclista",
  "Actor_Motociclista",
  "Actor_Conductor",
  "Actor_Pasajero",
  "Actor_Acompañante",
  
  # Otras variables
  "Número_total_vehiculos",
  "Num_Heridos",
  "Num_Fallecidos",
  "Edad_Promedio_De_Las_Victimas"
  
)

faltantes <- setdiff(
  variables_necesarias,
  names(df)
)

if (length(faltantes) > 0) {
  
  cat("\nFALTAN LAS SIGUIENTES VARIABLES:\n")
  print(faltantes)
  
  stop(
    "\nCorrige los nombres antes de continuar."
  )
}


# ============================================================
# 7. CONSTRUIR BASE PARA SPEARMAN
# ============================================================

datos_spearman <- df %>%
  
  transmute(
    
    `Gravedad` =
      Gravedad_num,
    
    `Motocicleta` =
      as.numeric(Veh_Motocicleta),
    
    `Automóvil` =
      as.numeric(Veh_Automovil),
    
    `Transporte público` =
      as.numeric(Veh_TransportePublico),
    
    `Carga` =
      as.numeric(Veh_Carga),
    
    `No motorizado` =
      as.numeric(Veh_NoMotorizado),
    
    `Otros vehículos` =
      as.numeric(Veh_Otros),
    
    `Total de vehículos` =
      as.numeric(Número_total_vehiculos),
    
    `Actor peatón` =
      as.numeric(Actor_Peaton),
    
    `Actor ciclista` =
      as.numeric(Actor_Ciclista),
    
    `Actor motociclista` =
      as.numeric(Actor_Motociclista),
    
    `Actor conductor` =
      as.numeric(Actor_Conductor),
    
    `Actor pasajero` =
      as.numeric(Actor_Pasajero),
    
    `Actor acompañante` =
      as.numeric(Actor_Acompañante),
    
    `Número de heridos` =
      as.numeric(Num_Heridos),
    
    `Número de fallecidos` =
      as.numeric(Num_Fallecidos),
    
    `Edad promedio de las víctimas` =
      as.numeric(Edad_Promedio_De_Las_Victimas)
    
  )


# ============================================================
# 8. FUNCIÓN PARA CALCULAR SPEARMAN
# ============================================================

spearman_rho <- function(x, y) {
  
  datos <- data.frame(
    x = x,
    y = y
  ) %>%
    filter(
      !is.na(x),
      !is.na(y)
    )
  
  if (
    length(unique(datos$x)) < 2 ||
    length(unique(datos$y)) < 2
  ) {
    return(NA_real_)
  }
  
  prueba <- suppressWarnings(
    cor.test(
      datos$x,
      datos$y,
      method = "spearman",
      exact = FALSE
    )
  )
  
  return(
    as.numeric(
      prueba$estimate
    )
  )
}


# ============================================================
# 9. FUNCIÓN PARA OBTENER P-VALOR
# ============================================================

spearman_p <- function(x, y) {
  
  datos <- data.frame(
    x = x,
    y = y
  ) %>%
    filter(
      !is.na(x),
      !is.na(y)
    )
  
  if (
    length(unique(datos$x)) < 2 ||
    length(unique(datos$y)) < 2
  ) {
    return(NA_real_)
  }
  
  prueba <- suppressWarnings(
    cor.test(
      datos$x,
      datos$y,
      method = "spearman",
      exact = FALSE
    )
  )
  
  return(
    as.numeric(
      prueba$p.value
    )
  )
}


# ============================================================
# 10. CREAR MATRIZ DE SPEARMAN
# ============================================================

variables <- names(datos_spearman)

matriz_spearman <- matrix(
  
  NA_real_,
  
  nrow = length(variables),
  
  ncol = length(variables),
  
  dimnames = list(
    variables,
    variables
  )
)


for (i in seq_along(variables)) {
  
  for (j in seq_along(variables)) {
    
    if (i == j) {
      
      matriz_spearman[i, j] <- 1
      
    } else {
      
      matriz_spearman[i, j] <-
        spearman_rho(
          datos_spearman[[i]],
          datos_spearman[[j]]
        )
      
    }
    
  }
  
}


# ============================================================
# 11. MOSTRAR MATRIZ
# ============================================================

cat("\n")
cat("============================================================\n")
cat("MATRIZ DE CORRELACIÓN DE SPEARMAN\n")
cat("============================================================\n\n")

print(
  round(
    matriz_spearman,
    3
  )
)


# ============================================================
# 12. PASAR MATRIZ A FORMATO LARGO
# ============================================================

matriz_larga <- as.data.frame(
  as.table(matriz_spearman)
)

names(matriz_larga) <- c(
  "Variable_1",
  "Variable_2",
  "Rho"
)


# ============================================================
# 13. GRÁFICA DE LA MATRIZ
# ============================================================

grafica_spearman <- ggplot(
  
  matriz_larga,
  
  aes(
    x = Variable_1,
    y = Variable_2,
    fill = Rho
  )
  
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.4
  ) +
  
  geom_text(
    
    aes(
      label = ifelse(
        is.na(Rho),
        "",
        sprintf(
          "%.2f",
          Rho
        )
      )
    ),
    
    size = 3.1
  ) +
  
  scale_fill_gradient2(
    
    low = "#D73027",
    
    mid = "#FFFFFF",
    
    high = "#2166AC",
    
    midpoint = 0,
    
    limits = c(
      -1,
      1
    ),
    
    breaks = seq(
      -1,
      1,
      0.5
    ),
    
    labels = number_format(
      accuracy = 0.1,
      decimal.mark = ","
    ),
    
    name = expression(
      rho ~ "de Spearman"
    )
    
  ) +
  
  scale_x_discrete(
    position = "top"
  ) +
  
  labs(
    
    title =
      "Matriz de correlación de Spearman",
    
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
    
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 0,
        vjust = 0,
        size = 10
      ),
    
    axis.text.y =
      element_text(
        size = 10
      ),
    
    panel.grid =
      element_blank(),
    
    legend.title =
      element_text(
        face = "bold"
      ),
    
    legend.position =
      "right",
    
    plot.margin =
      margin(
        20,
        20,
        20,
        20
      )
    
  )


# ============================================================
# 14. MOSTRAR MATRIZ
# ============================================================

print(
  grafica_spearman
)


# ============================================================
# 15. GUARDAR MATRIZ
# ============================================================

ggsave(
  
  filename =
    "Matriz_Spearman_Actores_Vehiculos.png",
  
  plot =
    grafica_spearman,
  
  width =
    16,
  
  height =
    14,
  
  units =
    "in",
  
  dpi =
    300
)


# ============================================================
# 16. CALCULAR P-VALORES PARA TODAS LAS PAREJAS
# ============================================================

resultados_p <- list()

contador <- 1


for (i in 1:(length(variables) - 1)) {
  
  for (j in (i + 1):length(variables)) {
    
    x <- datos_spearman[[i]]
    y <- datos_spearman[[j]]
    
    datos <- data.frame(
      x = x,
      y = y
    ) %>%
      filter(
        !is.na(x),
        !is.na(y)
      )
    
    if (
      length(unique(datos$x)) >= 2 &&
      length(unique(datos$y)) >= 2
    ) {
      
      prueba <- suppressWarnings(
        cor.test(
          datos$x,
          datos$y,
          method = "spearman",
          exact = FALSE
        )
      )
      
      resultados_p[[contador]] <- data.frame(
        
        Variable_1 =
          variables[i],
        
        Variable_2 =
          variables[j],
        
        rho =
          as.numeric(
            prueba$estimate
          ),
        
        p =
          as.numeric(
            prueba$p.value
          )
        
      )
      
      contador <- contador + 1
      
    }
    
  }
  
}


tabla_asociaciones <- bind_rows(
  resultados_p
)


# ============================================================
# 17. SELECCIONAR ASOCIACIONES RELEVANTES
# ============================================================
#
# Criterio:
#
# |rho| >= 0.30
#
# y p < 0.05
#
# El criterio se utiliza como filtro descriptivo para
# identificar asociaciones de magnitud apreciable.
# ============================================================

tabla_relevantes <- tabla_asociaciones %>%
  
  filter(
    abs(rho) >= 0.30,
    p < 0.05
  ) %>%
  
  arrange(
    desc(abs(rho))
  )


# ============================================================
# 18. FORMATEAR P-VALORES
# ============================================================

tabla_relevantes <- tabla_relevantes %>%
  
  mutate(
    
    `rho (p)` = case_when(
      
      p < 0.001 ~
        sprintf(
          "%.3f (<0,001)",
          rho
        ),
      
      TRUE ~
        sprintf(
          "%.3f (%.3f)",
          rho,
          p
        )
      
    )
    
  ) %>%
  
  select(
    `Variable 1` =
      Variable_1,
    
    `Variable 2` =
      Variable_2,
    
    `rho (p)`
  )


# ============================================================
# 19. MOSTRAR TABLA DE ASOCIACIONES RELEVANTES
# ============================================================

cat("\n")
cat("============================================================\n")
cat("ASOCIACIONES RELEVANTES DE SPEARMAN\n")
cat("============================================================\n\n")

print(
  tabla_relevantes,
  row.names = FALSE
)


# ============================================================
# 20. GUARDAR TABLA
# ============================================================

if (!requireNamespace(
  "openxlsx",
  quietly = TRUE
)) {
  
  install.packages("openxlsx")
  
}

library(openxlsx)


write.xlsx(
  
  tabla_relevantes,
  
  file =
    "Tabla_Asociaciones_Relevantes_Spearman.xlsx",
  
  rowNames = FALSE
)


# ============================================================
# 21. GUARDAR TODAS LAS ASOCIACIONES
# ============================================================

write.xlsx(
  
  tabla_asociaciones,
  
  file =
    "Tabla_Todas_Asociaciones_Spearman.xlsx",
  
  rowNames = FALSE
)


# ============================================================
# 22. FINAL
# ============================================================

cat("\n")
cat("============================================================\n")
cat("ANÁLISIS DE SPEARMAN COMPLETADO\n")
cat("============================================================\n\n")

cat(
  "Archivos generados:\n",
  "- Matriz_Spearman_Actores_Vehiculos.png\n",
  "- Tabla_Asociaciones_Relevantes_Spearman.xlsx\n",
  "- Tabla_Todas_Asociaciones_Spearman.xlsx\n"
)