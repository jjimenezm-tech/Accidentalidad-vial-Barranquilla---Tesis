# ============================================================
# MATRIZ DE V DE CRAMÉR
# ACTORES, VEHÍCULOS, TEMPORAL, ESPACIAL Y SEVERIDAD
# ============================================================


# ============================================================
# 1. PAQUETES
# ============================================================

paquetes <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "readxl",
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
library(readxl)
library(scales)


# ============================================================
# 2. CARGA DE LA BASE DE DATOS
# ============================================================

# Si df ya está cargado, lo conserva.
# Si no, intenta cargar el archivo.

if (!exists("df")) {
  
  df <- read_excel(
    "BASE_DATOS_ACCIDENTES_TRANSITO_BARRANQUILLA_2018_2025.xlsx"
  )
  
}

cat("Número de registros:", nrow(df), "\n")


# ============================================================
# 3. VERIFICAR VARIABLES NECESARIAS
# ============================================================

variables_necesarias <- c(
  
  "Gravedad_Accidente",
  "Clase_Accidente",
  "Año",
  "Mes",
  "Día_Semana",
  "Hora",
  "Tipo_via",
  
  "#_Acompañante_Muerto",
  "#_Acompañante_Herido",
  
  "#_Ciclista_Muerto",
  "#_Ciclista_Herido",
  
  "#_Conductor_Muerto",
  "#_Conductor_Herido",
  
  "#_Motociclista_Muerto",
  "#_Motociclista_Herido",
  
  "#_Pasajero_Muerto",
  "#_Pasajero_Herido",
  
  "#_Peaton_Muerto",
  "#_Peaton_Herido",
  
  "Veh_Motocicleta",
  "Veh_Automovil",
  "Veh_TransportePublico",
  "Veh_Carga",
  "Veh_NoMotorizado",
  "Veh_Otros"
  
)

faltantes <- setdiff(
  variables_necesarias,
  names(df)
)

if (length(faltantes) > 0) {
  
  cat("\nFALTAN LAS SIGUIENTES VARIABLES:\n")
  print(faltantes)
  
  stop(
    "\nEl script se detuvo porque faltan variables necesarias."
  )
}


# ============================================================
# 4. CREAR FRANJA HORARIA
# ============================================================
#
# Se utiliza la variable Hora.
#
# Clasificación:
#
# Madrugada = 00:00 - 05:59
# Mañana    = 06:00 - 11:59
# Tarde     = 12:00 - 17:59
# Noche     = 18:00 - 23:59
#
# ============================================================

# Verificar estructura de Hora

cat("\nEstructura de Hora:\n")
print(str(df$Hora))


# Extraer únicamente la hora

df <- df %>%
  mutate(
    
    Hora_num = as.numeric(
      format(Hora, "%H")
    ),
    
    Franja_Horaria = case_when(
      
      Hora_num >= 0 &
        Hora_num < 6 ~ "Madrugada",
      
      Hora_num >= 6 &
        Hora_num < 12 ~ "Mañana",
      
      Hora_num >= 12 &
        Hora_num < 18 ~ "Tarde",
      
      Hora_num >= 18 &
        Hora_num < 24 ~ "Noche",
      
      TRUE ~ NA_character_
      
    ),
    
    Franja_Horaria = factor(
      Franja_Horaria,
      levels = c(
        "Madrugada",
        "Mañana",
        "Tarde",
        "Noche"
      )
    )
    
  )


# Revisar distribución

cat("\nDistribución de Franja Horaria:\n")

print(
  table(
    df$Franja_Horaria,
    useNA = "ifany"
  )
)


# ============================================================
# 5. CONSTRUIR VARIABLES DE ACTORES
# ============================================================
#
# Actor = muertos + heridos de cada rol.
#
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
# 6. ACTORES COMO PRESENCIA / AUSENCIA
# ============================================================

df <- df %>%
  mutate(
    
    `Actor Peatón` =
      if_else(
        Actor_Peaton > 0,
        "Sí",
        "No"
      ),
    
    `Actor Ciclista` =
      if_else(
        Actor_Ciclista > 0,
        "Sí",
        "No"
      ),
    
    `Actor Motociclista` =
      if_else(
        Actor_Motociclista > 0,
        "Sí",
        "No"
      ),
    
    `Actor Conductor` =
      if_else(
        Actor_Conductor > 0,
        "Sí",
        "No"
      ),
    
    `Actor Pasajero` =
      if_else(
        Actor_Pasajero > 0,
        "Sí",
        "No"
      ),
    
    `Actor Acompañante` =
      if_else(
        Actor_Acompañante > 0,
        "Sí",
        "No"
      )
    
  )


# ============================================================
# 7. VEHÍCULOS COMO PRESENCIA / AUSENCIA
# ============================================================

df <- df %>%
  mutate(
    
    `Veh Motocicleta` =
      if_else(
        Veh_Motocicleta > 0,
        "Sí",
        "No"
      ),
    
    `Veh Automóvil` =
      if_else(
        Veh_Automovil > 0,
        "Sí",
        "No"
      ),
    
    `Veh Transporte público` =
      if_else(
        Veh_TransportePublico > 0,
        "Sí",
        "No"
      ),
    
    `Veh Carga` =
      if_else(
        Veh_Carga > 0,
        "Sí",
        "No"
      ),
    
    `Veh No motorizado` =
      if_else(
        Veh_NoMotorizado > 0,
        "Sí",
        "No"
      ),
    
    `Veh Otros` =
      if_else(
        Veh_Otros > 0,
        "Sí",
        "No"
      )
    
  )


# ============================================================
# 8. PREPARAR TIPO DE VÍA PARA V DE CRAMÉR
# ============================================================
#
# NO se modifica Tipo_via.
#
# Se crea Tipo_via_Cramer.
#
# Categorías principales conservadas:
#
# Intersección
# Avenida Circunvalar
# Carrera
# Calle
# Vía 40
# Avenida Cordialidad
#
# Las restantes se agrupan.
# ============================================================

categorias_principales <- c(
  "Intersección",
  "Avenida Circunvalar",
  "Carrera",
  "Calle",
  "Vía 40",
  "Avenida Cordialidad"
)


df <- df %>%
  mutate(
    
    Tipo_via_Cramer = case_when(
      
      Tipo_via %in% categorias_principales ~
        as.character(Tipo_via),
      
      TRUE ~
        "Otra vía / no clasificada"
      
    )
    
  )


# ============================================================
# 9. CONVERTIR VARIABLES CATEGÓRICAS A FACTOR
# ============================================================

df <- df %>%
  mutate(
    
    Gravedad_Accidente =
      factor(
        Gravedad_Accidente,
        levels = c(
          "Solo daños",
          "Con heridos",
          "Con muertos"
        )
      ),
    
    Clase_Accidente =
      as.factor(Clase_Accidente),
    
    Año =
      as.factor(Año),
    
    Mes =
      as.factor(Mes),
    
    Día_Semana =
      as.factor(Día_Semana),
    
    Franja_Horaria =
      factor(
        Franja_Horaria,
        levels = c(
          "Madrugada",
          "Mañana",
          "Tarde",
          "Noche"
        )
      ),
    
    Tipo_via_Cramer =
      factor(
        Tipo_via_Cramer,
        levels = c(
          "Intersección",
          "Avenida Circunvalar",
          "Carrera",
          "Calle",
          "Vía 40",
          "Avenida Cordialidad",
          "Otra vía / no clasificada"
        )
      )
    
  )


# ============================================================
# 10. SELECCIONAR VARIABLES PARA EL ANÁLISIS
# ============================================================

datos_cramer <- df %>%
  select(
    
    # --------------------------------------------------------
    # Severidad
    # --------------------------------------------------------
    
    `Gravedad del accidente` =
      Gravedad_Accidente,
    
    
    # --------------------------------------------------------
    # Características del accidente
    # --------------------------------------------------------
    
    `Clase de accidente` =
      Clase_Accidente,
    
    
    # --------------------------------------------------------
    # Variables temporales
    # --------------------------------------------------------
    
    Año,
    
    Mes,
    
    `Día de la semana` =
      Día_Semana,
    
    `Franja horaria` =
      Franja_Horaria,
    
    
    # --------------------------------------------------------
    # Variable espacial
    # --------------------------------------------------------
    
    `Tipo de vía` =
      Tipo_via_Cramer,
    
    
    # --------------------------------------------------------
    # Vehículos
    # --------------------------------------------------------
    
    `Veh Motocicleta`,
    
    `Veh Automóvil`,
    
    `Veh Transporte público`,
    
    `Veh Carga`,
    
    `Veh No motorizado`,
    
    `Veh Otros`,
    
    
    # --------------------------------------------------------
    # Actores
    # --------------------------------------------------------
    
    `Actor Peatón`,
    
    `Actor Ciclista`,
    
    `Actor Motociclista`,
    
    `Actor Conductor`,
    
    `Actor Pasajero`,
    
    `Actor Acompañante`
    
  )


# ============================================================
# 11. FUNCIÓN V DE CRAMÉR
# ============================================================

cramer_v <- function(x, y) {
  
  datos <- data.frame(
    x = x,
    y = y
  ) %>%
    filter(
      !is.na(x),
      !is.na(y)
    )
  
  
  datos$x <- factor(datos$x)
  datos$y <- factor(datos$y)
  
  
  tabla <- table(
    datos$x,
    datos$y
  )
  
  
  # Si alguna variable tiene una sola categoría,
  # no es posible calcular asociación.
  
  if (
    nrow(tabla) < 2 ||
    ncol(tabla) < 2
  ) {
    return(NA_real_)
  }
  
  
  prueba <- suppressWarnings(
    chisq.test(
      tabla,
      correct = FALSE
    )
  )
  
  
  n <- sum(tabla)
  r <- nrow(tabla)
  c <- ncol(tabla)
  
  
  V <- sqrt(
    as.numeric(prueba$statistic) /
      (
        n *
          min(
            r - 1,
            c - 1
          )
      )
  )
  
  
  return(V)
  
}


# ============================================================
# 12. CALCULAR MATRIZ DE V DE CRAMÉR
# ============================================================

variables <- names(datos_cramer)


matriz_cramer <- matrix(
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
      
      matriz_cramer[i, j] <- 1
      
    } else {
      
      matriz_cramer[i, j] <-
        cramer_v(
          datos_cramer[[i]],
          datos_cramer[[j]]
        )
      
    }
    
  }
  
}


# ============================================================
# 13. MOSTRAR MATRIZ EN CONSOLA
# ============================================================

cat("\n")
cat("============================================================\n")
cat("MATRIZ DE V DE CRAMÉR\n")
cat("============================================================\n\n")


print(
  round(
    matriz_cramer,
    3
  )
)


# ============================================================
# 14. MATRIZ A FORMATO LARGO
# ============================================================

matriz_larga <- as.data.frame(
  as.table(matriz_cramer)
)


names(matriz_larga) <- c(
  "Variable_X",
  "Variable_Y",
  "V_Cramer"
)


# ============================================================
# 15. GRÁFICA
# ============================================================

grafica_cramer <- ggplot(
  matriz_larga,
  aes(
    x = Variable_X,
    y = Variable_Y,
    fill = V_Cramer
  )
) +
  
  geom_tile(
    color = "white",
    linewidth = 0.4
  ) +
  
  geom_text(
    aes(
      label = ifelse(
        is.na(V_Cramer),
        "",
        sprintf(
          "%.2f",
          V_Cramer
        )
      )
    ),
    size = 3.2
  ) +
  
  scale_fill_gradient(
    low = "#EAF2F8",
    high = "#08519C",
    limits = c(0, 1),
    breaks = seq(
      0,
      1,
      0.2
    ),
    labels = number_format(
      accuracy = 0.1,
      decimal.mark = ","
    ),
    name = "V de Cramér"
  ) +
  
  scale_x_discrete(
    position = "top"
  ) +
  
  labs(
    title =
      "Matriz de asociación mediante V de Cramér",
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
# 16. MOSTRAR GRÁFICA
# ============================================================

print(grafica_cramer)


# ============================================================
# 17. GUARDAR GRÁFICA
# ============================================================

ggsave(
  filename =
    "Matriz_V_Cramer_Actores_Vehiculos.png",
  
  plot =
    grafica_cramer,
  
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
# 18. GUARDAR MATRIZ EN EXCEL
# ============================================================

if (!requireNamespace(
  "openxlsx",
  quietly = TRUE
)) {
  
  install.packages("openxlsx")
  
}

library(openxlsx)


write.xlsx(
  as.data.frame(matriz_cramer),
  file =
    "Matriz_V_Cramer_Actores_Vehiculos.xlsx",
  rowNames = TRUE
)


# ============================================================
# 19. DISTRIBUCIÓN DE FRANJA HORARIA
# ============================================================

cat("\n")
cat("============================================================\n")
cat("DISTRIBUCIÓN DE FRANJA HORARIA\n")
cat("============================================================\n\n")


print(
  table(
    df$Franja_Horaria,
    useNA = "ifany"
  )
)


# ============================================================
# 20. DISTRIBUCIÓN DE TIPO DE VÍA PARA CRAMÉR
# ============================================================

cat("\n")
cat("============================================================\n")
cat("DISTRIBUCIÓN DE TIPO DE VÍA PARA V DE CRAMÉR\n")
cat("============================================================\n\n")


print(
  table(
    df$Tipo_via_Cramer,
    useNA = "ifany"
  )
)


# ============================================================
# 21. FINAL
# ============================================================

cat("\n")
cat("============================================================\n")
cat("ANÁLISIS COMPLETADO CORRECTAMENTE\n")
cat("============================================================\n")

cat(
  "\nArchivos generados:\n",
  "- Matriz_V_Cramer_Actores_Vehiculos.png\n",
  "- Matriz_V_Cramer_Actores_Vehiculos.xlsx\n"
)
# ============================================================
# GRÁFICAS DE ASOCIACIÓN CON LA SEVERIDAD
# V DE CRAMÉR
# ============================================================

library(dplyr)
library(ggplot2)


# ============================================================
# 1. IDENTIFICAR AUTOMÁTICAMENTE LA VARIABLE DE SEVERIDAD
# ============================================================

# Revisar los nombres de la matriz
print(rownames(matriz_cramer))


# Buscar la fila que corresponde a Gravedad
fila_gravedad <- grep(
  "Gravedad",
  rownames(matriz_cramer),
  ignore.case = TRUE
)


# Verificar que se encontró exactamente una
if (length(fila_gravedad) != 1) {
  
  stop(
    paste0(
      "No se pudo identificar de forma única la variable de gravedad. ",
      "Revisa los nombres mostrados anteriormente."
    )
  )
}


# Nombre exacto utilizado por la matriz
nombre_gravedad <- rownames(matriz_cramer)[fila_gravedad]


cat(
  "\nVariable utilizada como severidad:",
  nombre_gravedad,
  "\n"
)


# ============================================================
# 2. EXTRAER LAS ASOCIACIONES CON LA SEVERIDAD
# ============================================================

tabla_severidad <- data.frame(
  
  Variable = colnames(matriz_cramer),
  
  V_Cramer = as.numeric(
    matriz_cramer[
      fila_gravedad,
    ]
  )
  
) %>%
  
  # Eliminar la propia variable de gravedad
  filter(
    Variable != nombre_gravedad,
    !is.na(V_Cramer)
  ) %>%
  
  # Ordenar de mayor a menor
  arrange(
    desc(V_Cramer)
  )


# ============================================================
# 3. CAMBIAR LOS NOMBRES PARA LA GRÁFICA
# ============================================================

tabla_severidad <- tabla_severidad %>%
  
  mutate(
    
    Variable = case_when(
      
      Variable == "Veh Motocicleta" ~
        "Motocicleta",
      
      Variable == "Veh Automóvil" ~
        "Automóvil",
      
      Variable == "Veh Transporte público" ~
        "Transporte público",
      
      Variable == "Veh Carga" ~
        "Carga",
      
      Variable == "Veh No motorizado" ~
        "No motorizado",
      
      Variable == "Veh Otros" ~
        "Otros",
      
      Variable == "Actor Peatón" ~
        "Actor peatón",
      
      Variable == "Actor Ciclista" ~
        "Actor ciclista",
      
      Variable == "Actor Motociclista" ~
        "Actor motociclista",
      
      Variable == "Actor Conductor" ~
        "Actor conductor",
      
      Variable == "Actor Pasajero" ~
        "Actor pasajero",
      
      Variable == "Actor Acompañante" ~
        "Actor acompañante",
      
      Variable == "Clase de accidente" ~
        "Clase de accidente",
      
      Variable == "Tipo de vía" ~
        "Tipo de vía",
      
      Variable == "Franja horaria" ~
        "Franja horaria",
      
      Variable == "Día de la semana" ~
        "Día de la semana",
      
      Variable == "Año" ~
        "Año",
      
      Variable == "Mes" ~
        "Mes",
      
      TRUE ~
        Variable
      
    )
    
  )


# ============================================================
# 4. REVISAR LA TABLA
# ============================================================

cat("\n")
cat("============================================================\n")
cat("ASOCIACIONES CON LA GRAVEDAD DEL ACCIDENTE\n")
cat("============================================================\n\n")

print(
  tabla_severidad
)


# ============================================================
# 5. GRÁFICA COMPLETA
# ============================================================

grafica_completa <- ggplot(
  
  tabla_severidad,
  
  aes(
    x = reorder(
      Variable,
      V_Cramer
    ),
    
    y = V_Cramer
  )
  
) +
  
  geom_col(
    fill = "#4472C4",
    width = 0.70
  ) +
  
  geom_text(
    
    aes(
      label = sprintf(
        "%.3f",
        V_Cramer
      )
    ),
    
    hjust = -0.15,
    
    size = 4
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    
    limits = c(
      0,
      max(
        tabla_severidad$V_Cramer,
        na.rm = TRUE
      ) + 0.08
    ),
    
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
    
  ) +
  
  labs(
    
    title =
      "Asociación entre las variables y la gravedad de los accidentes",
    
    x =
      "Variable categórica",
    
    y =
      "V de Cramér"
    
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    
    plot.title =
      element_text(
        hjust = 0.5,
        face = "bold",
        size = 15
      ),
    
    axis.title =
      element_text(
        face = "bold"
      ),
    
    axis.text.y =
      element_text(
        size = 11
      ),
    
    panel.grid.major.y =
      element_blank(),
    
    panel.grid.minor =
      element_blank()
    
  )


# Mostrar gráfica
print(grafica_completa)


# ============================================================
# 6. GUARDAR GRÁFICA COMPLETA
# ============================================================

ggsave(
  
  filename =
    "Grafica_V_Cramer_Completa.png",
  
  plot =
    grafica_completa,
  
  width =
    10,
  
  height =
    8,
  
  units =
    "in",
  
  dpi =
    300
)


# ============================================================
# 7. SELECCIONAR VARIABLES RELEVANTES
# ============================================================
#
# Criterio descriptivo:
# V de Cramér >= 0.20
# ============================================================

tabla_relevantes <- tabla_severidad %>%
  
  filter(
    V_Cramer >= 0.20
  )


# Mostrar tabla
cat("\n")
cat("============================================================\n")
cat("VARIABLES RELEVANTES (V >= 0.20)\n")
cat("============================================================\n\n")

print(
  tabla_relevantes
)


# ============================================================
# 8. GRÁFICA DE VARIABLES RELEVANTES
# ============================================================

grafica_relevantes <- ggplot(
  
  tabla_relevantes,
  
  aes(
    x = reorder(
      Variable,
      V_Cramer
    ),
    
    y = V_Cramer
  )
  
) +
  
  geom_col(
    fill = "#4472C4",
    width = 0.70
  ) +
  
  geom_text(
    
    aes(
      label = sprintf(
        "%.3f",
        V_Cramer
      )
    ),
    
    hjust = -0.15,
    
    size = 4
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    
    limits = c(
      0,
      max(
        tabla_relevantes$V_Cramer,
        na.rm = TRUE
      ) + 0.08
    ),
    
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
    
  ) +
  
  labs(
    
    title =
      "Principales variables asociadas con la gravedad de los accidentes",
    
    x =
      "Variable categórica",
    
    y =
      "V de Cramér"
    
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    
    plot.title =
      element_text(
        hjust = 0.5,
        face = "bold",
        size = 15
      ),
    
    axis.title =
      element_text(
        face = "bold"
      ),
    
    axis.text.y =
      element_text(
        size = 11
      ),
    
    panel.grid.major.y =
      element_blank(),
    
    panel.grid.minor =
      element_blank()
    
  )


# Mostrar gráfica
print(grafica_relevantes)


# ============================================================
# 9. GUARDAR GRÁFICA DE VARIABLES RELEVANTES
# ============================================================

ggsave(
  
  filename =
    "Grafica_V_Cramer_Variables_Relevantes.png",
  
  plot =
    grafica_relevantes,
  
  width =
    10,
  
  height =
    6,
  
  units =
    "in",
  
  dpi =
    300
)