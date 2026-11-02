# ============================================================
# ANALISIS DESCRIPTIVO
# Barranquilla, 2018–2025
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ============================================================
# 1. CARGAR BASE Y CARPETA DE SALIDA
# ============================================================

cat("Selecciona la base de datos Excel...\n")
ruta_archivo <- file.choose()
df <- readxl::read_excel(ruta_archivo)

cat("Base cargada:", nrow(df), "registros.\n")

carpeta_salida <- file.path(
  dirname(ruta_archivo),
  "Resultados_Graficos_Fase_2"
)

if (!dir.exists(carpeta_salida)) {
  dir.create(carpeta_salida, recursive = TRUE)
}

graficos_guardados <- character(0)

# ============================================================
# 2. COLORES
# ============================================================

colores_severidad <- c(
  "Solo daños" = "#4472C4",
  "Con heridos" = "#ED7D31",
  "Con muertos" = "#C00000"
)

azul_univariado <- "#4472C4"

colores_calidos <- c(
  "#FFF9C4",
  "#FDD76A",
  "#F7943D",
  "#EF3B2C",
  "#99000D"
)

color_fallecidos <- "#C00000"

# ============================================================
# 3. FUNCIONES AUXILIARES
# ============================================================

normalizar_nombre <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]", "", x)
}

buscar_columna <- function(candidatas, nombres = names(df)) {
  nombres_norm <- normalizar_nombre(nombres)
  candidatas_norm <- normalizar_nombre(candidatas)
  indice <- match(candidatas_norm, nombres_norm)
  indice <- indice[!is.na(indice)]
  
  if (length(indice) == 0) {
    return(NA_character_)
  }
  
  nombres[indice[1]]
}

exigir_columna <- function(candidatas, etiqueta) {
  columna <- buscar_columna(candidatas)
  
  if (is.na(columna)) {
    stop(
      paste0(
        "No se encontro la columna: ", etiqueta,
        "\nColumnas disponibles:\n",
        paste(names(df), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  columna
}

limpiar_texto <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  x
}

estandarizar_gravedad <- function(x) {
  original <- limpiar_texto(x)
  
  normalizado <- tolower(
    iconv(
      original,
      from = "",
      to = "ASCII//TRANSLIT"
    )
  )
  
  resultado <- rep(NA_character_, length(normalizado))
  
  resultado[
    grepl("solo.*dan|dan.*solo", normalizado)
  ] <- "Solo daños"
  
  resultado[
    grepl("herid", normalizado)
  ] <- "Con heridos"
  
  resultado[
    grepl("muert|fallecid", normalizado)
  ] <- "Con muertos"
  
  factor(
    resultado,
    levels = c(
      "Solo daños",
      "Con heridos",
      "Con muertos"
    )
  )
}

guardar_grafico <- function(grafico, nombre, ancho = 10, alto = 6) {
  
  archivo <- file.path(
    carpeta_salida,
    paste0(nombre, ".png")
  )
  
  print(grafico)
  
  ggsave(
    filename = archivo,
    plot = grafico,
    width = ancho,
    height = alto,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  
  if (!file.exists(archivo)) {
    stop(
      paste0(
        "No se pudo guardar el grafico: ",
        archivo
      ),
      call. = FALSE
    )
  }
  
  graficos_guardados <<- c(
    graficos_guardados,
    archivo
  )
  
  cat("Guardado:", archivo, "\n")
}

extraer_hora <- function(x) {
  
  if (inherits(x, "POSIXt")) {
    return(as.integer(format(x, "%H")))
  }
  
  if (is.numeric(x)) {
    resultado <- ifelse(
      x >= 0 & x < 1,
      floor(x * 24),
      x
    )
    
    resultado[
      resultado < 0 |
        resultado > 23
    ] <- NA
    
    return(as.integer(resultado))
  }
  
  x <- trimws(as.character(x))
  resultado <- rep(NA_integer_, length(x))
  
  tiene_dos_puntos <- grepl(
    "^[0-9]{1,2}:",
    x
  )
  
  resultado[tiene_dos_puntos] <- suppressWarnings(
    as.integer(
      sub(
        "^([0-9]{1,2}):.*$",
        "\\1",
        x[tiene_dos_puntos]
      )
    )
  )
  
  solo_numero <- !tiene_dos_puntos &
    grepl("^[0-9]{1,2}$", x)
  
  resultado[solo_numero] <- suppressWarnings(
    as.integer(x[solo_numero])
  )
  
  resultado[
    resultado < 0 |
      resultado > 23
  ] <- NA
  
  resultado
}

# ============================================================
# 4. IDENTIFICAR COLUMNAS
# ============================================================

col_gravedad <- exigir_columna(
  c(
    "Gravedad_Accidente",
    "Gravedad Accidente",
    "Severidad_Accidente"
  ),
  "gravedad del accidente"
)

col_clase <- exigir_columna(
  c(
    "Clase_Accidente",
    "Clase Accidente"
  ),
  "clase de accidente"
)

col_anio <- exigir_columna(
  c("Año", "Anio"),
  "año"
)

col_mes <- exigir_columna(
  c("Mes"),
  "mes"
)

col_dia <- exigir_columna(
  c(
    "Día_Semana",
    "Dia_Semana",
    "Día Semana",
    "Dia Semana"
  ),
  "dia de la semana"
)

col_tipo_via <- exigir_columna(
  c(
    "Tipo_Via",
    "Tipo_via",
    "Tipo de via"
  ),
  "tipo de via"
)

col_hora <- buscar_columna(c("Hora"))

col_franja <- buscar_columna(
  c(
    "Franja_Horaria",
    "Franja Horaria"
  )
)

col_edad <- buscar_columna(
  c(
    "Edad_Promedio_De_Las_Victimas",
    "Edad_Promedio_Victimas",
    "Edad promedio de las victimas"
  )
)

col_total_veh <- buscar_columna(
  c(
    "Numero_total_vehiculos",
    "Num_Total_Vehiculos",
    "Numero_Total_Vehiculos"
  )
)

# ============================================================
# 5. PREPARACION
# ============================================================

df$Gravedad_graf <- estandarizar_gravedad(
  df[[col_gravedad]]
)

df$Clase_graf <- limpiar_texto(
  df[[col_clase]]
)

df$Anio_graf <- as.character(
  df[[col_anio]]
)

# ------------------------------------------------------------
# Mes
# ------------------------------------------------------------

mes_original <- df[[col_mes]]

if (is.numeric(mes_original)) {
  
  mes_num <- as.integer(mes_original)
  
} else {
  
  mes_texto <- tolower(
    iconv(
      limpiar_texto(mes_original),
      from = "",
      to = "ASCII//TRANSLIT"
    )
  )
  
  mes_num <- suppressWarnings(
    as.integer(mes_texto)
  )
  
  nombres_meses <- c(
    "enero",
    "febrero",
    "marzo",
    "abril",
    "mayo",
    "junio",
    "julio",
    "agosto",
    "septiembre",
    "octubre",
    "noviembre",
    "diciembre"
  )
  
  mes_num[is.na(mes_num)] <- match(
    mes_texto[is.na(mes_num)],
    nombres_meses
  )
}

meses <- c(
  "Enero",
  "Febrero",
  "Marzo",
  "Abril",
  "Mayo",
  "Junio",
  "Julio",
  "Agosto",
  "Septiembre",
  "Octubre",
  "Noviembre",
  "Diciembre"
)

df$Mes_graf <- factor(
  meses[mes_num],
  levels = meses
)

# ------------------------------------------------------------
# Dia de la semana
# ------------------------------------------------------------

dias_semana <- c(
  "Lunes",
  "Martes",
  "Miércoles",
  "Jueves",
  "Viernes",
  "Sábado",
  "Domingo"
)

dias_normalizados <- tolower(
  iconv(
    limpiar_texto(df[[col_dia]]),
    from = "",
    to = "ASCII//TRANSLIT"
  )
)

dias_normalizados <- ifelse(
  dias_normalizados == "miercoles",
  "Miércoles",
  ifelse(
    dias_normalizados == "sabado",
    "Sábado",
    tools::toTitleCase(dias_normalizados)
  )
)

df$Dia_graf <- factor(
  dias_normalizados,
  levels = dias_semana
)

# ------------------------------------------------------------
# Tipo de via
# ------------------------------------------------------------

tipo_via_original <- limpiar_texto(
  df[[col_tipo_via]]
)

tipo_via_normalizado <- tolower(
  iconv(
    tipo_via_original,
    from = "",
    to = "ASCII//TRANSLIT"
  )
)

categorias_agrupar <- c(
  "calle 30",
  "avenida del rio",
  "avenida murillo",
  "via juan mina",
  "via galapa",
  "via prosperidad",
  "avenida ciudad caribe",
  "parqueadero",
  "puente pumarejo"
)

categorias_agrupar_norm <- tolower(
  iconv(
    categorias_agrupar,
    from = "",
    to = "ASCII//TRANSLIT"
  )
)

df$Tipo_Via_graf <- tipo_via_original

df$Tipo_Via_graf[
  tipo_via_normalizado %in%
    categorias_agrupar_norm
] <- "Otra vía / no clasificada"

df$Tipo_Via_graf <- factor(
  df$Tipo_Via_graf,
  levels = unique(df$Tipo_Via_graf)
)

# ------------------------------------------------------------
# Franja horaria
# ------------------------------------------------------------

if (is.na(col_franja)) {
  
  if (is.na(col_hora)) {
    stop(
      "No existe la columna Hora ni Franja_Horaria."
    )
  }
  
  hora_num <- extraer_hora(
    df[[col_hora]]
  )
  
  df$Franja_graf <- dplyr::case_when(
    hora_num >= 0 & hora_num < 6 ~ "Madrugada",
    hora_num >= 6 & hora_num < 12 ~ "Mañana",
    hora_num >= 12 & hora_num < 18 ~ "Tarde",
    hora_num >= 18 & hora_num <= 23 ~ "Noche",
    TRUE ~ NA_character_
  )
  
} else {
  
  df$Franja_graf <- limpiar_texto(
    df[[col_franja]]
  )
}

# Orden utilizado en la grafica enviada
df$Franja_graf <- factor(
  df$Franja_graf,
  levels = c(
    "Madrugada",
    "Mañana",
    "Noche",
    "Tarde"
  )
)

# ------------------------------------------------------------
# Vehiculos
# ------------------------------------------------------------

familias_vehiculos <- c(
  "Veh_Motocicleta",
  "Veh_Automovil",
  "Veh_TransportePublico",
  "Veh_Carga",
  "Veh_NoMotorizado",
  "Veh_Otros"
)

columnas_vehiculos <- names(df)[
  normalizar_nombre(names(df)) %in%
    normalizar_nombre(familias_vehiculos)
]

if (length(columnas_vehiculos) == 0) {
  stop(
    "No se encontraron columnas de tipos de vehiculos."
  )
}

for (col in columnas_vehiculos) {
  
  df[[col]] <- suppressWarnings(
    as.numeric(as.character(df[[col]]))
  )
  
  df[[col]][is.na(df[[col]])] <- 0
}

if (is.na(col_total_veh)) {
  
  df$Total_vehiculos_graf <- rowSums(
    df[, columnas_vehiculos, drop = FALSE],
    na.rm = TRUE
  )
  
} else {
  
  df$Total_vehiculos_graf <- suppressWarnings(
    as.numeric(
      as.character(
        df[[col_total_veh]]
      )
    )
  )
  
  faltantes <- is.na(
    df$Total_vehiculos_graf
  )
  
  df$Total_vehiculos_graf[
    faltantes
  ] <- rowSums(
    df[
      faltantes,
      columnas_vehiculos,
      drop = FALSE
    ],
    na.rm = TRUE
  )
}

# ============================================================
# 6. FRECUENCIA DE ACCIDENTES SEGUN GRAVEDAD
# ============================================================

datos <- df %>%
  filter(!is.na(Gravedad_graf)) %>%
  count(
    Gravedad_graf,
    name = "Accidentes"
  )

total_accidentes <- sum(
  datos$Accidentes
)

datos <- datos %>%
  mutate(
    Porcentaje = Accidentes /
      total_accidentes * 100,
    Etiqueta = paste0(
      scales::number(
        Accidentes,
        big.mark = ".",
        decimal.mark = ","
      ),
      "\n(",
      scales::number(
        Porcentaje,
        accuracy = 0.01,
        decimal.mark = ","
      ),
      "%)"
    )
  )

p1 <- ggplot(
  datos,
  aes(
    x = Gravedad_graf,
    y = Accidentes,
    fill = Gravedad_graf
  )
) +
  geom_col(width = 0.72) +
  geom_text(
    aes(label = Etiqueta),
    vjust = -0.35,
    size = 3.7,
    family = "Arial"
  ) +
  scale_fill_manual(
    values = colores_severidad,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = "."),
    expand = expansion(
      mult = c(0, 0.15)
    )
  ) +
  labs(
    title = "Frecuencia de accidentes según gravedad",
    x = "Gravedad del accidente",
    y = "Número de accidentes",
    fill = "Gravedad"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      family = "Arial"
    ),
    legend.position = "none",
    text = element_text(
      family = "Arial"
    )
  )

guardar_grafico(
  p1,
  "01_Frecuencia_Accidentes_por_Gravedad"
)

# ============================================================
# 7. FRECUENCIA SEGUN CLASE DE ACCIDENTE
# ============================================================

datos <- df %>%
  filter(!is.na(Clase_graf)) %>%
  count(
    Clase_graf,
    name = "Accidentes"
  ) %>%
  arrange(Accidentes)

p2 <- ggplot(
  datos,
  aes(
    x = reorder(Clase_graf, Accidentes),
    y = Accidentes
  )
) +
  geom_col(
    fill = azul_univariado,
    width = 0.72
  ) +
  geom_text(
    aes(
      label = scales::number(
        Accidentes,
        big.mark = "."
      )
    ),
    hjust = -0.12,
    size = 3.5,
    family = "Arial"
  ) +
  scale_y_log10(
    labels = label_number(
      big.mark = "."
    )
  ) +
  coord_flip() +
  labs(
    title = "Frecuencia de accidentes según clase",
    x = "Clase de accidente",
    y = "Número de accidentes (escala logarítmica base 10)"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      family = "Arial"
    ),
    text = element_text(
      family = "Arial"
    )
  )

guardar_grafico(
  p2,
  "02_Frecuencia_Accidentes_por_Clase",
  ancho = 11,
  alto = 7
)

# ============================================================
# 8. FRECUENCIA SEGUN TIPO DE VIA
# ============================================================

datos <- df %>%
  filter(!is.na(Tipo_Via_graf)) %>%
  count(
    Tipo_Via_graf,
    name = "Accidentes"
  ) %>%
  arrange(Accidentes)

total_via <- sum(datos$Accidentes)

datos <- datos %>%
  mutate(
    Porcentaje = Accidentes /
      total_via * 100,
    Etiqueta = paste0(
      scales::number(
        Accidentes,
        big.mark = "."
      ),
      "\n(",
      scales::number(
        Porcentaje,
        accuracy = 0.01,
        decimal.mark = ","
      ),
      "%)"
    )
  )

p3 <- ggplot(
  datos,
  aes(
    x = reorder(Tipo_Via_graf, Accidentes),
    y = Accidentes
  )
) +
  geom_col(
    fill = azul_univariado,
    width = 0.72
  ) +
  geom_text(
    aes(label = Etiqueta),
    hjust = -0.12,
    size = 3.5,
    family = "Arial"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = label_number(
      big.mark = "."
    ),
    expand = expansion(
      mult = c(0, 0.18)
    )
  ) +
  labs(
    title = "Frecuencia de accidentes según tipo de vía",
    x = "Tipo de vía",
    y = "Número de accidentes"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      family = "Arial"
    ),
    text = element_text(
      family = "Arial"
    )
  )

guardar_grafico(
  p3,
  "03_Frecuencia_Accidentes_por_Tipo_de_Via",
  ancho = 11,
  alto = 8
)

# ============================================================
# 9. FRECUENCIA SEGUN TIPO DE VEHICULO Y GRAVEDAD
# ============================================================

datos_vehiculos <- df %>%
  mutate(
    Automóvil = if_else(Veh_Automovil > 0, 1, 0),
    Motocicleta = if_else(Veh_Motocicleta > 0, 1, 0),
    `Transporte público` = if_else(
      Veh_TransportePublico > 0, 1, 0
    ),
    Carga = if_else(
      Veh_Carga > 0, 1, 0
    ),
    `No motorizado` = if_else(
      Veh_NoMotorizado > 0, 1, 0
    ),
    Otros = if_else(
      Veh_Otros > 0, 1, 0
    )
  )

datos_largos_vehiculos <- datos_vehiculos %>%
  select(
    Gravedad_graf,
    Automóvil,
    Motocicleta,
    `Transporte público`,
    Carga,
    `No motorizado`,
    Otros
  ) %>%
  pivot_longer(
    cols = c(
      Automóvil,
      Motocicleta,
      `Transporte público`,
      Carga,
      `No motorizado`,
      Otros
    ),
    names_to = "Tipo_Vehiculo",
    values_to = "Presencia"
  ) %>%
  filter(
    Presencia == 1,
    !is.na(Gravedad_graf)
  ) %>%
  mutate(
    Tipo_Vehiculo = factor(
      Tipo_Vehiculo,
      levels = c(
        "Automóvil",
        "Motocicleta",
        "Transporte público",
        "Carga",
        "No motorizado",
        "Otros"
      )
    ),
    Gravedad_graf = factor(
      Gravedad_graf,
      levels = c(
        "Solo daños",
        "Con heridos",
        "Con muertos"
      )
    )
  )

tabla_grafico_vehiculos <- datos_largos_vehiculos %>%
  count(
    Tipo_Vehiculo,
    Gravedad_graf,
    name = "Cantidad"
  )

totales_vehiculos <- tabla_grafico_vehiculos %>%
  group_by(Tipo_Vehiculo) %>%
  summarise(
    Total = sum(Cantidad),
    .groups = "drop"
  )

p4 <- ggplot(
  tabla_grafico_vehiculos,
  aes(
    x = Tipo_Vehiculo,
    y = Cantidad,
    fill = Gravedad_graf
  )
) +
  geom_col(width = 0.72) +
  geom_text(
    data = totales_vehiculos,
    aes(
      x = Tipo_Vehiculo,
      y = Total,
      label = scales::number(
        Total,
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 3.5,
    family = "Arial"
  ) +
  scale_fill_manual(
    values = colores_severidad,
    breaks = c(
      "Solo daños",
      "Con heridos",
      "Con muertos"
    )
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ".",
      decimal.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.07)
    )
  ) +
  labs(
    x = NULL,
    y = "Cantidad de accidentes",
    fill = "Gravedad del accidente"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    text = element_text(
      family = "Arial"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 15, 10, 10)
  )

guardar_grafico(
  p4,
  "04_Frecuencia_Accidentes_por_Tipo_de_Vehiculo_y_Gravedad",
  ancho = 10,
  alto = 6
)

# ============================================================
# 10. FRECUENCIA SEGUN GRAVEDAD Y AÑO
# ============================================================

datos <- df %>%
  filter(
    !is.na(Anio_graf),
    !is.na(Gravedad_graf)
  ) %>%
  count(
    Anio_graf,
    Gravedad_graf,
    name = "n"
  )

p5 <- ggplot(
  datos,
  aes(
    x = Anio_graf,
    y = n,
    fill = Gravedad_graf
  )
) +
  geom_col(width = 0.78) +
  scale_fill_manual(
    values = colores_severidad,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = "."
    ),
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
  labs(
    title = "Frecuencia de accidentes según gravedad y año",
    x = "Año",
    y = "Número de accidentes",
    fill = "Gravedad"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      family = "Arial"
    ),
    text = element_text(
      family = "Arial"
    )
  )

guardar_grafico(
  p5,
  "05_Frecuencia_Accidentes_por_Gravedad_y_Anio"
)

# ============================================================
# 11. DISTRIBUCION PORCENTUAL SEGUN FRANJA HORARIA
# ============================================================

datos <- df %>%
  filter(
    !is.na(Franja_graf),
    !is.na(Gravedad_graf)
  ) %>%
  count(
    Franja_graf,
    Gravedad_graf,
    name = "n"
  )

p6 <- ggplot(
  datos,
  aes(
    x = Franja_graf,
    y = n,
    fill = Gravedad_graf
  )
) +
  geom_col(
    position = "fill",
    width = 0.78
  ) +
  scale_fill_manual(
    values = colores_severidad,
    breaks = c(
      "Solo daños",
      "Con heridos",
      "Con muertos"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(
      accuracy = 1
    )
  ) +
  labs(
    title = "Distribución porcentual de la gravedad según franja horaria",
    x = "Franja horaria",
    y = "Porcentaje de accidentes",
    fill = "Gravedad"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      family = "Arial"
    ),
    text = element_text(
      family = "Arial"
    )
  )

guardar_grafico(
  p6,
  "06_Distribucion_Porcentual_Gravedad_por_Franja_Horaria"
)

# ============================================================
# 12. DISTRIBUCION PORCENTUAL SEGUN CLASE
# ============================================================

datos <- df %>%
  filter(
    !is.na(Clase_graf),
    !is.na(Gravedad_graf)
  ) %>%
  count(
    Clase_graf,
    Gravedad_graf,
    name = "n"
  )

# Orden especifico de la grafica enviada:
# Solo daños -> Con muertos -> Con heridos
datos$Gravedad_graf <- factor(
  datos$Gravedad_graf,
  levels = c(
    "Solo daños",
    "Con muertos",
    "Con heridos"
  )
)

p7 <- ggplot(
  datos,
  aes(
    x = Clase_graf,
    y = n,
    fill = Gravedad_graf
  )
) +
  geom_col(
    position = "fill",
    width = 0.78
  ) +
  coord_flip() +
  scale_fill_manual(
    values = colores_severidad,
    breaks = c(
      "Solo daños",
      "Con muertos",
      "Con heridos"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(
      accuracy = 1
    )
  ) +
  labs(
    title = "Distribución porcentual de la gravedad según clase de accidente",
    x = "Clase de accidente",
    y = "Porcentaje de accidentes",
    fill = "Gravedad"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      family = "Arial"
    ),
    text = element_text(
      family = "Arial"
    )
  )

guardar_grafico(
  p7,
  "07_Distribucion_Porcentual_Gravedad_por_Clase",
  ancho = 11,
  alto = 8
)

# ============================================================
# 13. DISTRIBUCION PORCENTUAL SEGUN NUMERO DE VEHICULOS
# ============================================================

datos <- df %>%
  filter(
    !is.na(Total_vehiculos_graf),
    !is.na(Gravedad_graf)
  ) %>%
  mutate(
    Numero_vehiculos = round(
      Total_vehiculos_graf
    )
  ) %>%
  filter(
    Numero_vehiculos >= 0
  ) %>%
  count(
    Numero_vehiculos,
    Gravedad_graf,
    name = "n"
  )

p8 <- ggplot(
  datos,
  aes(
    x = factor(Numero_vehiculos),
    y = n,
    fill = Gravedad_graf
  )
) +
  geom_col(
    position = "fill",
    width = 0.78
  ) +
  scale_fill_manual(
    values = colores_severidad
  ) +
  scale_y_continuous(
    labels = percent_format(
      accuracy = 1
    )
  ) +
  labs(
    title = "Distribución porcentual de la gravedad según número de vehículos",
    x = "Número de vehículos involucrados",
    y = "Porcentaje de accidentes",
    fill = "Gravedad"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      family = "Arial"
    ),
    text = element_text(
      family = "Arial"
    )
  )

guardar_grafico(
  p8,
  "08_Distribucion_Porcentual_Gravedad_por_Numero_de_Vehiculos"
)

# ============================================================
# 14. DISTRIBUCION PORCENTUAL SEGUN DIA DE LA SEMANA
# ============================================================

datos <- df %>%
  filter(
    !is.na(Dia_graf),
    !is.na(Gravedad_graf)
  ) %>%
  count(
    Dia_graf,
    Gravedad_graf,
    name = "n"
  )

p9 <- ggplot(
  datos,
  aes(
    x = Dia_graf,
    y = n,
    fill = Gravedad_graf
  )
) +
  geom_col(
    position = "fill",
    width = 0.78
  ) +
  scale_fill_manual(
    values = colores_severidad
  ) +
  scale_y_continuous(
    labels = percent_format(
      accuracy = 1
    )
  ) +
  labs(
    title = "Distribución porcentual de la gravedad según día de la semana",
    x = "Día de la semana",
    y = "Porcentaje de accidentes",
    fill = "Gravedad"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      family = "Arial"
    ),
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    text = element_text(
      family = "Arial"
    )
  )

guardar_grafico(
  p9,
  "09_Distribucion_Porcentual_Gravedad_por_Dia_Semana"
)

# ============================================================
# 15. TABLA DE DISTRIBUCION DE LA GRAVEDAD POR MES
# ============================================================

tabla_severidad_mes <- df %>%
  filter(
    !is.na(Mes_graf),
    !is.na(Gravedad_graf)
  ) %>%
  count(
    Mes_graf,
    Gravedad_graf,
    name = "Accidentes"
  ) %>%
  pivot_wider(
    names_from = Gravedad_graf,
    values_from = Accidentes,
    values_fill = 0
  )

for (col in c(
  "Solo daños",
  "Con heridos",
  "Con muertos"
)) {
  if (!col %in% names(tabla_severidad_mes)) {
    tabla_severidad_mes[[col]] <- 0
  }
}

tabla_severidad_mes <- tabla_severidad_mes %>%
  mutate(
    Total_mes =
      `Solo daños` +
      `Con heridos` +
      `Con muertos`,
    
    Porcentaje_Solo_danos =
      `Solo daños` /
      Total_mes * 100,
    
    Porcentaje_Con_heridos =
      `Con heridos` /
      Total_mes * 100,
    
    Porcentaje_Con_muertos =
      `Con muertos` /
      Total_mes * 100
  )

total_accidentes_periodo <- sum(
  tabla_severidad_mes$Total_mes,
  na.rm = TRUE
)

tabla_severidad_mes <- tabla_severidad_mes %>%
  mutate(
    Porcentaje_total_mes =
      Total_mes /
      total_accidentes_periodo * 100
  ) %>%
  arrange(Mes_graf) %>%
  select(
    Mes_graf,
    Total_mes,
    `Solo daños`,
    Porcentaje_Solo_danos,
    `Con heridos`,
    Porcentaje_Con_heridos,
    `Con muertos`,
    Porcentaje_Con_muertos,
    Porcentaje_total_mes
  )

cat(
  "\n====================================================\n",
  "DISTRIBUCION DE LA GRAVEDAD POR MES\n",
  "====================================================\n"
)

print(tabla_severidad_mes)

write.csv(
  tabla_severidad_mes,
  file = file.path(
    carpeta_salida,
    "13_Tabla_Distribucion_Gravedad_por_Mes.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
# 16. MAPA DE CALOR HORA X DIA
# ============================================================

if (is.na(col_hora)) {
  
  warning(
    "No se encontro Hora. Se omite el mapa de calor."
  )
  
} else {
  
  datos_calor <- df %>%
    mutate(
      Hora_num = extraer_hora(
        .data[[col_hora]]
      ),
      Dia = factor(
        Dia_graf,
        levels = dias_semana
      )
    ) %>%
    filter(
      !is.na(Hora_num),
      !is.na(Dia)
    ) %>%
    count(
      Dia,
      Hora_num,
      name = "Accidentes"
    )
  
  p10 <- ggplot(
    datos_calor,
    aes(
      x = Hora_num,
      y = Dia,
      fill = Accidentes
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 0.25
    ) +
    scale_fill_gradientn(
      colours = colores_calidos,
      labels = label_number(
        big.mark = "."
      )
    ) +
    scale_x_continuous(
      breaks = 0:23,
      limits = c(
        -0.5,
        23.5
      )
    ) +
    labs(
      title = "Mapa de calor de accidentes según hora y día de la semana",
      x = "Hora del día",
      y = "Día de la semana",
      fill = "Accidentes"
    ) +
    theme_minimal(
      base_size = 12,
      base_family = "Arial"
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        family = "Arial"
      ),
      panel.grid = element_blank(),
      text = element_text(
        family = "Arial"
      )
    )
  
  guardar_grafico(
    p10,
    "10_Mapa_Calor_Hora_Dia_Semana",
    ancho = 12,
    alto = 6
  )
}

# ============================================================
# 17. EDAD PROMEDIO DE LAS VICTIMAS SEGUN GRAVEDAD
# ============================================================

if (is.na(col_edad)) {
  
  warning(
    "No se encontro la columna de edad promedio."
  )
  
} else {
  
  df$Edad_promedio_graf <- suppressWarnings(
    as.numeric(
      as.character(
        df[[col_edad]]
      )
    )
  )
  
  datos_edad <- df %>%
    filter(
      !is.na(Gravedad_graf),
      !is.na(Edad_promedio_graf),
      Edad_promedio_graf > 0,
      Edad_promedio_graf <= 110
    )
  
  p11 <- ggplot(
    datos_edad,
    aes(
      x = Gravedad_graf,
      y = Edad_promedio_graf,
      fill = Gravedad_graf
    )
  ) +
    geom_boxplot(
      width = 0.62,
      outlier.alpha = 0.35,
      outlier.size = 1.2
    ) +
    scale_fill_manual(
      values = colores_severidad,
      drop = FALSE
    ) +
    labs(
      title = "Edad promedio de las víctimas según gravedad del accidente",
      x = "Gravedad del accidente",
      y = "Edad promedio (años)"
    ) +
    theme_minimal(
      base_size = 12,
      base_family = "Arial"
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        family = "Arial"
      ),
      legend.position = "none",
      text = element_text(
        family = "Arial"
      )
    )
  
  guardar_grafico(
    p11,
    "11_Edad_Promedio_Victimas_por_Gravedad"
  )
}

# ============================================================
# 18. LETALIDAD RELATIVA POR ACTOR VIAL
# BASADA EN VICTIMAS, NO EN ACCIDENTES
# ============================================================

actores_letalidad <- data.frame(
  Actor = c(
    "Peatón",
    "Ciclista",
    "Motociclista",
    "Acompañante",
    "Conductor",
    "Pasajero"
  ),
  Herido = c(
    "#_Peaton_Herido",
    "#_Ciclista_Herido",
    "#_Motociclista_Herido",
    "#_Acompañante_Herido",
    "#_Conductor_Herido",
    "#_Pasajero_Herido"
  ),
  Muerto = c(
    "#_Peaton_Muerto",
    "#_Ciclista_Muerto",
    "#_Motociclista_Muerto",
    "#_Acompañante_Muerto",
    "#_Conductor_Muerto",
    "#_Pasajero_Muerto"
  ),
  stringsAsFactors = FALSE
)

actores_letalidad$Col_Herido <- vapply(
  actores_letalidad$Herido,
  buscar_columna,
  character(1)
)

actores_letalidad$Col_Muerto <- vapply(
  actores_letalidad$Muerto,
  buscar_columna,
  character(1)
)

if (
  any(is.na(actores_letalidad$Col_Herido)) |
  any(is.na(actores_letalidad$Col_Muerto))
) {
  stop(
    paste0(
      "No se encontraron todas las columnas necesarias ",
      "para calcular la letalidad relativa."
    ),
    call. = FALSE
  )
}

resultados_letalidad <- lapply(
  seq_len(nrow(actores_letalidad)),
  function(i) {
    
    heridos <- suppressWarnings(
      as.numeric(
        as.character(
          df[[actores_letalidad$Col_Herido[i]]]
        )
      )
    )
    
    muertos <- suppressWarnings(
      as.numeric(
        as.character(
          df[[actores_letalidad$Col_Muerto[i]]]
        )
      )
    )
    
    heridos[is.na(heridos)] <- 0
    muertos[is.na(muertos)] <- 0
    
    total_victimas <- sum(
      heridos + muertos,
      na.rm = TRUE
    )
    
    total_muertos <- sum(
      muertos,
      na.rm = TRUE
    )
    
    data.frame(
      Actor = actores_letalidad$Actor[i],
      Victimas_heridas = sum(
        heridos,
        na.rm = TRUE
      ),
      Victimas_fallecidas = total_muertos,
      Total_victimas = total_victimas,
      Letalidad_relativa =
        ifelse(
          total_victimas > 0,
          total_muertos / total_victimas,
          NA_real_
        )
    )
  }
) %>%
  bind_rows() %>%
  filter(
    !is.na(Letalidad_relativa)
  ) %>%
  arrange(
    Letalidad_relativa
  )

p12 <- ggplot(
  resultados_letalidad,
  aes(
    x = reorder(
      Actor,
      Letalidad_relativa
    ),
    y = Letalidad_relativa
  )
) +
  geom_col(
    fill = color_fallecidos,
    width = 0.72
  ) +
  geom_text(
    aes(
      label = percent(
        Letalidad_relativa,
        accuracy = 0.1,
        decimal.mark = ","
      )
    ),
    hjust = -0.12,
    size = 3.7,
    family = "Arial"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = expansion(
      mult = c(0, 0.18)
    )
  ) +
  labs(
    title = "Letalidad relativa por actor vial",
    x = "Actor vial",
    y = "Letalidad relativa (%)"
  ) +
  theme_minimal(
    base_size = 12,
    base_family = "Arial"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      family = "Arial"
    ),
    text = element_text(
      family = "Arial"
    )
  )

guardar_grafico(
  p12,
  "12_Letalidad_Relativa_por_Actor_Vial",
  ancho = 11,
  alto = 7
)

write.csv(
  resultados_letalidad,
  file = file.path(
    carpeta_salida,
    "14_Tabla_Letalidad_Relativa_por_Actor_Vial.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
# 19. VERIFICACION FINAL DE ARCHIVOS
# ============================================================

cat("\n")
cat("====================================================\n")
cat("ANALISIS FINALIZADO\n")
cat("====================================================\n")

cat(
  "Carpeta de resultados:\n",
  carpeta_salida,
  "\n\n"
)

cat(
  "Graficas PNG guardadas:",
  length(graficos_guardados),
  "\n\n"
)

for (archivo in graficos_guardados) {
  cat(
    "✓ ",
    basename(archivo),
    "\n",
    sep = ""
  )
}

cat("\nArchivos CSV guardados:\n")

archivos_csv <- list.files(
  carpeta_salida,
  pattern = "\\.csv$",
  full.names = FALSE
)

for (archivo in archivos_csv) {
  cat(
    "✓ ",
    archivo,
    "\n",
    sep = ""
  )
}

cat("\n====================================================\n")
