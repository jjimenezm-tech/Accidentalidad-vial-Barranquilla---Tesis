# =============================================================================
# UNIFICACION DE BASES DE DATOS DE ACCIDENTALIDAD - BARRANQUILLA
#   Paso 1: Unificar DETALLE DEL ACCIDENTE + VEHICULOS sobre el FORMATO
#   Paso 2: Integrar DETALLE_VICTIMAS a esa base ya unificada
#   Paso 3: Estandarizar la columna de direcciones (Sitio_Exacto_Accidente)
# =============================================================================

# Instalar paquetes faltantes si es necesario
paquetes <- c("readxl", "openxlsx", "dplyr", "stringr", "purrr", "lubridate")
instalados <- paquetes %in% rownames(installed.packages())
if (any(!instalados)) install.packages(paquetes[!instalados])

suppressMessages({
  library(readxl)
  library(openxlsx)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(lubridate)
})

# -----------------------------------------------------------------------------
# 0. RUTAS DE ARCHIVOS DE ENTRADA/SALIDA -- AJUSTAR SI ES NECESARIO
# -----------------------------------------------------------------------------
RUTA_DETALLE   <- "Accidentalidad_en_Barranquilla_DETALLE_DEL_ACCIDENTE_20260615.xlsx"
RUTA_VEHICULOS <- "Accidentalidad_Barranquilla_VEHICULOS_20260615.xlsx"
RUTA_VICTIMAS  <- "Accidentalidad_en_Barranquilla_DETALLE_VICTIMAS_20260615.xlsx"
RUTA_FORMATO   <- "FORMATO_TABLA_PARA_BASE_DE_DATOS.xlsx"
RUTA_SALIDA    <- "Base_de_Datos_Unificada_Accidentalidad_Barranquilla.xlsx"

# Validacion de pre-requisitos: existencia de archivos de entrada
archivos_requeridos <- c(RUTA_DETALLE, RUTA_VEHICULOS, RUTA_VICTIMAS, RUTA_FORMATO)
faltantes <- archivos_requeridos[!file.exists(archivos_requeridos)]

if (length(faltantes) > 0) {
  stop(paste0(
    "\n[ERROR CRITICO] No se encontraron los siguientes archivos requeridos en el directorio de trabajo:\n - ",
    paste(faltantes, collapse = "\n - "),
    "\n\nPor favor asegurese de colocar estos archivos en la carpeta del proyecto antes de ejecutar el script."
  ))
}

# Funcion para parsear fechas de Excel de forma segura (soporta texto y numero de serie)
parse_fecha_excel <- function(x) {
  num_val <- suppressWarnings(as.numeric(x))
  if (!is.na(num_val[1])) {
    return(as.Date(num_val, origin = "1899-12-30"))
  } else {
    return(as.Date(parse_date_time(x, orders = c("ymd", "dmy", "mdy"))))
  }
}

# =============================================================================
# PASO 1A. LEER Y PREPARAR DETALLE DEL ACCIDENTE
# =============================================================================
# Cada fila de esta base es UN accidente. Define el numero total de accidentes.
detalle_raw <- read_excel(RUTA_DETALLE, sheet = "Data", col_types = "text")

detalle <- detalle_raw %>%
  transmute(
    fecha        = parse_fecha_excel(FECHA_ACCIDENTE),
    hora_txt     = HORA_ACCIDENTE,                      # "hh:mm:ss:am/pm"
    gravedad     = GRAVEDAD_ACCIDENTE,
    clase        = CLASE_ACCIDENTE,
    sitio        = SITIO_EXACTO_ACCIDENTE,
    herido_cnt   = as.integer(`CANT_HERIDOS_EN _SITIO_ACCIDENTE`),
    muerto_cnt   = as.integer(`CANT_MUERTOS_EN _SITIO_ACCIDENTE`),
    anio         = as.integer(AÑO_ACCIDENTE),
    mes          = MES_ACCIDENTE,
    dia_semana   = DIA_ACCIDENTE
  ) %>%
  mutate(
    dia_mes  = as.integer(format(fecha, "%d")),
    key      = paste(fecha, sitio, clase, sep = "||"),   # llave de cruce
    det_idx  = row_number()                              # orden original (1..N)
  )

cat("Total de accidentes (DETALLE):", nrow(detalle), "\n")

# Parseo de la hora "hh:mm:ss:am/pm" -> string "HH:MM:SS" en 24 horas
parse_hora <- function(h) {
  if (is.na(h)) return(NA_character_)
  partes <- str_split(h, ":")[[1]]
  hh <- as.integer(partes[1]); mm <- partes[2]; ss <- partes[3]
  ampm <- tolower(partes[4])
  if (ampm == "pm" && hh != 12) hh <- hh + 12
  if (ampm == "am" && hh == 12) hh <- 0
  sprintf("%02d:%s:%s", hh, mm, ss)
}
detalle$hora_24 <- vapply(detalle$hora_txt, parse_hora, character(1))

# =============================================================================
# PASO 1B. LEER Y PREPARAR VEHICULOS
# =============================================================================
vehiculos_raw <- read_excel(RUTA_VEHICULOS, sheet = "Data", col_types = "text")

vehiculos <- vehiculos_raw %>%
  transmute(
    fecha = parse_fecha_excel(Fecha_Accidente),
    sitio = `DIRECCION ACCIDENTE`,
    clase = `CLASE ACCIDENTE`,
    tipo  = CLASE_VEHICULO_ACCIDENTADO,
    qty   = as.integer(CANTIDAD_ACCIDENTES)
  ) %>%
  mutate(
    key     = paste(fecha, sitio, clase, sep = "||"),
    veh_idx = row_number()
  )

cat("Total de filas VEHICULOS:", nrow(vehiculos), "\n")

# =============================================================================
# PASO 1C. ALGORITMO DE REPARTO DE VEHICULOS ENTRE ACCIDENTES DE LA MISMA LLAVE
# =============================================================================
# Reglas (definidas por el usuario y validadas contra sus 3 ejemplos):
#  - Si una llave (fecha, sitio, clase) tiene UN solo accidente en DETALLE,
#    TODOS los vehiculos de esa llave pertenecen a ese accidente.
#  - Si tiene N > 1 accidentes:
#      a) Toda fila de VEHICULOS con CANTIDAD_ACCIDENTES > 1 es un grupo
#         indivisible: esos vehiculos van completos a un mismo accidente.
#      b) Las filas restantes (cantidad == 1) se reparten en partes iguales
#         (o lo mas equilibrado posible) entre los accidentes restantes,
#         respetando su orden original en el archivo de VEHICULOS.
#      c) Los grupos resultantes se ordenan segun la posicion en la que
#         aparece por primera vez cada grupo en VEHICULOS, y se asignan en
#         ese orden a los N accidentes de DETALLE (tambien en su orden
#         original).

det_por_llave <- split(detalle$det_idx, detalle$key)
veh_por_llave <- split(vehiculos$veh_idx, vehiculos$key)

# resultado: lista nombrada por det_idx -> named integer vector (tipo -> qty)
veh_asignado <- vector("list", nrow(detalle))

for (k in names(det_por_llave)) {
  det_idxs <- det_por_llave[[k]]
  N <- length(det_idxs)
  v_idxs <- veh_por_llave[[k]]
  if (is.null(v_idxs)) v_idxs <- integer(0)
  
  filas <- vehiculos[vehiculos$veh_idx %in% v_idxs, ]
  filas <- filas[order(match(filas$veh_idx, v_idxs)), ]  # preservar orden original
  
  if (N == 1) {
    # todos los vehiculos de la llave van a este unico accidente
    if (nrow(filas) > 0) {
      cont <- tapply(filas$qty, filas$tipo, sum)
    } else {
      cont <- integer(0)
    }
    veh_asignado[[det_idxs[1]]] <- cont
    next
  }
  
  # N > 1: separar filas "grandes" (indivisibles) de filas "sueltas"
  filas$pos <- seq_len(nrow(filas))
  grandes  <- filas[filas$qty > 1, ]
  sueltas  <- filas[filas$qty == 1, ]
  
  grupos <- list()  # cada elemento: list(first_idx=, counter=named vector)
  
  # a) cada fila grande = su propio grupo/accidente
  for (i in seq_len(nrow(grandes))) {
    r <- grandes[i, ]
    cnt <- setNames(r$qty, r$tipo)
    grupos[[length(grupos) + 1]] <- list(first_idx = r$pos, counter = cnt)
  }
  
  cupos_restantes <- N - nrow(grandes)
  
  # b) repartir filas sueltas en partes iguales entre los cupos restantes
  if (cupos_restantes > 0 && nrow(sueltas) > 0) {
    S <- nrow(sueltas)
    base  <- S %/% cupos_restantes
    extra <- S %% cupos_restantes
    pos <- 1
    for (s in seq_len(cupos_restantes)) {
      tam <- base + ifelse(s <= extra, 1, 0)
      if (tam > 0) {
        chunk <- sueltas[pos:(pos + tam - 1), , drop = FALSE]
        pos <- pos + tam
        cnt <- tapply(chunk$qty, chunk$tipo, sum)
        grupos[[length(grupos) + 1]] <- list(first_idx = chunk$pos[1], counter = cnt)
      } else {
        grupos[[length(grupos) + 1]] <- list(first_idx = Inf, counter = integer(0))
      }
    }
  } else if (cupos_restantes > 0) {
    for (s in seq_len(cupos_restantes)) {
      grupos[[length(grupos) + 1]] <- list(first_idx = Inf, counter = integer(0))
    }
  }
  
  # c) ordenar grupos por orden de primera aparicion y asignar 1:1 a los
  #    accidentes de la llave (tambien en su orden original)
  orden <- order(vapply(grupos, function(g) g$first_idx, numeric(1)))
  grupos <- grupos[orden]
  
  # ajuste defensivo de longitud (no deberia ocurrir con estos datos)
  if (length(grupos) < N) {
    for (i in (length(grupos) + 1):N) grupos[[i]] <- list(counter = integer(0))
  }
  grupos <- grupos[1:N]
  
  for (i in seq_len(N)) {
    veh_asignado[[det_idxs[i]]] <- grupos[[i]]$counter
  }
}

cat("Reparto de vehiculos completado.\n")

# =============================================================================
# PASO 2A. LEER Y PREPARAR VICTIMAS
# =============================================================================
victimas_raw <- read_excel(RUTA_VICTIMAS, sheet = "Data", col_types = "text")

victimas <- victimas_raw %>%
  transmute(
    fecha = parse_fecha_excel(Fecha_Accidente),
    sitio = `DIRECCION ACCIDENTE`,
    cond  = CONDICION_VICTIMA,
    grav  = GRAVEDAD_ACCIDENTE,   # "herido" / "muerto"
    clase = CLASE_ACCIDENTE,
    sexo  = SEXO_VICTIMA,
    edad  = as.numeric(EDAD_VICTIMA),
    cant  = as.integer(CANTIDAD_VICTIMAS)
  ) %>%
  mutate(
    key     = paste(fecha, sitio, clase, sep = "||"),
    vic_idx = row_number()
  )

cat("Total de filas VICTIMAS:", nrow(victimas), "\n")

# =============================================================================
# PASO 2B. REPARTO DE VICTIMAS ENTRE ACCIDENTES DE LA MISMA LLAVE
# =============================================================================
# DETALLE trae, por cada accidente individual, la cantidad EXACTA de heridos
# y de muertos (herido_cnt / muerto_cnt). Esos valores se usan como "cupos"
# objetivo por accidente y por gravedad para repartir con precision las filas
# de VICTIMAS cuando una llave agrupa mas de un accidente.
#
# Reglas de reparto (por gravedad, heridos y muertos por separado):
#  a) Un accidente "Solo daños" nunca recibe victimas (su cupo es 0 en ambas
#     gravedades, asi que naturalmente queda excluido).
#  b) Correspondencia actor-vehiculo: "Motociclista" se asigna preferente-
#     mente al accidente (de la misma llave) que tenga un vehiculo de la
#     familia motociclo; "Ciclista" al que tenga bicicleta/ciclotaxi.
#  c) Filas con CANTIDAD_VICTIMAS > 1 se tratan como bloque indivisible.
#  d) El resto se reparte en el orden original del archivo, llenando cada
#     accidente hasta agotar exactamente su cupo de heridos/muertos.

MOTO_FAMILIA <- c("MOTOCICLETA", "MOTOCARRO", "CICLOMOTOR", "MOTOCICLO",
                  "MOTOTRICICLO", "TRICIMOTO", "CUATRIMOTO")
BICI_FAMILIA <- c("BICICLETA", "CICLO TAXI")

tiene_familia <- function(det_idx, familia) {
  cnt <- veh_asignado[[det_idx]]
  if (is.null(cnt) || length(cnt) == 0) return(FALSE)
  any(names(cnt) %in% familia)
}

# asignar_gravedad: reparte un vector de filas (misma gravedad) entre los
# accidentes de una llave, respetando los cupos y la afinidad actor-vehiculo.
asignar_gravedad <- function(det_idxs, filas_sev) {
  N <- length(det_idxs)
  objetivo <- if (filas_sev$grav[1] == "herido") {
    detalle$herido_cnt[det_idxs]
  } else {
    detalle$muerto_cnt[det_idxs]
  }
  restante <- objetivo
  resultado <- vector("list", N)
  for (i in seq_len(N)) resultado[[i]] <- list()
  asignada <- rep(FALSE, nrow(filas_sev))
  
  # Fase 1: afinidad actor-vehiculo (Motociclista / Ciclista)
  for (i in seq_len(nrow(filas_sev))) {
    fila <- filas_sev[i, ]
    familia <- NULL
    if (fila$cond == "Motociclista") familia <- MOTO_FAMILIA
    if (fila$cond == "Ciclista")     familia <- BICI_FAMILIA
    if (is.null(familia)) next
    
    candidatos <- which(restante > 0 & vapply(det_idxs, tiene_familia, logical(1), familia = familia))
    if (length(candidatos) == 0) next
    
    elegido <- candidatos[which(restante[candidatos] >= fila$cant)[1]]
    if (is.na(elegido)) elegido <- candidatos[which.max(restante[candidatos])]
    
    resultado[[elegido]][[length(resultado[[elegido]]) + 1]] <- fila
    restante[elegido] <- restante[elegido] - fila$cant
    asignada[i] <- TRUE
  }
  
  # Fase 2: resto, primer cupo disponible en orden original
  for (i in seq_len(nrow(filas_sev))) {
    if (asignada[i]) next
    fila <- filas_sev[i, ]
    elegido <- which(restante >= fila$cant)[1]
    if (is.na(elegido)) elegido <- which.max(restante)
    
    resultado[[elegido]][[length(resultado[[elegido]]) + 1]] <- fila
    restante[elegido] <- restante[elegido] - fila$cant
  }
  
  list(resultado = resultado, restante = restante)
}

vic_por_llave <- split(victimas$vic_idx, victimas$key)
vic_asignado <- vector("list", nrow(detalle))  # det_idx -> lista de filas de victima

for (k in names(vic_por_llave)) {
  det_idxs <- det_por_llave[[k]]
  v_idxs <- vic_por_llave[[k]]
  filas <- victimas[victimas$vic_idx %in% v_idxs, ]
  filas <- filas[order(match(filas$vic_idx, v_idxs)), ]
  
  N <- length(det_idxs)
  if (N == 1) {
    vic_asignado[[det_idxs[1]]] <- split(filas, seq_len(nrow(filas)))
    next
  }
  
  heridos <- filas[filas$grav == "herido", ]
  muertos <- filas[filas$grav == "muerto", ]
  
  for (i in seq_len(N)) vic_asignado[[det_idxs[i]]] <- list()
  
  if (nrow(heridos) > 0) {
    out <- asignar_gravedad(det_idxs, heridos)
    for (i in seq_len(N)) {
      vic_asignado[[det_idxs[i]]] <- c(vic_asignado[[det_idxs[i]]], out$resultado[[i]])
    }
  }
  if (nrow(muertos) > 0) {
    out <- asignar_gravedad(det_idxs, muertos)
    for (i in seq_len(N)) {
      vic_asignado[[det_idxs[i]]] <- c(vic_asignado[[det_idxs[i]]], out$resultado[[i]])
    }
  }
}

cat("Reparto de victimas completado.\n")

# =============================================================================
# PASO 3. CONSTRUCCION DE LA TABLA FINAL SEGUN EL FORMATO
# =============================================================================
formato_wb <- loadWorkbook(RUTA_FORMATO)
encabezados <- names(read_excel(RUTA_FORMATO, sheet = "Hoja1", n_max = 0))

# --- columnas de tipo de vehiculo (las que empiezan con "#_" y no son de
#     victimas ni el total) ---
es_col_victima <- function(h) grepl("_Muerto$|_Herido$", h) || h == "#_Total_Victimas"
cols_vehiculo <- encabezados[str_starts(encabezados, "#_") & !vapply(encabezados, es_col_victima, logical(1))]
tipos_vehiculo <- str_remove(cols_vehiculo, "^#_")

normaliza <- function(x) {
  x <- toupper(trimws(x))
  chartr("ÁÉÍÓÚÑ", "AEIOUN", x)
}
tipos_vehiculo_norm <- normaliza(tipos_vehiculo)
names(cols_vehiculo) <- tipos_vehiculo_norm  # lookup: tipo normalizado -> nombre de columna

# --- columnas de victima por actor vial (16: 8 actores x Muerto/Herido) ---
cols_actor_muerto <- encabezados[str_ends(encabezados, "_Muerto")]
cols_actor_herido <- encabezados[str_ends(encabezados, "_Herido")]
actor_muerto_norm <- normaliza(str_remove(str_remove(cols_actor_muerto, "^#_"), "_Muerto$"))
actor_herido_norm <- normaliza(str_remove(str_remove(cols_actor_herido, "^#_"), "_Herido$"))
names(cols_actor_muerto) <- actor_muerto_norm
names(cols_actor_herido) <- actor_herido_norm

# lookup seguro sobre un vector nombrado
lookup <- function(vec, key) {
  i <- match(key, names(vec))
  if (is.na(i)) NULL else vec[[i]]
}

construir_fila <- function(idx) {
  d <- detalle[idx, ]
  fila <- list(
    Id_Accidente = sprintf("ACC-%05d", idx),
    Año = d$anio,
    Mes = d$mes,
    Día = d$dia_mes,
    Día_Semana = d$dia_semana,
    Hora = d$hora_24,
    Sitio_Exacto_Accidente = d$sitio,
    Clase_Accidente = d$clase,
    Gravedad_Accidente = d$gravedad
  )
  
  # --- vehiculos: inicializar en 0 y llenar segun lo asignado ---
  cnt_veh <- veh_asignado[[idx]]
  for (col in cols_vehiculo) fila[[col]] <- 0L
  if (!is.null(cnt_veh) && length(cnt_veh) > 0) {
    for (tipo in names(cnt_veh)) {
      tipo_n <- normaliza(tipo)
      col <- lookup(cols_vehiculo, tipo_n)
      if (is.null(col)) stop("Tipo de vehiculo sin columna en formato: ", tipo)
      fila[[col]] <- as.integer(cnt_veh[[tipo]])
    }
  }
  fila[["Número_total_vehiculos"]] <- sum(unlist(fila[cols_vehiculo]))
  
  # --- victimas: inicializar en 0 y llenar segun lo asignado ---
  for (col in cols_actor_muerto) fila[[col]] <- 0L
  for (col in cols_actor_herido) fila[[col]] <- 0L
  
  filas_vic <- vic_asignado[[idx]]
  edad_pond <- 0; total_vic <- 0
  if (!is.null(filas_vic) && length(filas_vic) > 0) {
    for (v in filas_vic) {
      cond_n <- normaliza(v$cond)
      if (v$grav == "muerto") {
        col <- lookup(cols_actor_muerto, cond_n)
      } else {
        col <- lookup(cols_actor_herido, cond_n)
      }
      if (is.null(col)) stop("Condicion de victima sin columna: ", v$cond, " / ", v$grav)
      fila[[col]] <- fila[[col]] + as.integer(v$cant)
      if (!is.na(v$edad)) edad_pond <- edad_pond + v$edad * v$cant
      total_vic <- total_vic + v$cant
    }
  }
  
  fila[["Num_Heridos"]]    <- sum(unlist(fila[cols_actor_herido]))
  fila[["Num_Fallecidos"]] <- sum(unlist(fila[cols_actor_muerto]))
  fila[["#_Total_Victimas"]] <- fila[["Num_Heridos"]] + fila[["Num_Fallecidos"]]
  fila[["Edad_Promedio_De_Las_Victimas"]] <- if (total_vic > 0) round(edad_pond / total_vic, 1) else NA_real_
  
  fila
}

cat("Construyendo tabla final (", nrow(detalle), " filas)...\n", sep = "")
filas_finales <- map(seq_len(nrow(detalle)), construir_fila)
tabla_final <- bind_rows(filas_finales)
tabla_final <- tabla_final[, encabezados]  # mismo orden de columnas que el formato

cat("Tabla final construida:", nrow(tabla_final), "filas x", ncol(tabla_final), "columnas\n")

# =============================================================================
# VALIDACIONES
# =============================================================================
cat("\n--- VALIDACIONES ---\n")
cat("Suma total de vehiculos:", sum(tabla_final$Número_total_vehiculos), "(esperado: 55720)\n")
cat("Suma total de heridos:", sum(tabla_final$Num_Heridos), "(esperado: 18756)\n")
cat("Suma total de muertos:", sum(tabla_final$Num_Fallecidos), "(esperado: 339)\n")
sd_con_victimas <- sum(tabla_final$Gravedad_Accidente == "Solo daños" &
                         (tabla_final$Num_Heridos + tabla_final$Num_Fallecidos) > 0)
cat("Accidentes 'Solo daños' con alguna victima asignada:", sd_con_victimas, "(esperado: 0)\n")

# =============================================================================
# PASO 4. ESTANDARIZACION DE DIRECCIONES (PRIMERA PARTE)
# =============================================================================
estandarizar_direccion <- function(addr) {
  if (is.na(addr)) return(addr)
  s <- toupper(trimws(addr))
  if (s == "" || s == "NA") return(addr)
  s <- str_squish(s)
  
  s <- str_replace_all(s, "\\bN[°ºO]\\.?\\s*", "# ")
  s <- str_replace_all(s, "\\b(CON|ESQUINA|ENTRE)\\b", "#")
  s <- str_replace_all(s, "(?<=[0-9])\\s+Y\\s+(?=[0-9])", " # ")
  
  expandir <- function(patron, reemplazo, s) {
    str_replace_all(s, regex(paste0("(?<![A-Z])", patron, "\\.?(?![A-Z])")), reemplazo)
  }
  s <- expandir("CLLE", "CALLE", s)
  s <- expandir("ALLE", "CALLE", s)
  s <- expandir("CL", "CALLE", s)
  s <- expandir("CRA", "CARRERA", s)
  s <- expandir("CR", "CARRERA", s)
  s <- expandir("AV", "AVENIDA", s)
  s <- expandir("TRANSVERSSAL", "TRANSVERSAL", s)
  s <- expandir("TRANSV", "TRANSVERSAL", s)
  s <- expandir("TRANS", "TRANSVERSAL", s)
  
  s <- str_replace_all(s, regex("\\bAVENIDA\\s+CIRCUNVALAR\\b"), "AVENIDA 110")
  s <- str_replace_all(s, regex("\\bCIRCUNVALAR\\b"), "AVENIDA 110")
  
  s <- str_replace_all(s, "\\s*#\\s*", " # ")
  s <- str_replace_all(s, "(#\\s*){2,}", "# ")
  s <- str_squish(s)
  s <- str_replace(s, "^#\\s*", "")
  
  NUM <- "\\d+[A-Z]{0,2}(?:\\s?(?:BIS|SUR|NORTE))?"
  
  pat1 <- regex(paste0("^(CALLE|CARRERA)\\s+(", NUM, ")\\s*#?\\s*(CALLE|CARRERA)\\s+(",
                       NUM, ")((?:\\s*-\\s*", NUM, ")?)(.*)$"))
  m1 <- str_match(s, pat1)
  if (!is.na(m1[1, 1])) {
    t1 <- m1[1, 2]; n1 <- m1[1, 3]; t2 <- m1[1, 4]; n2 <- m1[1, 5]
    extra <- m1[1, 6]; resto <- m1[1, 7]
    complementaria <- c("CALLE" = "CARRERA", "CARRERA" = "CALLE")
    if (t2 == complementaria[[t1]]) {
      s <- str_squish(paste0(t1, " ", n1, " # ", n2, extra, resto))
    } else {
      s <- str_squish(paste0(t1, " ", n1, " # ", t2, " ", n2, extra, resto))
    }
    return(s)
  }
  
  pat2 <- regex(paste0("^(CALLE|CARRERA)\\s+(", NUM, ")\\s+(", NUM, ")(?:\\s*-?\\s*(", NUM, "))?(.*)$"))
  m2 <- str_match(s, pat2)
  if (!is.na(m2[1, 1])) {
    t1 <- m2[1, 2]; n1 <- m2[1, 3]; n2 <- m2[1, 4]; n3 <- m2[1, 5]; resto <- m2[1, 6]
    if (!is.na(n3) && n3 != "") {
      s <- str_squish(paste0(t1, " ", n1, " # ", n2, "-", n3, resto))
    } else {
      s <- str_squish(paste0(t1, " ", n1, " # ", n2, resto))
    }
    return(s)
  }
  
  s
}

cat("\nEstandarizando direcciones...\n")
tabla_final$Sitio_Exacto_Accidente <- vapply(
  tabla_final$Sitio_Exacto_Accidente, estandarizar_direccion, character(1)
)

# =============================================================================
# PASO 5. ESCRIBIR ARCHIVO DE SALIDA
# =============================================================================
wb <- createWorkbook()
addWorksheet(wb, "Hoja1")
writeData(wb, "Hoja1", tabla_final)
saveWorkbook(wb, RUTA_SALIDA, overwrite = TRUE)

cat("\nArchivo final escrito en:", RUTA_SALIDA, "\n")

