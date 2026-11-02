# ============================================================
# ESTANDARIZACION DE DIRECCIONES DE ACCIDENTALIDAD
# ============================================================
# Lee la direccion, genera la direccion estandarizada y las
# observaciones.
# ============================================================
paquetes <- c("tidyverse", "readxl", "openxlsx")
instalados <- paquetes %in% rownames(installed.packages())
if (any(!instalados)) install.packages(paquetes[!instalados])

library(tidyverse)
library(readxl)
library(openxlsx)
library(tidyverse)
library(readxl)
library(openxlsx)

# ------------------------------------------------------------
# 0. PARAMETROS Y VERIFICACION DE PRE-REQUISITOS
# ------------------------------------------------------------
ruta_entrada  <- "Base_de_Datos_Unificada_Accidentalidad_Barranquilla.xlsx"
hoja_entrada  <- "Hoja1"
ruta_salida   <- "Base_de_Datos_Unificada_Direcciones_Estandarizadas.xlsx"

col_id        <- "Id_Accidente"
col_direccion <- "Sitio_Exacto_Accidente"      
col_std       <- "Direccion_Estandarizada"     
col_obs       <- "Observaciones_Direccion"     

# Comprobación de prerrequisito
if (!file.exists(ruta_entrada)) {
  stop(paste(
    "\n[ERROR] No se encontro el archivo de entrada:", ruta_entrada,
    "\nPor favor ejecute primero el script de unificacion de base de datos antes de correr este script."
  ))
}

# ------------------------------------------------------------
# 1. DICCIONARIOS DE NORMALIZACION
# ------------------------------------------------------------
sin_calle   <- c("CL","CLLE","CLL","CALE","CALL","CLK","ALLE","CELL","CALLR")
sin_carrera <- c("CR","CRA","CARERA","CARERRA","CARRER","CARRERS",
                  "CARRERAB","CARRERAN","CRT","CARA")
sin_avenida <- c("AV","AVENIDAD","AVNIDA","AVE")
sin_diag    <- c("DIAG")
sin_transv  <- c("TRANSV","TRANSVERSSAL")

patron_lista <- function(v) paste0("\\b(", paste(v, collapse = "|"), ")\\b")

# tipos "pegados" al numero: CRA60 -> CRA 60 ; CALLE72 -> CALLE 72
tipos_todos <- unique(c(sin_calle, sin_carrera, sin_avenida, sin_diag, sin_transv,
                         "CALLE", "CARRERA", "AVENIDA", "DIAGONAL", "TRANSVERSAL"))
tipos_todos <- tipos_todos[order(-nchar(tipos_todos))]
patron_pegado <- paste0("\\b(", paste(tipos_todos, collapse = "|"), ")(\\d)")

separar_tipo_pegado <- function(x) str_replace_all(x, regex(patron_pegado), "\\1 \\2")

fusionar_sur <- function(x) str_replace_all(x, regex("(\\d+)SUR\\b"), "\\1 SUR")

normalizar_numeral_no <- function(x) str_replace_all(x, regex("N[\u00b0O]\\.?\\s*(?=\\d)"), "# ")

normalizar_tipos_via <- function(x) {
  x <- str_replace_all(x, regex(patron_lista(sin_calle)),   "CALLE")
  x <- str_replace_all(x, regex(patron_lista(sin_carrera)), "CARRERA")
  x <- str_replace_all(x, regex(patron_lista(sin_avenida)), "AVENIDA")
  x <- str_replace_all(x, regex(patron_lista(sin_diag)),    "DIAGONAL")
  x <- str_replace_all(x, regex(patron_lista(sin_transv)),  "TRANSVERSAL")
  x <- str_replace_all(x, regex("\\bKILOMETRO\\b"), "KM")   # se deja como "KM"
  x
}

# typos puntuales detectados en la base
corregir_typos <- function(x) {
  x <- str_replace_all(x, regex("\\bCIRVCUNVALAR\\b"), "CIRCUNVALAR")
  x <- str_replace_all(x, regex("\\bCOORDIALIDAD\\b"), "CORDIALIDAD")
  x <- str_replace_all(x, regex("\\bAVENIDA CIA 40\\b"), "VIA 40")
  x <- str_replace_all(x, regex("\\bAVENIDA 40\\b"), "VIA 40")
  x
}

# vias con nombre propio conocidas en Barranquilla
aplicar_nombres_propios <- function(x) {
  x <- str_replace_all(x, regex("\\bVIA\\s+(CORDIALIDAD|MURILLO|CIRCUNVALAR)\\b"), "\\1")
  x <- str_replace_all(x, regex("(?<!AVENIDA )\\bCIRCUNVALAR\\b"), "AVENIDA CIRCUNVALAR")
  x <- str_replace_all(x, regex("\\bCALLE 110\\b"), "AVENIDA CIRCUNVALAR")
  x <- str_replace_all(x, regex("\\bAVENIDA 110\\b"), "AVENIDA CIRCUNVALAR")
  x <- str_replace_all(x, regex("(?<!AVENIDA )\\bMURILLO\\b"), "AVENIDA MURILLO")
  x <- str_replace_all(x, regex("(?<!AVENIDA )\\bCORDIALIDAD\\b"), "AVENIDA CORDIALIDAD")
  x <- str_replace_all(x, regex("\\b(AVENIDA CIRCUNVALAR)( \\1)+\\b"), "\\1")
  x <- str_replace_all(x, regex("\\b(AVENIDA CORDIALIDAD)( \\1)+\\b"), "\\1")
  x <- str_replace_all(x, regex("\\b(AVENIDA MURILLO)( \\1)+\\b"), "\\1")
  x
}

limpiar_numeral_2x <- function(x) str_replace_all(x, "#\\s*#", "#")

# "20 B 44" -> "20B 44" (letra suelta pegada al numero anterior; nunca fusiona "Y", se usa en rangos)
fusionar_letra_suelta <- function(x) {
  str_replace_all(x, regex("(\\d+)\\s+([A-Z])\\b"), function(m) {
    sapply(m, function(mi) {
      piezas <- str_match(mi, "(\\d+)\\s+([A-Z])")
      
      if (piezas[1, 3] == "Y") {
        return(mi)
      }
      
      paste0(piezas[1, 2], piezas[1, 3])
    })
  })
}
# FRENTE / FRENTE A / FRENTE AL / FRENTE A LA / FRENTE A NOMENCLATURA + 2 numeros
# -> se resuelve como si el accidente hubiese ocurrido en esa direccion
resolver_frente <- function(x) {
  patron <- paste0(
    "FRENTE(?:\\s+(?:A\\s+)?(?:LA\\s+)?(?:AL\\s+)?(?:NOMENCLATURA\\s+)?)?",
    "#?\\s*([0-9]+[A-Z]{0,2})\\b(?:\\s*-\\s*|\\s+)([0-9]+)\\b"
  )
  str_replace(x, regex(patron), "# \\1-\\2")
}

es_numero <- function(tok) str_detect(tok, "^[0-9]+[A-Z]{0,3}$")

# ------------------------------------------------------------
# 2. DETECTOR GENERICO DE "COMPONENTE VIAL"
#    (una via con nombre propio, o TIPO + NUMERO [+ SUR])
# ------------------------------------------------------------
vias_con_nombre <- list(
  c("VIA 40", "VIA40"),
  c("AVENIDA CIRCUNVALAR", "AVENIDA"),
  c("AVENIDA MURILLO", "AVENIDA"),
  c("AVENIDA CORDIALIDAD", "AVENIDA")
)

detectar_componente <- function(s) {
  for (via in vias_con_nombre) {
    if (str_starts(s, fixed(via[1]))) {
      return(list(texto = via[1], tipo = via[2], largo = nchar(via[1])))
    }
  }
  m <- str_match(s, "^(CALLE|CARRERA|DIAGONAL|TRANSVERSAL|AVENIDA)\\s+(\\S+)(\\s+SUR\\b)?")
  if (is.na(m[1, 1])) return(NULL)
  tipo <- m[1, 2]; num <- m[1, 3]; sur <- !is.na(m[1, 4])
  if (str_detect(num, "CALLE|CARRERA|AVENIDA|DIAGONAL|TRANSVERSAL")) return(NULL)
  if (tipo == "AVENIDA" && !es_numero(num)) return(NULL)
  comp <- paste0(tipo, " ", num, if (sur) " SUR" else "")
  list(texto = comp, tipo = tipo, largo = nchar(m[1, 1]))
}

OPUESTO <- c(CALLE = "CARRERA", CARRERA = "CALLE", DIAGONAL = "CARRERA",
             TRANSVERSAL = "CARRERA", AVENIDA = "CARRERA", VIA40 = "CALLE")

# ------------------------------------------------------------
# 3. FUNCION PRINCIPAL: estandariza una sola direccion
# ------------------------------------------------------------
estandarizar_una <- function(texto_original) {
  
  # ----------------------------------------------------------
  # 3.1 DIRECCION VACIA
  # ----------------------------------------------------------
  if (is.na(texto_original) || str_trim(texto_original) == "") {
    return(c(
      estandarizada = NA,
      observacion = "DIRECCION VACIA - VERIFICAR"
    ))
  }
  
  # ----------------------------------------------------------
  # 3.2 NORMALIZACION INICIAL
  # ----------------------------------------------------------
  s <- str_squish(toupper(texto_original))
  s <- str_squish(str_replace_all(s, "\\.", " "))
  s <- limpiar_numeral_2x(s)
  s <- fusionar_sur(s)
  s <- normalizar_numeral_no(s)
  s <- separar_tipo_pegado(s)
  s <- normalizar_tipos_via(s)
  s <- corregir_typos(s)
  s <- aplicar_nombres_propios(s)
  s <- fusionar_letra_suelta(s)
  s <- resolver_frente(s)
  s <- str_squish(s)
  
  # ----------------------------------------------------------
  # 3.3 DETECTAR PRIMER COMPONENTE VIAL
  # ----------------------------------------------------------
  comp <- detectar_componente(s)
  
  if (is.null(comp)) {
    return(c(
      estandarizada = s,
      observacion = "NO ESTANDARIZADA"
    ))
  }
  
  ancla_str <- comp$texto
  ancla_tipo <- comp$tipo
  
  # Todo lo que queda después del primer componente vial
  resto <- str_trim(str_sub(s, comp$largo + 1))
  resto <- str_squish(str_replace(resto, regex("^CON\\s+"), ""))
  
  opuesto <- OPUESTO[[ancla_tipo]]
  
  # ----------------------------------------------------------
  # 3.4 SI NO QUEDA NADA DESPUES DE LA VIA
  # ----------------------------------------------------------
  if (resto == "") {
    return(c(
      estandarizada = ancla_str,
      observacion = "NO ESTANDARIZADA"
    ))
  }
  
  # ----------------------------------------------------------
  # 3.5 FUNCION PARA REFERENCIAS NO RECONOCIDAS
  # ----------------------------------------------------------
  flag_referencia <- function() {
    c(
      estandarizada = s,
      observacion = "NO ESTANDARIZADA"
    )
  }
  
  # ----------------------------------------------------------
  # 3.6 DIRECCIONES DEL TIPO:
  #     CARRERA 46 ENTRE CALLE 72 Y 74
  #     CARRERA 46 ENTRE 72 Y 74
  # ----------------------------------------------------------
  if (str_starts(resto, "ENTRE ")) {
    
    resto_entre <- str_sub(resto, 7)
    
    comp_e <- detectar_componente(resto_entre)
    
    if (!is.null(comp_e)) {
      
      despues_e <- str_trim(
        str_sub(resto_entre, comp_e$largo + 1)
      )
      
      m2 <- str_match(
        despues_e,
        "^Y\\s+(\\S+)$"
      )
      
      if (!is.na(m2[1, 1])) {
        return(c(
          estandarizada = str_squish(
            paste(
              ancla_str,
              "ENTRE",
              comp_e$texto,
              "Y",
              m2[1, 2]
            )
          ),
          observacion = ""
        ))
      }
      
    } else {
      
      m2 <- str_match(
        resto_entre,
        "^([0-9A-Z]+)\\s+Y\\s+([0-9A-Z]+)$"
      )
      
      if (!is.na(m2[1, 1])) {
        return(c(
          estandarizada = str_squish(
            paste(
              ancla_str,
              "ENTRE",
              m2[1, 2],
              "Y",
              m2[1, 3]
            )
          ),
          observacion = ""
        ))
      }
    }
    
    return(flag_referencia())
  }
  
  # ----------------------------------------------------------
  # 3.7 RESTO QUE EMPIEZA CON "#"
  #
  # Ejemplo:
  # CARRERA 46 # 72-30
  # CARRERA 46 # 72
  # VIA 40 # 72-30
  # ----------------------------------------------------------
  m <- str_match(
    resto,
    "^#\\s*([0-9]+[A-Z]{0,3})\\b(?:\\s*-\\s*|\\s+)?([0-9]+[A-Z]{0,3})?\\s*(.*)$"
  )
  
  if (!is.na(m[1, 1]) &&
      (is.na(m[1, 4]) || m[1, 4] == "")) {
    
    n1 <- m[1, 2]
    n2 <- m[1, 3]
    
    if (!is.na(n2) && n2 != "") {
      
      nueva <- if (ancla_tipo == "VIA40") {
        paste(
          ancla_str,
          "CALLE",
          n1,
          "-",
          n2
        )
      } else {
        paste0(
          ancla_str,
          " # ",
          n1,
          "-",
          n2
        )
      }
      
    } else {
      
      nueva <- if (ancla_tipo == "VIA40") {
        paste(
          ancla_str,
          "CALLE",
          n1
        )
      } else {
        paste(
          ancla_str,
          opuesto,
          n1
        )
      }
    }
    
    return(c(
      estandarizada = str_squish(
        aplicar_nombres_propios(nueva)
      ),
      observacion = ""
    ))
  }
  
  # ----------------------------------------------------------
  # 3.8 RESTO COMO UN SOLO TOKEN CON GUION
  #
  # Ejemplo:
  # CARRERA 46 37-42
  # ----------------------------------------------------------
  m <- str_match(
    resto,
    "^([0-9]+[A-Z]{0,2})-([0-9]+[A-Z]{0,2})$"
  )
  
  if (!is.na(m[1, 1])) {
    
    n1 <- m[1, 2]
    n2 <- m[1, 3]
    
    nueva <- if (ancla_tipo == "VIA40") {
      paste(
        ancla_str,
        "CALLE",
        n1,
        "-",
        n2
      )
    } else {
      paste0(
        ancla_str,
        " # ",
        n1,
        "-",
        n2
      )
    }
    
    return(c(
      estandarizada = str_squish(
        aplicar_nombres_propios(nueva)
      ),
      observacion = ""
    ))
  }
  
  # ----------------------------------------------------------
  # 3.9 RESTO EMPIEZA CON UNA SEGUNDA VIA
  #
  # Ejemplo:
  # CARRERA 46 CALLE 72
  # CARRERA 46 CALLE 72 30
  # CARRERA 46 Y CALLE 72
  # ----------------------------------------------------------
  comp2 <- detectar_componente(resto)
  
  if (!is.null(comp2)) {
    
    despues <- str_trim(
      str_sub(resto, comp2$largo + 1)
    )
    
    # Segunda via sin información adicional
    if (despues == "") {
      
      nueva <- paste(
        ancla_str,
        comp2$texto
      )
      
      obs <- if (comp2$tipo == ancla_tipo) {
        "NO ESTANDARIZADA"
      } else {
        ""
      }
      
      return(c(
        estandarizada = str_squish(nueva),
        observacion = obs
      ))
    }
    
    # Segunda via seguida de Y + número
    m3 <- str_match(
      despues,
      "^Y\\s+(\\S+)$"
    )
    
    if (!is.na(m3[1, 1])) {
      return(c(
        estandarizada = str_squish(
          paste(
            ancla_str,
            comp2$texto,
            "Y",
            m3[1, 2]
          )
        ),
        observacion = ""
      ))
    }
    
    # Segunda via seguida de un número
    if (es_numero(despues)) {
      
      m4 <- str_match(
        comp2$texto,
        "^(CALLE|CARRERA|DIAGONAL|TRANSVERSAL|AVENIDA)\\s+(\\S+)(\\s+SUR)?"
      )
      
      # IMPORTANTE:
      # m4[1,2] = numero
      # m4[1,3] = SUR, si existe
      num2 <- if (!is.na(m4[1, 1])) {
        m4[1, 2]
      } else {
        comp2$texto
      }
      
      tiene_sur <- !is.na(m4[1, 1]) &&
        !is.na(m4[1, 3])
      
      nueva <- if (tiene_sur) {
        paste0(
          ancla_str,
          " # ",
          num2,
          " SUR ",
          despues
        )
      } else if (ancla_tipo == "VIA40") {
        paste(
          ancla_str,
          "CALLE",
          num2,
          "-",
          despues
        )
      } else {
        paste0(
          ancla_str,
          " # ",
          num2,
          "-",
          despues
        )
      }
      
      return(c(
        estandarizada = str_squish(
          aplicar_nombres_propios(nueva)
        ),
        observacion = ""
      ))
    }
    
    return(flag_referencia())
  }
  
  # ----------------------------------------------------------
  # 3.10 RESTO CON UN SOLO NUMERO
  #
  # Ejemplo:
  # CARRERA 46 72
  # ----------------------------------------------------------
  partes <- str_split(
    resto,
    " "
  )[[1]]
  
  if (length(partes) == 1 &&
      es_numero(partes[1])) {
    
    nueva <- if (ancla_tipo == "VIA40") {
      paste(
        ancla_str,
        "CALLE",
        partes[1]
      )
    } else {
      paste(
        ancla_str,
        opuesto,
        partes[1]
      )
    }
    
    return(c(
      estandarizada = str_squish(
        aplicar_nombres_propios(nueva)
      ),
      observacion = ""
    ))
  }
  
  # ----------------------------------------------------------
  # 3.11 RESTO CON DOS NUMEROS
  #
  # Ejemplo:
  # CARRERA 46 72 30
  # ----------------------------------------------------------
  if (length(partes) == 2 &&
      es_numero(partes[1]) &&
      es_numero(partes[2])) {
    
    nueva <- if (ancla_tipo == "VIA40") {
      paste(
        ancla_str,
        "CALLE",
        partes[1],
        "-",
        partes[2]
      )
    } else {
      paste0(
        ancla_str,
        " # ",
        partes[1],
        "-",
        partes[2]
      )
    }
    
    return(c(
      estandarizada = str_squish(
        aplicar_nombres_propios(nueva)
      ),
      observacion = ""
    ))
  }
  
  # ----------------------------------------------------------
  # 3.12 CUALQUIER CASO NO RECONOCIDO
  # ----------------------------------------------------------
  flag_referencia()
}

# ------------------------------------------------------------
# 4. UNIFICAR ORDEN CUANDO EL MISMO PAR APARECE EN AMBOS ORDENES
#    (CARRERA primero, EXCEPTO cuando interviene AVENIDA CIRCUNVALAR,
#     que siempre va primero)
# ------------------------------------------------------------
patron_componente <- paste0(
  "(AVENIDA CIRCUNVALAR|AVENIDA MURILLO|AVENIDA CORDIALIDAD|",
  "CALLE\\s+\\S+|CARRERA\\s+\\S+|DIAGONAL\\s+\\S+|TRANSVERSAL\\s+\\S+|AVENIDA\\s+\\S+)"
)

partir_componentes <- function(s) {
  if (is.na(s)) return(NULL)
  m <- str_extract_all(s, regex(patron_componente))[[1]]
  if (length(m) == 0) return(NULL)
  if (paste(m, collapse = " ") != s) return(NULL)
  m
}

orden_canonico <- function(comp) {
  if (any(str_starts(comp, "AVENIDA CIRCUNVALAR"))) {
    primeros <- comp[str_starts(comp, "AVENIDA CIRCUNVALAR")]
    segundos <- comp[!str_starts(comp, "AVENIDA CIRCUNVALAR")]
    if (length(primeros) > 0 && length(segundos) > 0) return(paste(primeros[1], segundos[1]))
    return(paste(comp, collapse = " "))
  }
  if (any(str_starts(comp, "CARRERA"))) {
    primeros <- comp[str_starts(comp, "CARRERA")]
    segundos <- comp[!str_starts(comp, "CARRERA")]
    if (length(primeros) > 0 && length(segundos) > 0) return(paste(primeros[1], segundos[1]))
    return(paste(comp, collapse = " "))
  }
  paste(comp, collapse = " ")
}

unificar_orden <- function(vector_std) {
  comp_list <- map(vector_std, partir_componentes)
  es_par <- map_lgl(comp_list, ~ !is.null(.x) && length(.x) == 2)

  claves <- rep(NA_character_, length(vector_std))
  claves[es_par] <- map_chr(comp_list[es_par], ~ paste(sort(.x), collapse = " ||| "))

  ordenes_por_clave <- tibble(clave = claves[es_par], orden = vector_std[es_par]) %>%
    distinct() %>%
    count(clave, name = "n_ordenes")
  claves_duplicadas <- ordenes_por_clave$clave[ordenes_por_clave$n_ordenes > 1]

  resultado <- vector_std
  idx <- which(es_par & claves %in% claves_duplicadas)
  for (i in idx) resultado[i] <- orden_canonico(comp_list[[i]])
  resultado
}

# ------------------------------------------------------------
# 5. EJECUCION
# ------------------------------------------------------------
datos <- read_excel(ruta_entrada, sheet = hoja_entrada)

resultado_parcial <- map_dfr(datos[[col_direccion]], ~ {
  r <- estandarizar_una(.x)
  tibble(estandarizada = unname(r["estandarizada"]), observacion = unname(r["observacion"]))
})

datos_final <- datos
datos_final[[col_std]] <- resultado_parcial$estandarizada
datos_final[[col_obs]] <- resultado_parcial$observacion

# unificar orden (CARRERA primero, salvo Circunvalar) sobre toda la columna generada
datos_final[[col_std]] <- unificar_orden(datos_final[[col_std]])

# observaciones vacias -> NA para que no se vean como texto ""
datos_final[[col_obs]] <- na_if(datos_final[[col_obs]], "")

# ------------------------------------------------------------
# 6. EXPORTAR A EXCEL
wb <- createWorkbook()

addWorksheet(wb, hoja_entrada)
writeData(wb, hoja_entrada, datos_final)

saveWorkbook(wb, ruta_salida, overwrite = TRUE)

cat("Listo. Filas con observaciones:",
    sum(!is.na(datos_final[[col_obs]])), "de", nrow(datos_final), "\n")

