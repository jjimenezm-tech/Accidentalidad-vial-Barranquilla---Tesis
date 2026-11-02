# ============================================================
# CONSTRUCCIÓN DE LA VARIABLE TIPO_VIA
# ACCIDENTALIDAD EN BARRANQUILLA
# ============================================================
#
# Objetivo:
#   Clasificar las direcciones estandarizadas de los accidentes
#   según el tipo de vía identificado en la dirección.
#
# La variable se construye mediante reglas de clasificación
# aplicadas sobre una versión normalizada de
# Direccion_Estandarizada.
#
# La normalización se utiliza únicamente para la clasificación
# y NO modifica la dirección estandarizada original.
# ============================================================


# ============================================================
# 1. PAQUETES
# ============================================================

library(readxl)
library(dplyr)
library(stringr)


# ============================================================
# 2. CARGAR BASE DE DATOS
# ============================================================

BASE_DE_DATOS_ACCIDENTALIDAD_BARRANQUILLA_COMPLETA <- read_excel(
  "BASE DE DATOS ACCIDENTALIDAD BARRANQUILLA COMPLETA.xlsx"
)

base <- BASE_DE_DATOS_ACCIDENTALIDAD_BARRANQUILLA_COMPLETA


# ============================================================
# 3. NORMALIZAR LA DIRECCIÓN PARA LA CLASIFICACIÓN
# ============================================================
#
# Se crea una variable auxiliar denominada
# Direccion_Clasificacion.
#
# Esta variable:
#   - convierte el texto a mayúsculas;
#   - elimina tildes;
#   - normaliza espacios;
#   - elimina espacios al inicio y al final.
#
# Direccion_Estandarizada permanece sin modificaciones.
# ============================================================

base <- base %>%
  mutate(
    Direccion_Clasificacion =
      Direccion_Estandarizada %>%
      str_to_upper() %>%
      str_replace_all("[ÁÀÄÂ]", "A") %>%
      str_replace_all("[ÉÈËÊ]", "E") %>%
      str_replace_all("[ÍÌÏÎ]", "I") %>%
      str_replace_all("[ÓÒÖÔ]", "O") %>%
      str_replace_all("[ÚÙÜÛ]", "U") %>%
      str_replace_all("\\s+", " ") %>%
      str_trim()
  )


# ============================================================
# 4. CLASIFICACIÓN PRINCIPAL DEL TIPO DE VÍA
# ============================================================
#
# IMPORTANTE:
#
# Las reglas se evalúan en el orden en que aparecen.
# Cuando una dirección cumple varias condiciones,
# se asigna la primera categoría que corresponda.
#
# Esto es especialmente importante para direcciones que
# contienen simultáneamente una vía principal y referencias
# a otras vías.
# ============================================================

base <- base %>%
  mutate(
    
    Tipo_via = case_when(
      
      
      # --------------------------------------------------------
      # 4.1 VÍA JUAN MINA
      # --------------------------------------------------------
      #
      # Se identifica cuando "VIA JUAN MINA" aparece en
      # cualquier parte de la dirección.
      #
      # Ejemplo:
      # CARRERA 38 VIA JUAN MINA
      #
      # Tiene prioridad para evitar que la dirección sea
      # clasificada únicamente como Carrera o Calle.
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "\\bVIA\\s+JUAN\\s+MINA\\b"
      ) ~ "Vía Juan Mina",
      
      
      # --------------------------------------------------------
      # 4.2 AVENIDA CORDIALIDAD
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "^AVENIDA\\s+CORDIALIDAD\\b"
      ) ~ "Avenida Cordialidad",
      
      
      # --------------------------------------------------------
      # 4.3 AVENIDA DEL RÍO
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "^AVENIDA\\s+DEL\\s+RIO\\b"
      ) ~ "Avenida del Río",
      
      
      # --------------------------------------------------------
      # 4.4 AVENIDA CIRCUNVALAR
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "^AVENIDA\\s+CIRCUNVALAR\\b"
      ) ~ "Avenida Circunvalar",
      
      
      # --------------------------------------------------------
      # 4.5 AVENIDA MURILLO
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "^AVENIDA\\s+MURILLO\\b"
      ) ~ "Avenida Murillo",
      
      
      # --------------------------------------------------------
      # 4.6 AVENIDA CIUDAD CARIBE
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "^AVENIDA\\s+CIUDAD\\s+CARIBE\\b"
      ) ~ "Avenida Ciudad Caribe",
      
      
      # --------------------------------------------------------
      # 4.7 VÍA 40
      # --------------------------------------------------------
      #
      # Puede aparecer en cualquier parte de la dirección.
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "\\bVIA\\s*40\\b"
      ) ~ "Vía 40",
      
      
      # --------------------------------------------------------
      # 4.8 VÍA GALAPA
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "\\bVIA\\s+GALAPA\\b"
      ) ~ "Vía Galapa",
      
      
      # --------------------------------------------------------
      # 4.9 PUENTE PUMAREJO
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "\\bPUENTE\\s+(NUEVO\\s+)?PUMAREJO\\b"
      ) ~ "Puente Pumarejo",
      
      
      # --------------------------------------------------------
      # 4.10 VÍA PROSPERIDAD
      # --------------------------------------------------------
      #
      # Se contemplan las formas:
      #   VIA PROSPERIDAD
      #   VIA LA PROSPERIDAD
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "\\bVIA\\s+(?:LA\\s+)?PROSPERIDAD\\b"
      ) ~ "Vía Prosperidad",
      
      
      # --------------------------------------------------------
      # 4.11 PARQUEADERO
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "\\bPARQUEADERO\\b"
      ) ~ "Parqueadero",
      
      
      # --------------------------------------------------------
      # 4.12 CALLE 30
      # --------------------------------------------------------
      #
      # Se separa de Calle porque constituye una vía específica
      # que posteriormente se conserva como categoría propia
      # en la base maestra.
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "^CALLE\\s+30(?:\\b|\\s|#)"
      ) ~ "Calle 30",
      
      
      # --------------------------------------------------------
      # 4.13 CALLE
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "^CALLE\\b"
      ) ~ "Calle",
      
      
      # --------------------------------------------------------
      # 4.14 CARRERA
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "^CARRERA\\b"
      ) ~ "Carrera",
      
      
      # --------------------------------------------------------
      # 4.15 DIAGONAL
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "^DIAGONAL\\b"
      ) ~ "Diagonal",
      
      
      # --------------------------------------------------------
      # 4.16 TRANSVERSAL
      # --------------------------------------------------------
      #
      # Se contempla también la forma con error de escritura
      # "TRASNVERSAL".
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "^TRANSVERSAL\\b|^TRASNVERSAL\\b"
      ) ~ "Transversal",
      
      
      # --------------------------------------------------------
      # 4.17 RESTO
      # --------------------------------------------------------
      
      TRUE ~ "Otra vía / no clasificada"
    )
  )


# ============================================================
# 5. IDENTIFICACIÓN DE INTERSECCIONES
# ============================================================
#
# La clasificación de intersecciones se realiza mediante una
# variable auxiliar denominada Subtipo_via.
#
# Una dirección se considera intersección cuando contiene
# referencias a dos tipos de vías y no contiene "#".
#
# La ausencia de "#" se utiliza para diferenciar estas
# referencias de nomenclaturas prediales.
# ============================================================

base <- base %>%
  mutate(
    
    Subtipo_via = case_when(
      
      
      # --------------------------------------------------------
      # 5.1 INTERSECCIÓN CALLE - CARRERA
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "\\bCALLE\\s+[0-9A-Z]+\\b"
      ) &
        str_detect(
          Direccion_Clasificacion,
          "\\bCARRERA\\s+[0-9A-Z]+\\b"
        ) &
        !str_detect(
          Direccion_Clasificacion,
          "#"
        ) ~ "Intersección Calle-Carrera",
      
      
      # --------------------------------------------------------
      # 5.2 INTERSECCIÓN DIAGONAL - CARRERA
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "\\bDIAGONAL\\s+[0-9A-Z]+\\b"
      ) &
        str_detect(
          Direccion_Clasificacion,
          "\\bCARRERA\\s+[0-9A-Z]+\\b"
        ) &
        !str_detect(
          Direccion_Clasificacion,
          "#"
        ) ~ "Intersección Diagonal-Carrera",
      
      
      # --------------------------------------------------------
      # 5.3 INTERSECCIÓN TRANSVERSAL - CARRERA
      # --------------------------------------------------------
      
      str_detect(
        Direccion_Clasificacion,
        "\\bTRANSVERSAL\\s+[0-9A-Z]+\\b"
      ) &
        str_detect(
          Direccion_Clasificacion,
          "\\bCARRERA\\s+[0-9A-Z]+\\b"
        ) &
        !str_detect(
          Direccion_Clasificacion,
          "#"
        ) ~ "Intersección Transversal-Carrera",
      
      
      # --------------------------------------------------------
      # 5.4 NO APLICA
      # --------------------------------------------------------
      
      TRUE ~ "No aplica"
    )
  )


# ============================================================
# 6. UNIFICACIÓN DE LAS INTERSECCIONES
# ============================================================
#
# Los diferentes subtipos de intersección se consolidan en
# una única categoría:
#
#                    "Intersección"
#
# Esto permite que la variable Tipo_via represente una
# categoría espacial común para todos los accidentes ocurridos
# en intersecciones.
# ============================================================

base <- base %>%
  mutate(
    
    Tipo_via = case_when(
      
      Subtipo_via %in% c(
        "Intersección Calle-Carrera",
        "Intersección Diagonal-Carrera",
        "Intersección Transversal-Carrera"
      ) ~ "Intersección",
      
      TRUE ~ Tipo_via
    )
  )


# ============================================================
# 7. VERIFICACIÓN DE LA VARIABLE
# ============================================================

cat("\n============================================\n")
cat("DISTRIBUCIÓN FINAL DE TIPO_VIA\n")
cat("============================================\n\n")

print(
  base %>%
    count(Tipo_via, sort = TRUE)
)


# ============================================================
# 8. VERIFICACIÓN DE SUBTIPOS DE INTERSECCIÓN
# ============================================================

cat("\n============================================\n")
cat("DISTRIBUCIÓN DE SUBTIPO_VIA\n")
cat("============================================\n\n")

print(
  base %>%
    count(Subtipo_via, sort = TRUE)
)


# ============================================================
# 9. MOSTRAR EJEMPLOS DE CADA CATEGORÍA
# ============================================================

cat("\n============================================\n")
cat("EJEMPLOS POR TIPO DE VÍA\n")
cat("============================================\n\n")

ejemplos_tipo <- base %>%
  select(
    Direccion_Estandarizada,
    Tipo_via,
    Subtipo_via
  ) %>%
  distinct(Tipo_via, .keep_all = TRUE) %>%
  arrange(Tipo_via)

print(
  ejemplos_tipo,
  n = 100
)


# ============================================================
# 10. REVISAR DIRECCIONES NO CLASIFICADAS
# ============================================================
#
# Esta revisión permite identificar direcciones que no fueron
# reconocidas por las reglas principales.
# ============================================================

cat("\n============================================\n")
cat("DIRECCIONES NO CLASIFICADAS\n")
cat("============================================\n\n")

no_clasificadas <- base %>%
  filter(
    Tipo_via == "Otra vía / no clasificada"
  ) %>%
  count(
    Direccion_Estandarizada,
    sort = TRUE
  )

print(
  no_clasificadas,
  n = 250
)


# ============================================================
# 11. VERIFICAR ESPECÍFICAMENTE VÍA JUAN MINA
# ============================================================

cat("\n============================================\n")
cat("VERIFICACIÓN VÍA JUAN MINA\n")
cat("============================================\n\n")

print(
  base %>%
    filter(
      str_detect(
        Direccion_Clasificacion,
        "\\bVIA\\s+JUAN\\s+MINA\\b"
      )
    ) %>%
    count(
      Tipo_via,
      sort = TRUE
    )
)


# ============================================================
# 12. VERIFICAR NÚMERO TOTAL DE REGISTROS
# ============================================================

cat("\n============================================\n")
cat("VERIFICACIÓN FINAL\n")
cat("============================================\n\n")

cat(
  "Número de registros:",
  nrow(base),
  "\n"
)

cat(
  "Número de categorías:",
  n_distinct(base$Tipo_via),
  "\n"
)
