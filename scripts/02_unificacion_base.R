# ==============================================================================
# 02_UNIFICACION_BASE.R
# ==============================================================================
# Proyecto : ¿Están bien calibradas las casas de apuestas?
# Curso    : Estadística Industrial · Universidad del Magdalena · 2026-II
# Autores  : Mateo Valencia, Samuel Lopez, Camilo Henriquez,
#            Valeria De Jesus Gutierrez Nuñez
#
# Qué hace : Une los CSV de Football-Data de cada temporada, toma las columnas del proyecto,
#            les pone nombre en español y calcula Promedio_Apertura (promedio de las
#            6 casas comparadas). Al final verifica la base contra la existente.
# Lee      : datos_crudos/SP1 2017-2018.csv, datos_crudos/SP1 2018-2019.csv
# Genera   : datos/Base_Unificada_Limpia_VAR.csv
# Antes    : 01_control_auditoria.R
# Después  : 03_margen_probabilidades.R
# ==============================================================================

# Diccionario de columnas (según football-data.co.uk/notes.txt):
#   Div              -> Liga
#   Date             -> Fecha                   (se conserva el texto original dd/mm/aa o dd/mm/aaaa)
#   HomeTeam/AwayTeam-> Equipo_Local / Equipo_Visitante
#   FTHG/FTAG        -> Goles_Local / Goles_Visitante   (goles al final del partido)
#   FTR              -> Resultado_Final         (H = gana local, D = empate, A = gana visitante)
#   B365H/D/A        -> Bet365_Local/Empate/Visitante
#   BWH/D/A          -> Bwin_...
#   IWH/D/A          -> Interwetten_...
#   PSH/D/A          -> Pinnacle_...
#   WHH/D/A          -> WilliamHill_...
#   VCH/D/A          -> BetVictor_...
#   Promedio_Apertura_... -> SE CALCULA AQUÍ: promedio simple de las cuotas de las 6 casas que se
#                            comparan (Bet365, Bwin, Interwetten, Pinnacle, WilliamHill, BetVictor),
#                            partido por partido y resultado por resultado.
#                            (No se usa la columna BbAv de Football-Data, que promedia muchas más casas.)
#   PSCH/D/A         -> Pinnacle_Cierre_...     (cuota de CIERRE de Pinnacle: la "C" = closing)
#
# ==============================================================================

# 0. CONFIGURACIÓN (rutas y parámetros centralizados en 00_configuracion.R)
# ------------------------------------------------------------------------------
if (!exists("CONFIG_CARGADA")) {
  source(if (file.exists("scripts/00_configuracion.R")) "scripts/00_configuracion.R" else "00_configuracion.R",
         encoding = "UTF-8")
}

# 1. RUTAS (definidas en 00_configuracion.R)
# ------------------------------------------------------------------------------
ruta_salida <- ruta_base_unificada          # archivo que se crea
ruta_base_existente <- ruta_base_analitica  # base para la verificación final (si ya existe)

# 2. DICCIONARIO: COLUMNA ORIGINAL DE FOOTBALL-DATA -> NOMBRE EN EL PROYECTO
# ------------------------------------------------------------------------------
diccionario_info <- c(
  Liga = "Div", Fecha = "Date",
  Equipo_Local = "HomeTeam", Equipo_Visitante = "AwayTeam",
  Goles_Local = "FTHG", Goles_Visitante = "FTAG",
  Resultado_Final = "FTR"
)

# Prefijo de Football-Data de cada operador (se le agrega H, D o A)
diccionario_operadores <- c(as.list(casas_proyecto),   # casas definidas en 00_configuracion.R
                            list(Pinnacle_Cierre = "PSC"))

# Casas que forman el promedio de apertura (cambiar aquí si se agregan o quitan casas)
casas_promedio <- names(casas_proyecto)   # definidas en 00_configuracion.R
sufijos <- c(Local = "H", Empate = "D", Visitante = "A")

# 3. LECTURA Y CONSTRUCCIÓN DE CADA TEMPORADA
# ------------------------------------------------------------------------------
cat("\n========================================================================\n")
cat(">> UNIFICANDO TEMPORADAS DE FOOTBALL-DATA\n")
cat("========================================================================\n")

lista_temporadas <- list()
crudos <- list()   # se guardan para la verificación del final

for (ruta_cruda in rutas_crudas) {
  ruta <- gsub("\\\\", "/", ruta_cruda)
  ruta <- gsub("\"", "", ruta)
  nombre_archivo <- basename(ruta)
  
  datos <- tryCatch(read.csv(ruta, stringsAsFactors = FALSE),
                    error = function(e) { cat("Error al leer", nombre_archivo, "- verifica la ruta.\n"); NULL })
  if (is.null(datos)) next
  
  # Quitar filas vacías que Football-Data a veces deja al final
  datos <- datos[!is.na(datos$HomeTeam) & datos$HomeTeam != "", ]
  crudos[[nombre_archivo]] <- datos
  
  base <- data.frame(row.names = seq_len(nrow(datos)))
  
  # A. Información del partido
  for (nuevo in names(diccionario_info)) {
    original <- diccionario_info[[nuevo]]
    if (!original %in% names(datos)) stop("Falta la columna ", original, " en ", nombre_archivo)
    base[[nuevo]] <- datos[[original]]
  }
  
  # B. Cuotas de cada operador
  cat("\n", nombre_archivo, " (", nrow(datos), " partidos)\n", sep = "")
  for (operador in names(diccionario_operadores)) {
    opciones <- diccionario_operadores[[operador]]
    prefijo <- opciones[paste0(opciones, "H") %in% names(datos)][1]
    for (r in names(sufijos)) {
      nueva_col <- paste0(operador, "_", r)
      if (is.na(prefijo)) {
        base[[nueva_col]] <- NA_real_
      } else {
        base[[nueva_col]] <- suppressWarnings(as.numeric(datos[[paste0(prefijo, sufijos[[r]])]]))
      }
    }
    cat(sprintf("   %-18s <- %s\n", operador,
                ifelse(is.na(prefijo), "NO DISPONIBLE en este archivo",
                       paste0(prefijo, "H / ", prefijo, "D / ", prefijo, "A"))))
  }
  
  # C. Promedio de apertura = promedio de las cuotas de las casas comparadas
  for (r in names(sufijos)) {
    cuotas_casas <- base[, paste0(casas_promedio, "_", r)]
    base[[paste0("Promedio_Apertura_", r)]] <- round(rowMeans(cuotas_casas, na.rm = TRUE), 4)
  }
  n_casas <- rowSums(!is.na(base[, paste0(casas_promedio, "_Local")]))
  cat(sprintf("   %-18s <- promedio de %s (partidos con las %d casas: %d de %d)\n", "Promedio_Apertura",
              paste(casas_promedio, collapse = ", "), length(casas_promedio), sum(n_casas == length(casas_promedio)), nrow(base)))
  
  # D. Orden final de columnas (el mismo que espera el script de margen)
  orden <- c(names(diccionario_info),
             as.vector(t(outer(c(casas_promedio, "Promedio_Apertura", "Pinnacle_Cierre"), names(sufijos), paste, sep = "_"))))
  base <- base[, orden]
  
  lista_temporadas[[nombre_archivo]] <- base
}

# 4. UNIÓN DE TEMPORADAS Y EXPORTACIÓN
# ------------------------------------------------------------------------------
base_unificada <- do.call(rbind, lista_temporadas)
row.names(base_unificada) <- NULL

write.csv(base_unificada, file = ruta_salida, row.names = FALSE)

cat("\n>> Base unificada:", nrow(base_unificada), "partidos x", ncol(base_unificada), "columnas\n")
cat(">> Guardada en:", ruta_salida, "\n")

# 5. VERIFICACIÓN: ¿REPRODUCE ESTE SCRIPT LA BASE ANALÍTICA QUE YA TENEMOS?
# ------------------------------------------------------------------------------
# Compara, celda por celda, las columnas originales de la base analítica con las
# que acaba de crear este script. Si alguna no coincide, busca en los CSV crudos
# qué columna de Football-Data sí coincide, para saber de dónde salió.
# Nota: si la base existente se creó con la versión anterior (promedio de Football-Data),
# las 3 columnas de Promedio_Apertura NO coincidirán; es lo esperado tras este cambio,
# y el detector mostrará de qué columna cruda venían antes.
if (file.exists(ruta_base_existente)) {
  cat("\n========================================================================\n")
  cat(">> VERIFICACIÓN CONTRA LA BASE ANALÍTICA EXISTENTE\n")
  cat("========================================================================\n")
  existente <- read.csv(ruta_base_existente, stringsAsFactors = FALSE)
  
  iguales <- function(a, b) {
    if (length(a) != length(b)) return(FALSE)
    na_a <- is.na(a) | as.character(a) == ""; na_b <- is.na(b) | as.character(b) == ""
    if (any(na_a != na_b)) return(FALSE)
    an <- suppressWarnings(as.numeric(a)); bn <- suppressWarnings(as.numeric(b))
    if (all(!is.na(an[!na_a])) && all(!is.na(bn[!na_b]))) return(all(abs(an[!na_a] - bn[!na_b]) < 1e-9))
    all(as.character(a[!na_a]) == as.character(b[!na_b]))
  }
  
  crudo_total <- do.call(rbind, lapply(crudos, function(d) d[, Reduce(intersect, lapply(crudos, names))]))
  n_ok <- 0
  for (col in names(base_unificada)) {
    if (!col %in% names(existente)) { cat(sprintf("   %-30s no está en la base existente\n", col)); next }
    ok <- iguales(base_unificada[[col]], existente[[col]])
    n_ok <- n_ok + ok
    if (ok) {
      cat(sprintf("   [OK]  %-28s idéntica\n", col))
    } else {
      fuentes <- names(crudo_total)[vapply(crudo_total, function(x) iguales(x, existente[[col]]), logical(1))]
      cat(sprintf("   [!!]  %-28s NO coincide. Columna(s) cruda(s) que sí coinciden: %s\n", col,
                  ifelse(length(fuentes) == 0, "ninguna", paste(fuentes, collapse = ", "))))
    }
  }
  cat(sprintf("\n>> %d de %d columnas coinciden exactamente con la base analítica existente.\n",
              n_ok, ncol(base_unificada)))
}
cat("========================================================================\n")