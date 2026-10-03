# ==============================================================================
# 01_CONTROL_AUDITORIA.R
# ==============================================================================
# Proyecto : ¿Están bien calibradas las casas de apuestas?
# Curso    : Estadística Industrial · Universidad del Magdalena · 2026-II
# Autores  : Mateo Valencia, Samuel Lopez, Camilo Henriquez,
#            Valeria De Jesus Gutierrez Nuñez
#
# Qué hace : Audita los CSV originales de Football-Data antes de usarlos: cuotas imposibles
#            (<= 1), estadísticas negativas, datos faltantes con trazabilidad por partido
#            y disponibilidad de cada columna. No modifica ningún archivo.
# Lee      : datos_crudos/SP1 2017-2018.csv, datos_crudos/SP1 2018-2019.csv
# Genera   : Solo informe en consola
# Antes    : 00_configuracion.R
# Después  : 02_unificacion_base.R
# ==============================================================================

# 0. CONFIGURACIÓN (rutas y parámetros centralizados en 00_configuracion.R)
# ------------------------------------------------------------------------------
if (!exists("CONFIG_CARGADA")) {
  source(if (file.exists("scripts/00_configuracion.R")) "scripts/00_configuracion.R" else "00_configuracion.R",
         encoding = "UTF-8")
}

# 1. TEMPORADAS A AUDITAR Y OPERADORES USADOS
# ------------------------------------------------------------------------------
# Vienen de 00_configuracion.R:
#   rutas_crudas        -> un CSV por temporada
#   operadores_analisis -> prefijos usados en el análisis; los faltantes de otros
#                          operadores (p. ej. LB = Ladbrokes) se marcan como "No usado"

# 2. INICIO DEL CICLO DE AUDITORÍA MÚLTIPLE
# ------------------------------------------------------------------------------
for (ruta_cruda in rutas_crudas) {
  
  # Limpieza de la ruta
  ruta <- gsub("\\\\", "/", ruta_cruda)
  ruta <- gsub("\"", "", ruta)
  
  # Extraer el nombre del archivo para los títulos principales
  nombre_archivo <- basename(ruta)
  
  cat("\n\n########################################################################\n")
  cat("        RESULTADOS DEL CONTROL TEMPORADA:", nombre_archivo, "\n")
  cat("########################################################################\n")
  
  # Lectura de la base con manejador de errores por si una ruta está mal escrita
  datos_crudos <- tryCatch({
    read.csv(ruta, stringsAsFactors = FALSE)
  }, error = function(e) {
    cat("Error al leer el archivo. Verifica la ruta.\n")
    return(NULL)
  })
  
  if (is.null(datos_crudos)) next
  
  # MEJORA 1: Eliminar columnas "fantasma" que Football-Data a veces incluye al final
  columnas_validas <- vapply(datos_crudos, function(x) sum(is.na(x) | x == "") < nrow(datos_crudos), FUN.VALUE = logical(1))
  datos_crudos <- datos_crudos[, columnas_validas]
  
  diagnostico <- list(archivo_analizado = ruta)
  diagnostico$total_variables <- ncol(datos_crudos)
  diagnostico$total_observaciones <- nrow(datos_crudos)
  
  # 3. DEFINICIÓN DEL DICCIONARIO PARA RUTEO DE VALIDACIONES
  # ------------------------------------------------------------------------------
  vars_no_cuotas <- c(
    "Div", "Date", "Time", "HomeTeam", "AwayTeam", "Referee", "Attendance",
    "FTHG", "HG", "FTAG", "AG", "FTR", "Res", "HTHG", "HTAG", "HTR",
    "HS", "AS", "HST", "AST", "HHW", "AHW", "HC", "AC", "HF", "AF",
    "HFKC", "AFKC", "HO", "AO", "HY", "AY", "HR", "AR", "HBP", "ABP",
    "BbAHh", "AHh", "AHCh", "BbAH", "Bb1X2", "BbOU" # <- VARIABLES DE CONTEO AÑADIDAS
  )
  
  todas_las_columnas <- names(datos_crudos)
  cols_cuotas <- setdiff(todas_las_columnas, vars_no_cuotas)
  cols_estadisticas <- intersect(todas_las_columnas, vars_no_cuotas)
  
  # Se excluyen de las métricas físicas (goles/tarjetas >= 0) las de texto, los hándicaps y los conteos
  cols_stats_numericas <- setdiff(cols_estadisticas, c("Div", "Date", "Time", "HomeTeam", "AwayTeam", "Referee", "FTR", "Res", "HTR", "BbAHh", "AHh", "AHCh", "BbAH", "Bb1X2", "BbOU"))
  
  # 4. ANÁLISIS DE FALTANTES (NAs y Vacíos)
  # ------------------------------------------------------------------------------
  nas_por_variable <- vapply(datos_crudos, function(x) sum(is.na(x) | as.character(x) == ""), FUN.VALUE = numeric(1))
  diagnostico$total_nas_global <- sum(nas_por_variable)
  
  # 5. LÓGICA DE NEGOCIO (CON TRAZABILIDAD DE PARTIDOS)
  # ------------------------------------------------------------------------------
  registro_anomalias <- data.frame(
    Partido = character(), Fecha = character(), Variable = character(), 
    Valor = numeric(), Problema = character(), stringsAsFactors = FALSE
  )
  
  # A. Auditoría de Cuotas Imposibles (<= 1)
  for (col in cols_cuotas) {
    valores <- suppressWarnings(as.numeric(datos_crudos[[col]]))
    indices_error <- which(valores <= 1)
    
    if (length(indices_error) > 0) {
      for (i in indices_error) {
        partido <- paste(datos_crudos$HomeTeam[i], "vs", datos_crudos$AwayTeam[i])
        fecha <- as.character(datos_crudos$Date[i])
        registro_anomalias <- rbind(registro_anomalias, 
                                    data.frame(Partido = partido, Fecha = fecha, Variable = col, Valor = valores[i], Problema = "Cuota <= 1"))
      }
    }
  }
  
  # B. Auditoría de Estadísticas Físicas Imposibles (< 0)
  for (col in cols_stats_numericas) {
    valores <- suppressWarnings(as.numeric(datos_crudos[[col]]))
    indices_error <- which(valores < 0)
    
    if (length(indices_error) > 0) {
      for (i in indices_error) {
        partido <- paste(datos_crudos$HomeTeam[i], "vs", datos_crudos$AwayTeam[i])
        fecha <- as.character(datos_crudos$Date[i])
        registro_anomalias <- rbind(registro_anomalias, 
                                    data.frame(Partido = partido, Fecha = fecha, Variable = col, Valor = valores[i], Problema = "Stat Físico < 0"))
      }
    }
  }
  
  # C. MEJORA 2: Trazabilidad de Datos Faltantes (qué partido, qué operador, qué columnas)
  # Las cuotas 1X2 (H, D, A) de un mismo operador se agrupan en una sola fila por partido.
  registro_faltantes <- data.frame(
    Fila = integer(), Partido = character(), Fecha = character(), Grupo = character(),
    Columnas = character(), Usado_en_analisis = character(), stringsAsFactors = FALSE
  )
  
  # Prefijos con trío completo H/D/A en el archivo (p. ej. PSC -> PSCH, PSCD, PSCA)
  prefijos_1x2 <- unique(sub("H$", "", grep("H$", cols_cuotas, value = TRUE)))
  prefijos_1x2 <- prefijos_1x2[paste0(prefijos_1x2, "D") %in% cols_cuotas & paste0(prefijos_1x2, "A") %in% cols_cuotas]
  
  # Grupo de cada columna: su operador 1X2 si pertenece a un trío; si no, la propia columna
  grupo_columna <- setNames(names(datos_crudos), names(datos_crudos))
  for (pref in prefijos_1x2) grupo_columna[paste0(pref, c("H", "D", "A"))] <- pref
  
  cols_con_faltantes <- names(nas_por_variable)[nas_por_variable > 0]
  if (length(cols_con_faltantes) > 0) {
    for (grp in unique(grupo_columna[cols_con_faltantes])) {
      cols_grp <- intersect(cols_con_faltantes, names(grupo_columna)[grupo_columna == grp])
      falta_mat <- sapply(cols_grp, function(cc) is.na(datos_crudos[[cc]]) | as.character(datos_crudos[[cc]]) == "")
      falta_mat <- matrix(falta_mat, nrow = nrow(datos_crudos))   # asegura matriz aunque sea 1 columna
      filas_falta <- which(rowSums(falta_mat) > 0)
      for (i in filas_falta) {
        registro_faltantes <- rbind(registro_faltantes, data.frame(
          Fila = i,
          Partido = paste(datos_crudos$HomeTeam[i], "vs", datos_crudos$AwayTeam[i]),
          Fecha = as.character(datos_crudos$Date[i]),
          Grupo = grp,
          Columnas = paste(cols_grp[falta_mat[i, ]], collapse = ", "),
          Usado_en_analisis = ifelse(grp %in% operadores_analisis, "Sí", "No"),
          stringsAsFactors = FALSE))
      }
    }
    registro_faltantes <- registro_faltantes[order(registro_faltantes$Usado_en_analisis != "Sí", registro_faltantes$Fila), ]
    row.names(registro_faltantes) <- NULL
  }
  diagnostico$partidos_con_faltantes <- length(unique(registro_faltantes$Fila))
  diagnostico$faltantes_en_operadores_usados <- sum(registro_faltantes$Usado_en_analisis == "Sí")
  
  # 6. GENERACIÓN DE LA TABLA DE DISPONIBILIDAD
  # ------------------------------------------------------------------------------
  tabla_disponibilidad <- data.frame(
    Variable = names(datos_crudos),
    Tipo = ifelse(names(datos_crudos) %in% vars_no_cuotas, "Metadato/Estadística", "Cuota/Mercado"),
    Datos_Faltantes = nas_por_variable[names(datos_crudos)],
    Porcentaje_Completo = round(((nrow(datos_crudos) - nas_por_variable[names(datos_crudos)]) / nrow(datos_crudos)) * 100, 1),
    stringsAsFactors = FALSE
  )
  tabla_disponibilidad <- tabla_disponibilidad[order(tabla_disponibilidad$Variable), ]
  row.names(tabla_disponibilidad) <- NULL
  
  # 7. IMPRESIÓN DE RESULTADOS
  # ------------------------------------------------------------------------------
  cat(">> 1. RESUMEN ESTRUCTURAL\n")
  cat("Observaciones (Filas):", diagnostico$total_observaciones, "\n")
  cat("Variables Totales Válidas:", diagnostico$total_variables, "\n")
  cat("   - Identificadas como Cuotas:", length(cols_cuotas), "\n")
  cat("   - Identificadas como Stats/Metadatos:", length(cols_estadisticas), "\n\n")
  
  cat(">> 2. VALIDACIÓN LÓGICA (Trazabilidad de Anomalías)\n")
  if (nrow(registro_anomalias) > 0) {
    cat(paste("¡ALERTA! Se encontraron", nrow(registro_anomalias), "anomalías lógicas.\nDetalle de los errores:\n"))
    print(registro_anomalias)
  } else {
    cat("   - NINGUNA anomalía detectada. Todas las cuotas son > 1 y los stats >= 0.\n")
  }
  
  # MEJORA 2 (impresión): detalle partido por partido de los datos faltantes
  cat("\n>> 2.1 TRAZABILIDAD DE DATOS FALTANTES (Partido, Operador y Columnas)\n")
  if (nrow(registro_faltantes) > 0) {
    cat("   - Partidos con algún faltante:", diagnostico$partidos_con_faltantes, "\n")
    cat("   - Registros en operadores USADOS en el análisis:", diagnostico$faltantes_en_operadores_usados, "\n")
    cat("   - Registros en operadores NO usados:", sum(registro_faltantes$Usado_en_analisis == "No"), "\n\n")
    print(registro_faltantes, row.names = FALSE)
  } else {
    cat("   - NINGÚN dato faltante. Todas las columnas están 100% completas.\n")
  }
  
  if(class(datos_crudos$Date) == "character") {
    cat("\n   [!] IMPORTANTE: R leyó la fecha como texto. Se requerirá as.Date() en la limpieza.\n")
  }
  
  cat("\n>> 3. TABLA DE VARIABLES DISPONIBLES EN EL ARCHIVO\n")
  print(tabla_disponibilidad, row.names = FALSE)
  
  cat("\n>> 4. LISTA PLANA PARA EXTRACCIÓN RÁPIDA (PARA COPIAR Y COMPARAR)\n")
  cat(paste(sort(names(datos_crudos)), collapse = ", "), "\n")
  cat("========================================================================\n")
}