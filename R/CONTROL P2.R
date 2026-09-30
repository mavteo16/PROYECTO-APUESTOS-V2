# ==============================================================================
# SCRIPT DE CONTROL Y AUDITORÍA DE BD (VERSIÓN 6.2 - DICCIONARIO BLINDADO)
# ==============================================================================

# 1. CONFIGURACIÓN DE RUTAS (Añade aquí todas las temporadas que quieras comparar)
# ------------------------------------------------------------------------------
rutas_crudas <- c(
  "C:/Users/Invitadou/Downloads/SP1 2017-2018.csv",
  "C:/Users/Invitadou/Downloads/SP1 2018-2019.csv"
  # Puedes añadir más rutas separadas por comas
)

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
  
  if(class(datos_crudos$Date) == "character") {
    cat("\n   [!] IMPORTANTE: R leyó la fecha como texto. Se requerirá as.Date() en la limpieza.\n")
  }
  
  cat("\n>> 3. TABLA DE VARIABLES DISPONIBLES EN EL ARCHIVO\n")
  print(tabla_disponibilidad, row.names = FALSE)
  
  cat("\n>> 4. LISTA PLANA PARA EXTRACCIÓN RÁPIDA (PARA COPIAR Y COMPARAR)\n")
  cat(paste(sort(names(datos_crudos)), collapse = ", "), "\n")
  cat("========================================================================\n")
}
}
