# ==============================================================================
# SCRIPT DE CONVERSIÓN PROBABILÍSTICA Y REMOCIÓN DE MARGEN (FASE 2 ORGANIZADA)
# ==============================================================================

# 1. LECTURA DE LA BASE LIMPIA UNIFICADA Y CONFIGURACIÓN DE RUTAS
# ------------------------------------------------------------------------------
ruta_cruda <- "C:/Users/Invitadou/Downloads/Base_Unificada_Limpia_VAR.csv"
ruta <- gsub("\\\\", "/", ruta_cruda)
ruta <- gsub("\"", "", ruta)
base_datos <- read.csv(ruta, stringsAsFactors = FALSE)

# Directorio Maestro de Resultados y subcarpeta Tablas_CSV
ruta_resultados <- "C:/Users/Invitadou/Desktop/PROYECTO-APUESTAS/Resultados"
dir_tablas <- file.path(ruta_resultados, "Tablas_CSV")

if (!dir.exists(dir_tablas)) {
  dir.create(dir_tablas, recursive = TRUE)
}

# 2. FUNCIONES MATEMÁTICAS PARA EL TRATAMIENTO DEL MARGEN (ANÁLISIS DUAL)
# ------------------------------------------------------------------------------
calcular_probabilidades <- function(o_H, o_D, o_A) {
  if (is.na(o_H) | is.na(o_D) | is.na(o_A)) {
    return(rep(NA, 7))
  }
  
  pi_H <- 1 / o_H
  pi_D <- 1 / o_D
  pi_A <- 1 / o_A
  suma_pi <- pi_H + pi_D + pi_A
  
  margen <- suma_pi - 1
  
  # Método 1: Normalización Multiplicativa (Proporcional)
  p_H_mult <- pi_H / suma_pi
  p_D_mult <- pi_D / suma_pi
  p_A_mult <- pi_A / suma_pi
  
  # Método 2: Método de Potencia (Ajuste no lineal / Logarítmico)
  f_potencia <- function(k) { (pi_H^(1/k) + pi_D^(1/k) + pi_A^(1/k)) - 1 }
  
  ajuste <- tryCatch(
    uniroot(f_potencia, interval = c(0.01, 1))$root,
    error = function(e) NA
  )
  
  if (is.na(ajuste)) {
    p_H_pot <- NA; p_D_pot <- NA; p_A_pot <- NA
  } else {
    p_H_pot <- pi_H^(1/ajuste)
    p_D_pot <- pi_D^(1/ajuste)
    p_A_pot <- pi_A^(1/ajuste)
  }
  
  return(c(margen, p_H_mult, p_D_mult, p_A_mult, p_H_pot, p_D_pot, p_A_pot))
}

# 3. DETECCIÓN DINÁMICA DE CASAS DE APUESTAS
# ------------------------------------------------------------------------------
cols_local <- grep("_Local$", names(base_datos), value = TRUE)
prefijos_casas <- gsub("_Local$", "", cols_local)
prefijos_casas <- prefijos_casas[!grepl("Equipo|Goles|Promedio_", prefijos_casas)]
extras <- c("Promedio_Apertura", "Pinnacle_Cierre")
prefijos_casas <- unique(c(prefijos_casas, extras[extras %in% gsub("_(Local|Empate|Visitante)$", "", names(base_datos))]))

# 4. APLICACIÓN DE CÁLCULOS POR OPERADOR
# ------------------------------------------------------------------------------
base_probabilidades <- base_datos

for (casa in prefijos_casas) {
  col_H <- paste0(casa, "_Local")
  col_D <- paste0(casa, "_Empate")
  col_A <- paste0(casa, "_Visitante")
  
  if(!(col_H %in% names(base_probabilidades))) next
  
  resultados_temp <- matrix(nrow = nrow(base_probabilidades), ncol = 7)
  
  for (i in 1:nrow(base_probabilidades)) {
    resultados_temp[i, ] <- calcular_probabilidades(
      base_probabilidades[[col_H]][i], 
      base_probabilidades[[col_D]][i], 
      base_probabilidades[[col_A]][i]
    )
  }
  
  base_probabilidades[[paste0(casa, "_Margen")]] <- resultados_temp[, 1]
  base_probabilidades[[paste0(casa, "_P_Local_Mult")]] <- resultados_temp[, 2]
  base_probabilidades[[paste0(casa, "_P_Empate_Mult")]] <- resultados_temp[, 3]
  base_probabilidades[[paste0(casa, "_P_Vis_Mult")]] <- resultados_temp[, 4]
  base_probabilidades[[paste0(casa, "_P_Local_Pot")]] <- resultados_temp[, 5]
  base_probabilidades[[paste0(casa, "_P_Empate_Pot")]] <- resultados_temp[, 6]
  base_probabilidades[[paste0(casa, "_P_Vis_Pot")]] <- resultados_temp[, 7]
  
  base_probabilidades[[col_H]] <- NULL
  base_probabilidades[[col_D]] <- NULL
  base_probabilidades[[col_A]] <- NULL
}

# 5. REPORTE COMPARATIVO Y EXPORTACIÓN DE ESTADÍSTICAS DE MARGEN (SENSIBILIDAD)
# ------------------------------------------------------------------------------
cat("\n========================================================================\n")
cat("          ANÁLISIS COMPARATIVO DE MÉTODOS DE DESMARGEN                  \n")
cat("========================================================================\n")

# Generamos una tabla resumen con las características del margen por operador
resumen_margenes <- data.frame(Casa_Apuestas = character(), Margen_Promedio_Pct = numeric(), stringsAsFactors = FALSE)

for(casa in prefijos_casas) {
  col_m <- paste0(casa, "_Margen")
  if(col_m %in% names(base_probabilidades)) {
    m_prom <- mean(base_probabilidades[[col_m]], na.rm = TRUE) * 100
    resumen_margenes <- rbind(resumen_margenes, data.frame(Casa_Apuestas = casa, Margen_Promedio_Pct = round(m_prom, 2)))
  }
}

print(resumen_margenes)
# Exportar la caracterización del margen directamente a la carpeta ordenada
write.csv(resumen_margenes, file = file.path(dir_tablas, "Caracterizacion_Margenes_Operadores.csv"), row.names = FALSE)

casa_ejemplo <- ifelse("Bet365" %in% prefijos_casas, "Bet365", prefijos_casas[1])
col_mult <- paste0(casa_ejemplo, "_P_Local_Mult")
col_pot  <- paste0(casa_ejemplo, "_P_Local_Pot")

if(col_mult %in% names(base_probabilidades)) {
  diferencia_promedio <- mean(abs(base_probabilidades[[col_mult]] - base_probabilidades[[col_pot]]), na.rm = TRUE)
  max_diferencia <- max(abs(base_probabilidades[[col_mult]] - base_probabilidades[[col_pot]]), na.rm = TRUE)
  
  cat(sprintf("\nAnalizando el operador de referencia: %s\n", casa_ejemplo))
  cat(sprintf("Diferencia promedio entre Multiplicativo y Potencia (Prob. Local): %.4f\n", diferencia_promedio))
  cat(sprintf("Diferencia MÁXIMA observada en un partido: %.4f\n", max_diferencia))
  cat("\nConclusión visible: El método multiplicativo y el logarítmico difieren.\n")
  cat("El método de potencia castiga menos al favorito, justificando su elección técnica.\n")
}

# 6. LIMPIEZA FINAL: EXCLUSIÓN DE COLUMNAS MULTIPLICATIVAS PARA EL MOTOR MAESTRO
# ------------------------------------------------------------------------------
columnas_multiplicativas <- grep("_Mult$", names(base_probabilidades), value = TRUE)
base_final_exportar <- base_probabilidades[, !(names(base_probabilidades) %in% columnas_multiplicativas)]

cat("\n========================================================================\n")
cat("          REPORTE DE PREPARACIÓN DE LA BASE DE DATOS FINAL              \n")
cat("========================================================================\n")
cat("Se procesaron las probabilidades para los operadores detectados.\n")
cat(sprintf("Se eliminaron %d columnas multiplicativas redundantes.\n", length(columnas_multiplicativas)))
cat("Dimensiones finales de la base limpia:", ncol(base_final_exportar), "variables.\n")

# 7. EXPORTACIÓN DE LA BASE DEFINITIVA
# ------------------------------------------------------------------------------
ruta_exportacion <- gsub("\\.csv$", "_probabilidades_VAR.csv", ruta, ignore.case = TRUE)
write.csv(base_final_exportar, file = ruta_exportacion, row.names = FALSE)

cat("\n========================================================================\n")
cat(">> EXPORTACIÓN EXITOSA:\n")
cat("La base de datos definitiva se guardó correctamente en:\n")
cat(ruta_exportacion, "\n")
cat("Y la tabla resumen de márgenes se organizó en tu subcarpeta:\n")
cat(file.path(dir_tablas, "Caracterizacion_Margenes_Operadores.csv"), "\n")
cat("========================================================================\n")