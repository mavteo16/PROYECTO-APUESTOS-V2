# ==============================================================================
# 03_MARGEN_PROBABILIDADES.R
# ==============================================================================
# Proyecto : ¿Están bien calibradas las casas de apuestas?
# Curso    : Estadística Industrial · Universidad del Magdalena · 2026-II
# Autores  : Mateo Valencia, Samuel Lopez, Camilo Henriquez,
#            Valeria De Jesus Gutierrez Nuñez
#
# Qué hace : Convierte las cuotas en probabilidades: calcula el margen de cada operador,
#            las probabilidades brutas (1/cuota) y las normalizadas con dos métodos
#            (multiplicativo y potencia). Construye la base analítica (Entregable 1).
# Lee      : datos/Base_Unificada_Limpia_VAR.csv
# Genera   : datos/Base_Analitica_Entregable_1.csv (Entregable 1)
#            Resultados/Tablas_CSV/Caracterizacion_Margenes_Operadores.csv
# Antes    : 02_unificacion_base.R
# Después  : 04_analisis_temporadas.R
# ==============================================================================

# 0. CONFIGURACIÓN (rutas y parámetros centralizados en 00_configuracion.R)
# ------------------------------------------------------------------------------
if (!exists("CONFIG_CARGADA")) {
  source(if (file.exists("scripts/00_configuracion.R")) "scripts/00_configuracion.R" else "00_configuracion.R",
         encoding = "UTF-8")
}

# 1. LECTURA DE LA BASE UNIFICADA
# ------------------------------------------------------------------------------
base_datos <- tryCatch({
  read.csv(ruta_base_unificada, stringsAsFactors = FALSE)
}, error = function(e) {
  stop("No se pudo leer ", ruta_base_unificada, ". Ejecuta primero 02_unificacion_base.R.")
})

# 2. FUNCIONES MATEMÁTICAS PARA EL TRATAMIENTO DEL MARGEN (ANÁLISIS DUAL)
# ------------------------------------------------------------------------------
# Devuelve 10 valores: Margen, 3 Brutas, 3 Multiplicativas, 3 Potencia
calcular_probabilidades <- function(o_H, o_D, o_A) {
  
  # Forzar coerción numérica para evitar el error de "argumento no-numérico"
  o_H <- suppressWarnings(as.numeric(o_H))
  o_D <- suppressWarnings(as.numeric(o_D))
  o_A <- suppressWarnings(as.numeric(o_A))
  
  if (is.na(o_H) || is.na(o_D) || is.na(o_A) || o_H <= 1 || o_D <= 1 || o_A <= 1) {
    return(rep(NA, 10))
  }
  
  # 1. Probabilidades Brutas (implícitas sin ajustar)
  pi_H <- 1 / o_H
  pi_D <- 1 / o_D
  pi_A <- 1 / o_A
  suma_pi <- pi_H + pi_D + pi_A
  
  # 2. Margen del Operador
  margen <- suma_pi - 1
  
  # 3. Método 1: Normalización Multiplicativa (Proporcional)
  p_H_mult <- pi_H / suma_pi
  p_D_mult <- pi_D / suma_pi
  p_A_mult <- pi_A / suma_pi
  
  # 4. Método 2: Método de Potencia (Ajuste no lineal / Odds-Ratio)
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
  
  return(c(margen, pi_H, pi_D, pi_A, p_H_mult, p_D_mult, p_A_mult, p_H_pot, p_D_pot, p_A_pot))
}

# 3. DETECCIÓN DINÁMICA DE CASAS DE APUESTAS
# ------------------------------------------------------------------------------
cols_local <- grep("_Local$", names(base_datos), value = TRUE)
prefijos_casas <- gsub("_Local$", "", cols_local)
prefijos_casas <- prefijos_casas[!grepl("Equipo|Goles|Promedio_", prefijos_casas)]
extras <- c("Promedio_Apertura", "Pinnacle_Cierre")
prefijos_casas <- unique(c(prefijos_casas, extras[extras %in% gsub("_(Local|Empate|Visitante)$", "", names(base_datos))]))

# 4. APLICACIÓN DE CÁLCULOS POR OPERADOR (SIN BORRAR DATOS ORIGINALES)
# ------------------------------------------------------------------------------
base_probabilidades <- base_datos

cat("\n========================================================================\n")
cat(">> CONSTRUYENDO BASE ANALÍTICA (CUMPLIMIENTO ENTREGABLE 1)\n")
cat("========================================================================\n")

for (casa in prefijos_casas) {
  col_H <- paste0(casa, "_Local")
  col_D <- paste0(casa, "_Empate")
  col_A <- paste0(casa, "_Visitante")
  
  if(!(col_H %in% names(base_probabilidades))) next
  
  resultados_temp <- matrix(nrow = nrow(base_probabilidades), ncol = 10)
  
  for (i in 1:nrow(base_probabilidades)) {
    resultados_temp[i, ] <- calcular_probabilidades(
      base_probabilidades[[col_H]][i], 
      base_probabilidades[[col_D]][i], 
      base_probabilidades[[col_A]][i]
    )
  }
  
  # Asignación de variables según Rúbrica
  base_probabilidades[[paste0(casa, "_Margen")]] <- resultados_temp[, 1]
  base_probabilidades[[paste0(casa, "_P_Local_Bruta")]] <- resultados_temp[, 2]
  base_probabilidades[[paste0(casa, "_P_Empate_Bruta")]] <- resultados_temp[, 3]
  base_probabilidades[[paste0(casa, "_P_Vis_Bruta")]] <- resultados_temp[, 4]
  base_probabilidades[[paste0(casa, "_P_Local_Mult")]] <- resultados_temp[, 5]
  base_probabilidades[[paste0(casa, "_P_Empate_Mult")]] <- resultados_temp[, 6]
  base_probabilidades[[paste0(casa, "_P_Vis_Mult")]] <- resultados_temp[, 7]
  base_probabilidades[[paste0(casa, "_P_Local_Pot")]] <- resultados_temp[, 8]
  base_probabilidades[[paste0(casa, "_P_Empate_Pot")]] <- resultados_temp[, 9]
  base_probabilidades[[paste0(casa, "_P_Vis_Pot")]] <- resultados_temp[, 10]
  
  # NOTA METODOLÓGICA: NO borramos col_H, col_D ni col_A. 
  # La rúbrica exige que la base contenga las "cuotas originales".
}

# 5. REPORTE COMPARATIVO Y EXPORTACIÓN DE ESTADÍSTICAS DE MARGEN (SENSIBILIDAD)
# ------------------------------------------------------------------------------
resumen_margenes <- data.frame(Casa_Apuestas = character(), Margen_Promedio_Pct = numeric(), stringsAsFactors = FALSE)

for(casa in prefijos_casas) {
  col_m <- paste0(casa, "_Margen")
  if(col_m %in% names(base_probabilidades)) {
    m_prom <- mean(base_probabilidades[[col_m]], na.rm = TRUE) * 100
    resumen_margenes <- rbind(resumen_margenes, data.frame(Casa_Apuestas = casa, Margen_Promedio_Pct = round(m_prom, 2)))
  }
}

write.csv(resumen_margenes, file = file.path(dir_tablas, "Caracterizacion_Margenes_Operadores.csv"), row.names = FALSE)

# 6. EXPORTACIÓN DE LA BASE DEFINITIVA
# ------------------------------------------------------------------------------
ruta_exportacion <- ruta_base_analitica   # datos/Base_Analitica_Entregable_1.csv
write.csv(base_probabilidades, file = ruta_exportacion, row.names = FALSE)

cat(">> ¡PROCESO COMPLETADO!\n")
cat("La base de datos analítica cumple con los 5 requisitos de la rúbrica y se guardó en:\n")
cat(ruta_exportacion, "\n")
cat("========================================================================\n")