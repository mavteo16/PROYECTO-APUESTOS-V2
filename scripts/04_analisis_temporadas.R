# ==============================================================================
# 04_ANALISIS_TEMPORADAS.R
# ==============================================================================
# Proyecto : ¿Están bien calibradas las casas de apuestas?
# Curso    : Estadística Industrial · Universidad del Magdalena · 2026-II
# Autores  : Mateo Valencia, Samuel Lopez, Camilo Henriquez,
#            Valeria De Jesus Gutierrez Nuñez
#
# Qué hace : Evalúa cada operador en cada temporada con las probabilidades del método de
#            potencia: Brier, log-loss y error de calibración (ECE, 10 intervalos), y
#            dibuja la curva de fiabilidad de cada operador.
# Lee      : datos/Base_Analitica_Entregable_1.csv
# Genera   : Resultados/Tablas_CSV/1_Precision_Predictiva_<temporada>.csv
#            Resultados/Tablas_CSV/2_Fiabilidad_Estructural_<temporada>.csv
#            Resultados/Graficos_Globales/Grafico_1 y Grafico_2 por temporada
#            Resultados/Curvas_Fiabilidad/<temporada>/Curva_Fiabilidad_<operador>.png
# Antes    : 03_margen_probabilidades.R
# Después  : 05_comparativo_VAR.R
# ==============================================================================

# 0. CONFIGURACIÓN (rutas y parámetros centralizados en 00_configuracion.R)
# ------------------------------------------------------------------------------
if (!exists("CONFIG_CARGADA")) {
  source(if (file.exists("scripts/00_configuracion.R")) "scripts/00_configuracion.R" else "00_configuracion.R",
         encoding = "UTF-8")
}

# 1. RUTAS Y CARPETAS (definidas en 00_configuracion.R)
# ------------------------------------------------------------------------------
# dir_tablas, dir_graf_glob, dir_curvas_1718, dir_curvas_1819 y ruta_resultados

# Paleta de colores institucional y profesional
col_H <- "#2b6a9e"    # Azul Acero (Local)
col_D <- "#e07a5f"    # Terracota (Empate)
col_A <- "#3d5a80"    # Azul Marino Oscuro (Visitante)

# 2. LECTURA Y SEPARACIÓN TEMPORAL (PRE Y POST VAR)
# ------------------------------------------------------------------------------
cat("\n========================================================================\n")
cat(">> INICIANDO MOTOR DE ANÁLISIS UNIFICADO Y ORGANIZACIÓN DE REPOSITORIO\n")
cat("========================================================================\n")

base_datos <- read.csv(ruta_base_analitica, stringsAsFactors = FALSE)

# Partición estructural segura (380 partidos por temporada inglesa/española estándar)
base_1718 <- base_datos[1:partidos_por_temporada, ]
base_1819 <- base_datos[(partidos_por_temporada + 1):nrow(base_datos), ]

# 3. DEFINICIÓN DE FUNCIONES MATEMÁTICAS AUXILIARES
# ------------------------------------------------------------------------------
calcular_ece <- function(probabilidades, observaciones, num_bins = 10) {
  breaks <- seq(0, 1, length.out = num_bins + 1)
  bin_indices <- cut(probabilidades, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  esperado <- tapply(probabilidades, bin_indices, mean, na.rm = TRUE)
  observado <- tapply(observaciones, bin_indices, mean, na.rm = TRUE)
  conteo <- tapply(observaciones, bin_indices, length)
  validos <- !is.na(esperado)
  return(sum(abs(esperado[validos] - observado[validos]) * (conteo[validos] / sum(conteo[validos]))))
}

# 4. FUNCIÓN MAESTRA DE PROCESAMIENTO Y EXPORTACIÓN ESTRUCTURADA
# ------------------------------------------------------------------------------
ejecutar_analisis_temporada <- function(datos, id_temporada, etiqueta_titulo, dir_destino_curvas) {
  
  # A. Binarización
  datos$Obs_Local  <- ifelse(datos$Resultado_Final == "H", 1, 0)
  datos$Obs_Empate <- ifelse(datos$Resultado_Final == "D", 1, 0)
  datos$Obs_Vis    <- ifelse(datos$Resultado_Final == "A", 1, 0)
  
  cols_pot <- grep("_P_Local_Pot$", names(datos), value = TRUE)
  casas <- gsub("_P_Local_Pot$", "", cols_pot)
  
  tabla_precision <- data.frame(Casa_Apuestas = character(), Brier_Score = numeric(), Log_Loss = numeric(), stringsAsFactors = FALSE)
  tabla_fiabilidad <- data.frame(Casa_Apuestas = character(), ECE_Local = numeric(), ECE_Empate = numeric(), ECE_Vis = numeric(), ECE_Prom = numeric(), stringsAsFactors = FALSE)
  
  # B. Cálculos por Casa de Apuestas
  for (casa in casas) {
    p_H <- datos[[paste0(casa, "_P_Local_Pot")]]
    p_D <- datos[[paste0(casa, "_P_Empate_Pot")]]
    p_A <- datos[[paste0(casa, "_P_Vis_Pot")]]
    
    filtro <- complete.cases(p_H, p_D, p_A)
    N <- sum(filtro)
    
    # Brier & Log-Loss
    brier <- sum((p_H[filtro] - datos$Obs_Local[filtro])^2 + (p_D[filtro] - datos$Obs_Empate[filtro])^2 + (p_A[filtro] - datos$Obs_Vis[filtro])^2) / N
    eps <- 1e-15
    p_H_log <- pmax(pmin(p_H[filtro], 1 - eps), eps); p_D_log <- pmax(pmin(p_D[filtro], 1 - eps), eps); p_A_log <- pmax(pmin(p_A[filtro], 1 - eps), eps)
    logloss <- -sum(datos$Obs_Local[filtro] * log(p_H_log) + datos$Obs_Empate[filtro] * log(p_D_log) + datos$Obs_Vis[filtro] * log(p_A_log)) / N
    
    tabla_precision <- rbind(tabla_precision, data.frame(Casa_Apuestas = casa, Brier_Score = round(brier, 5), Log_Loss = round(logloss, 5)))
    
    # ECE
    ece_H <- calcular_ece(p_H[filtro], datos$Obs_Local[filtro])
    ece_D <- calcular_ece(p_D[filtro], datos$Obs_Empate[filtro])
    ece_A <- calcular_ece(p_A[filtro], datos$Obs_Vis[filtro])
    
    tabla_fiabilidad <- rbind(tabla_fiabilidad, data.frame(
      Casa_Apuestas = casa, ECE_Local = round(ece_H, 4), ECE_Empate = round(ece_D, 4), ECE_Vis = round(ece_A, 4), ECE_Prom = round(mean(c(ece_H, ece_D, ece_A)), 4)
    ))
  }
  
  # Ordenar Tablas
  tabla_precision <- tabla_precision[order(tabla_precision$Brier_Score), ]; row.names(tabla_precision) <- NULL
  tabla_fiabilidad <- tabla_fiabilidad[order(tabla_fiabilidad$ECE_Prom), ]; row.names(tabla_fiabilidad) <- NULL
  
  # C. Reporte en consola y Exportación de CSV a Tablas_CSV
  cat(sprintf("\n=== RESULTADOS: %s ===\n", etiqueta_titulo))
  cat(">> PRECISIÓN PREDICTIVA (Menor Brier = Mejor):\n"); print(tabla_precision)
  cat("\n>> FIABILIDAD ESTRUCTURAL (Menor ECE = Mejor):\n"); print(tabla_fiabilidad)
  
  write.csv(tabla_precision, file = file.path(dir_tablas, paste0("1_Precision_Predictiva_", id_temporada, ".csv")), row.names = FALSE)
  write.csv(tabla_fiabilidad, file = file.path(dir_tablas, paste0("2_Fiabilidad_Estructural_", id_temporada, ".csv")), row.names = FALSE)
  
  # D. Gráfico 1: Cleveland Dot Plot (Brier) -> a Graficos_Globales
  tp_grafico <- tabla_precision[order(tabla_precision$Brier_Score, decreasing = TRUE), ]
  xmin <- min(tp_grafico$Brier_Score) - 0.0005; xmax <- max(tp_grafico$Brier_Score) + 0.0005
  
  png(file.path(dir_graf_glob, paste0("Grafico_1_Precision_Brier_", id_temporada, ".png")), width = 1100, height = 600, res = 120)
  par(mar = c(5, 12, 4, 2), bg = "#fcfcfc")
  dotchart(tp_grafico$Brier_Score, labels = tp_grafico$Casa_Apuestas, cex = 1.1, pch = 16, col = col_H, pt.cex = 1.8, xlim = c(xmin, xmax),
           main = paste("Precisión Predictiva Global (Brier Score) -", etiqueta_titulo), xlab = "Brier Score")
  abline(v = seq(xmin, xmax, by = 0.001), col = "gray90", lty = 2)
  dev.off()
  
  # E. Gráfico 2: Barras Agrupadas ECE -> a Graficos_Globales
  tf_grafico <- tabla_fiabilidad[order(tabla_fiabilidad$ECE_Prom, decreasing = FALSE), ]
  matriz_ece <- t(as.matrix(tf_grafico[, c("ECE_Local", "ECE_Empate", "ECE_Vis")]))
  colnames(matriz_ece) <- tf_grafico$Casa_Apuestas
  
  png(file.path(dir_graf_glob, paste0("Grafico_2_Fiabilidad_ECE_Comparativo_", id_temporada, ".png")), width = 1100, height = 650, res = 120)
  par(mar = c(12, 5, 4, 2), bg = "#fcfcfc")
  
  barplot(matriz_ece, beside = TRUE, col = c(col_H, col_D, col_A), ylim = c(0, max(matriz_ece) * 1.15),
          axisnames = FALSE, border = NA, main = paste("Análisis de Calibración Estructural Multiclase (ECE)\n", etiqueta_titulo), ylab = "Error Esperado (Proporción)")
  grid(nx = NA, ny = NULL, col = "gray85", lty = 2)
  barplot(matriz_ece, beside = TRUE, col = c(col_H, col_D, col_A), add = TRUE, border = NA, las = 2, cex.names = 0.95, axisnames = TRUE, axes = FALSE)
  legend("topleft", legend = c("Error en Local", "Error en Empate", "Error en Visitante"), fill = c(col_H, col_D, col_A), border = NA, bty = "n", cex = 1.1)
  dev.off()
  
  # F. Gráfico 3: Curvas Individuales Widescreen -> a su subcarpeta específica de Curvas
  for (casa in casas) {
    p_H <- datos[[paste0(casa, "_P_Local_Pot")]]; p_D <- datos[[paste0(casa, "_P_Empate_Pot")]]; p_A <- datos[[paste0(casa, "_P_Vis_Pot")]]
    
    calc_b <- function(p, o) {
      b <- seq(0, 1, length.out = 11); idx <- cut(p, breaks = b, include.lowest = TRUE, labels = FALSE)
      df <- data.frame(Bin = 1:10, Esperado = NA, Observado = NA)
      df$Esperado[match(names(tapply(p, idx, mean, na.rm=T)), df$Bin)] <- tapply(p, idx, mean, na.rm=T)
      df$Observado[match(names(tapply(o, idx, mean, na.rm=T)), df$Bin)] <- tapply(o, idx, mean, na.rm=T)
      return(na.omit(df))
    }
    
    b_H <- calc_b(p_H, datos$Obs_Local); b_D <- calc_b(p_D, datos$Obs_Empate); b_A <- calc_b(p_A, datos$Obs_Vis)
    
    png(file.path(dir_destino_curvas, paste0("Curva_Fiabilidad_", casa, "_", id_temporada, ".png")), width = 1100, height = 650, res = 120)
    par(bg = "#fcfcfc", mar = c(5, 5, 4, 2))
    
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "Probabilidad Asignada", ylab = "Frecuencia Empírica",
         main = paste("Curva de Fiabilidad Estructural -", casa, "(", etiqueta_titulo, ")"), cex.main = 1.3, cex.lab = 1.1)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#f8f9fa", border = NA)
    grid(col = "#e9ecef", lty = 1, lwd = 1.5)
    abline(a = 0, b = 1, lty = 2, col = "#6c757d", lwd = 2)
    
    lines(b_H$Esperado, b_H$Observado, col = col_H, lwd = 2.5, type = "b", pch = 16, cex = 1.4)
    lines(b_D$Esperado, b_D$Observado, col = col_D, lwd = 2.5, type = "b", pch = 15, cex = 1.4)
    lines(b_A$Esperado, b_A$Observado, col = col_A, lwd = 2.5, type = "b", pch = 17, cex = 1.4)
    legend("topleft", legend = c("Local", "Empate", "Visitante", "Calibración Perfecta"),
           col = c(col_H, col_D, col_A, "#6c757d"), lty = c(1,1,1,2), pch = c(16,15,17,NA), lwd = 2.5, bty = "n", cex = 1.05)
    dev.off()
  }
}

# 5. EJECUCIÓN SECUENCIAL AUTOMATIZADA
# ------------------------------------------------------------------------------
ejecutar_analisis_temporada(datos = base_1718, id_temporada = "1718", etiqueta_titulo = "Temporada 17/18 (Pre-VAR)", dir_destino_curvas = dir_curvas_1718)
ejecutar_analisis_temporada(datos = base_1819, id_temporada = "1819", etiqueta_titulo = "Temporada 18/19 (Post-VAR)", dir_destino_curvas = dir_curvas_1819)

# 6. REPORTE FINAL
# ------------------------------------------------------------------------------
cat("\n========================================================================\n")
cat(">> EJECUCIÓN TOTAL Y ORGANIZACIÓN DE REPOSITORIO FINALIZADA CON ÉXITO\n")
cat("========================================================================\n")
cat("Tus resultados se han distribuido de forma profesional en:\n")
cat(ruta_resultados, "\n")
cat("  ├── Tablas_CSV/\n")
cat("  ├── Graficos_Globales/\n")
cat("  └── Curvas_Fiabilidad/\n")
cat("       ├── 1718_Pre_VAR/\n")
cat("       └── 1819_Post_VAR/\n")
cat("========================================================================\n")