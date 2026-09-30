# ==============================================================================
# MOTOR MAESTRO DE ANÁLISIS PREDICTIVO Y FIABILIDAD (VERSIÓN 6.0 - ORGANIZADO)
# ==============================================================================

# 1. CONFIGURACIÓN DE RUTAS Y JERARQUÍA DE SUBCARPETAS
# ------------------------------------------------------------------------------
ruta_cruda <- "C:/Users/Invitadou/Downloads/Base_Unificada_Limpia_VAR.csv"
ruta_cruda <- gsub("\\\\", "/", ruta_cruda)
ruta_cruda <- gsub("\"", "", ruta_cruda)

# Directorio Maestro de Resultados
ruta_resultados <- "C:/Users/Invitadou/Desktop/PROYECTO-APUESTAS/Resultados"
ruta_resultados <- gsub("\\\\", "/", ruta_resultados)

# Creación de la jerarquía profesional de subcarpetas
dir_tablas      <- file.path(ruta_resultados, "Tablas_CSV")
dir_graf_glob   <- file.path(ruta_resultados, "Graficos_Globales")
dir_curvas_1718 <- file.path(ruta_resultados, "Curvas_Fiabilidad", "1718_Pre_VAR")
dir_curvas_1819 <- file.path(ruta_resultados, "Curvas_Fiabilidad", "1819_Post_VAR")

directorios_necesarios <- c(dir_tablas, dir_graf_glob, dir_curvas_1718, dir_curvas_1819)
for (d in directorios_necesarios) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

# Paleta de colores institucional y profesional
col_H    <- "#2b6a9e"    # Azul Acero (Local)
col_D    <- "#e07a5f"    # Terracota (Empate)
col_A    <- "#3d5a80"    # Azul Marino Oscuro (Visitante)
col_pre  <- "#2b6a9e"    # Azul (Pre-VAR)
col_post <- "#e07a5f"    # Terracota (Post-VAR)

# 2. LECTURA Y SEPARACIÓN ESTRUCTURAL (380 PARTIDOS POR TEMPORADA)
# ------------------------------------------------------------------------------
cat("\n========================================================================\n")
cat(">> INICIANDO MOTOR DE ANÁLISIS Y ESTRUCTURACIÓN DE REPOSITORIO\n")
cat("========================================================================\n")

base_datos <- read.csv(ruta_cruda, stringsAsFactors = FALSE)

# Partición estructural exacta
base_1718 <- base_datos[1:380, ]
base_1819 <- base_datos[381:nrow(base_datos), ]

preparar_verdad <- function(df) {
  df$Obs_Local  <- ifelse(df$Resultado_Final == "H", 1, 0)
  df$Obs_Empate <- ifelse(df$Resultado_Final == "D", 1, 0)
  df$Obs_Vis    <- ifelse(df$Resultado_Final == "A", 1, 0)
  return(df)
}

base_1718 <- preparar_verdad(base_1718)
base_1819 <- preparar_verdad(base_1819)

cols_pot <- grep("_Local$", names(base_1718), value = TRUE)
casas <- gsub("_Local$", "", cols_pot)

# 3. FUNCIONES MATEMÁTICAS AUXILIARES
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

calcular_metricas <- function(datos) {
  res_prec <- data.frame(Casa_Apuestas = character(), Brier_Score = numeric(), Log_Loss = numeric(), stringsAsFactors = FALSE)
  res_fiab <- data.frame(Casa_Apuestas = character(), ECE_Local = numeric(), ECE_Empate = numeric(), ECE_Vis = numeric(), ECE_Prom = numeric(), stringsAsFactors = FALSE)
  
  for (casa in casas) {
    p_H <- datos[[paste0(casa, "_Local")]]
    p_D <- datos[[paste0(casa, "_Empate")]]
    p_A <- datos[[paste0(casa, "_Visitante")]]
    
    filtro <- complete.cases(p_H, p_D, p_A)
    N <- sum(filtro)
    
    # Brier & Log-Loss
    brier <- sum((p_H[filtro] - datos$Obs_Local[filtro])^2 + 
                   (p_D[filtro] - datos$Obs_Empate[filtro])^2 + 
                   (p_A[filtro] - datos$Obs_Vis[filtro])^2) / N
    
    eps <- 1e-15
    p_H_log <- pmax(pmin(p_H[filtro], 1 - eps), eps)
    p_D_log <- pmax(pmin(p_D[filtro], 1 - eps), eps)
    p_A_log <- pmax(pmin(p_A[filtro], 1 - eps), eps)
    logloss <- -sum(datos$Obs_Local[filtro] * log(p_H_log) + 
                      datos$Obs_Empate[filtro] * log(p_D_log) + 
                      datos$Obs_Vis[filtro] * log(p_A_log)) / N
    
    res_prec <- rbind(res_prec, data.frame(Casa_Apuestas = casa, Brier_Score = round(brier, 5), Log_Loss = round(logloss, 5)))
    
    # ECE
    ece_H <- calcular_ece(p_H[filtro], datos$Obs_Local[filtro])
    ece_D <- calcular_ece(p_D[filtro], datos$Obs_Empate[filtro])
    ece_A <- calcular_ece(p_A[filtro], datos$Obs_Vis[filtro])
    
    res_fiab <- rbind(res_fiab, data.frame(
      Casa_Apuestas = casa, ECE_Local = round(ece_H, 4), ECE_Empate = round(ece_D, 4), ECE_Vis = round(ece_A, 4), ECE_Prom = round(mean(c(ece_H, ece_D, ece_A)), 4)
    ))
  }
  
  res_prec <- res_prec[order(res_prec$Brier_Score), ]; row.names(res_prec) <- NULL
  res_fiab <- res_fiab[order(res_fiab$ECE_Prom), ]; row.names(res_fiab) <- NULL
  
  return(list(prec = res_prec, fiab = res_fiab))
}

met_1718 <- calcular_metricas(base_1718)
met_1819 <- calcular_metricas(base_1819)

# 4. FUNCIÓN MAESTRA DE PROCESAMIENTO Y EXPORTACIÓN ESTRUCTURADA
# ------------------------------------------------------------------------------
procesar_temporada <- function(datos, metrica_lista, id_temporada, etiqueta_titulo, dir_destino_curvas) {
  
  tabla_precision <- metrica_lista$prec
  tabla_fiabilidad <- metrica_lista$fiab
  
  # A. Exportación de Tablas CSV a subcarpeta Tablas_CSV
  cat(sprintf("\n=== RESULTADOS: %s ===\n", etiqueta_titulo))
  cat(">> PRECISIÓN PREDICTIVA:\n"); print(tabla_precision)
  cat("\n>> FIABILIDAD ESTRUCTURAL:\n"); print(tabla_fiabilidad)
  
  write.csv(tabla_precision, file = file.path(dir_tablas, paste0("1_Precision_Predictiva_", id_temporada, ".csv")), row.names = FALSE)
  write.csv(tabla_fiabilidad, file = file.path(dir_tablas, paste0("2_Fiabilidad_Estructural_", id_temporada, ".csv")), row.names = FALSE)
  
  # B. Gráfico Global 1: Cleveland Dot Plot (Brier) -> a Graficos_Globales
  tp_grafico <- tabla_precision[order(tabla_precision$Brier_Score, decreasing = TRUE), ]
  xmin <- min(tp_grafico$Brier_Score) - 0.0005; xmax <- max(tp_grafico$Brier_Score) + 0.0005
  
  png(file.path(dir_graf_glob, paste0("Grafico_1_Precision_Brier_", id_temporada, ".png")), width = 1100, height = 600, res = 120)
  par(mar = c(5, 12, 4, 2), bg = "#fcfcfc")
  dotchart(tp_grafico$Brier_Score, labels = tp_grafico$Casa_Apuestas, cex = 1.1, pch = 16, col = col_H, pt.cex = 1.8, xlim = c(xmin, xmax),
           main = paste("Precisión Predictiva Global (Brier Score) -", etiqueta_titulo), xlab = "Brier Score")
  abline(v = seq(xmin, xmax, by = 0.001), col = "gray90", lty = 2)
  dev.off()
  
  # C. Gráfico Global 2: Barras Agrupadas ECE -> a Graficos_Globales
  tf_grafico <- tabla_fiabilidad[order(tabla_fiabilidad$ECE_Prom, decreasing = FALSE), ]
  matriz_ece <- t(as.matrix(tf_grafico[, c("ECE_Local", "ECE_Empate", "ECE_Vis")]))
  colnames(matriz_ece) <- tf_grafico$Casa_Apuestas
  
  png(file.path(dir_graf_glob, paste0("Grafico_2_Fiabilidad_ECE_Comparativo_", id_temporada, ".png")), width = 1100, height = 650, res = 120)
  par(mar = c(12, 5, 4, 2), bg = "#fcfcfc")
  
  barplot(matriz_ece, beside = TRUE, col = c(col_H, col_D, col_A), ylim = c(0, max(matriz_ece) * 1.15),
          axisnames = FALSE, border = NA, main = paste("Análisis de Calibración Estructural Multiclase (ECE)\n", etiqueta_titulo), ylab = "Error Esperado (Proporción)")
  grid(nx = NULL, ny = NULL, col = "gray85", lty = 2)
  barplot(matriz_ece, beside = TRUE, col = c(col_H, col_D, col_A), add = TRUE, border = NA, las = 2, cex.names = 0.95, axisnames = TRUE, axes = FALSE)
  legend("topleft", legend = c("Error en Local", "Error en Empate", "Error en Visitante"), fill = c(col_H, col_D, col_A), border = NA, bty = "n", cex = 1.1)
  dev.off()
  
  # D. Curvas de Fiabilidad Individuales -> a su subcarpeta específica de Curvas
  for (casa in casas) {
    p_H <- datos[[paste0(casa, "_Local")]]; p_D <- datos[[paste0(casa, "_Empate")]]; p_A <- datos[[paste0(casa, "_Visitante")]]
    
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

# 5. EJECUCIÓN PARA AMBAS TEMPORADAS
# ------------------------------------------------------------------------------
procesar_temporada(base_1718, met_1718, "1718", "Temporada 17/18 (Pre-VAR)", dir_curvas_1718)
procesar_temporada(base_1819, met_1819, "1819", "Temporada 18/19 (Post-VAR)", dir_curvas_1819)

# 6. GENERACIÓN DE GRÁFICOS COMPARATIVOS (EFECTO VAR) -> a Graficos_Globales
# ------------------------------------------------------------------------------
delta_prec <- merge(met_1718$prec, met_1819$prec, by = "Casa_Apuestas", suffixes = c("_pre", "_post"))
delta_prec$Delta_Brier <- delta_prec$Brier_Score_post - delta_prec$Brier_Score_pre
delta_prec <- delta_prec[order(delta_prec$Delta_Brier, decreasing = TRUE), ]

delta_fiab <- merge(met_1718$fiab, met_1819$fiab, by = "Casa_Apuestas", suffixes = c("_pre", "_post"))
delta_fiab$Delta_ECE_Prom <- delta_fiab$ECE_Prom_post - delta_fiab$ECE_Prom_pre
delta_fiab <- delta_fiab[order(delta_fiab$Delta_ECE_Prom, decreasing = TRUE), ]

write.csv(delta_prec, file.path(dir_tablas, "Tabla_Delta_Precision_VAR.csv"), row.names = FALSE)
write.csv(delta_fiab, file.path(dir_tablas, "Tabla_Delta_Fiabilidad_VAR.csv"), row.names = FALSE)

# Gráfico A: Mancuernas Brier
dp <- delta_prec[order(delta_prec$Brier_Score_pre, decreasing = FALSE), ]
y_coords <- 1:nrow(dp)
xlims <- c(min(dp$Brier_Score_pre, dp$Brier_Score_post) - 0.005, max(dp$Brier_Score_pre, dp$Brier_Score_post) + 0.005)

png(file.path(dir_graf_glob, "Grafico_A_Impacto_Brier_Mancuernas.png"), width = 1100, height = 650, res = 120)
par(mar = c(5, 12, 4, 2), bg = "#fcfcfc")
plot(0, 0, type = "n", xlim = xlims, ylim = c(0.5, nrow(dp) + 0.5), yaxt = "n", 
     xlab = "Brier Score (Valores mayores indican más error)", ylab = "", 
     main = "Colapso de Precisión Predictiva (Efecto VAR)\nDesplazamiento del Error Cuadrático Medio por Operador")
grid(nx = NULL, ny = NA, col = "gray85", lty = 2)
axis(2, at = y_coords, labels = dp$Casa_Apuestas, las = 2, tick = FALSE, cex.axis = 0.95)

segments(dp$Brier_Score_pre, y_coords, dp$Brier_Score_post, y_coords, col = "gray75", lwd = 4)
points(dp$Brier_Score_pre, y_coords, col = col_pre, pch = 16, cex = 2)
points(dp$Brier_Score_post, y_coords, col = col_post, pch = 16, cex = 2)

legend("topleft", legend = c("17/18 (Pre-VAR)", "18/19 (Post-VAR)", "Magnitud de Degradación"), 
       col = c(col_pre, col_post, "gray75"), pch = c(16, 16, NA), lty = c(NA, NA, 1), lwd = c(NA, NA, 4), bty = "n")
dev.off()

# Gráfico B: Barras Divergentes ECE
df_div <- delta_fiab[order(delta_fiab$Delta_ECE_Prom, decreasing = FALSE), ]
col_div <- ifelse(df_div$Delta_ECE_Prom > 0, col_post, "#81b29a") 

png(file.path(dir_graf_glob, "Grafico_B_Impacto_ECE_Divergente.png"), width = 1100, height = 650, res = 120)
par(mar = c(5, 12, 4, 2), bg = "#fcfcfc")
barplot(df_div$Delta_ECE_Prom, names.arg = df_div$Casa_Apuestas, horiz = TRUE, las = 2, 
        col = col_div, border = NA, 
        main = "Pérdida de Calibración Estructural del Mercado\nCrecimiento Neto del Error Esperado de Calibración (Delta ECE)", 
        xlab = "Aumento Porcentual del Error (Delta)")
grid(nx = NULL, ny = NA, col = "gray85", lty = 2)
barplot(df_div$Delta_ECE_Prom, names.arg = df_div$Casa_Apuestas, horiz = TRUE, las = 2, col = col_div, border = NA, add = TRUE, axes=FALSE)
abline(v = 0, lwd = 2, col = "black")
dev.off()

# Gráfico C: Superposición del Mercado (Promedio Apertura - Empate)
calc_b_sup <- function(p, o) {
  b <- seq(0, 1, length.out = 11); idx <- cut(p, breaks = b, include.lowest = TRUE, labels = FALSE)
  df <- data.frame(Bin = 1:10, Esperado = NA, Observado = NA)
  df$Esperado[match(names(tapply(p, idx, mean, na.rm=T)), df$Bin)] <- tapply(p, idx, mean, na.rm=T)
  df$Observado[match(names(tapply(o, idx, mean, na.rm=T)), df$Bin)] <- tapply(o, idx, mean, na.rm=T)
  return(na.omit(df))
}

p_pre <- base_1718$Promedio_Apertura_Empate; o_pre <- base_1718$Obs_Empate
p_post <- base_1819$Promedio_Apertura_Empate; o_post <- base_1819$Obs_Empate

curva_pre <- calc_b_sup(p_pre, o_pre)
curva_post <- calc_b_sup(p_post, o_post)

png(file.path(dir_graf_glob, "Grafico_C_Superposicion_Mercado_Empate.png"), width = 1100, height = 650, res = 120)
par(mar = c(5, 5, 4, 2), bg = "#fcfcfc")

plot(NULL, xlim = c(0, 0.45), ylim = c(0, 0.45), 
     xlab = "Probabilidad Asignada al Empate (Promedio Apertura)", ylab = "Frecuencia Empírica Observada",
     main = "Descalibración del Empate: Curva de Frontera del Mercado\n(Pre-VAR vs Post-VAR)", 
     cex.main = 1.3, cex.lab = 1.1)

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#f8f9fa", border = NA)
grid(col = "#e9ecef", lty = 1, lwd = 1.5)
abline(a = 0, b = 1, lty = 2, col = "#6c757d", lwd = 2)

lines(curva_pre$Esperado, curva_pre$Observado, col = col_pre, lwd = 3, type = "b", pch = 16, cex = 1.6)
lines(curva_post$Esperado, curva_post$Observado, col = col_post, lwd = 3, type = "b", pch = 17, cex = 1.6)

legend("topleft", legend = c("Mercado 17/18 (Pre-VAR)", "Mercado 18/19 (Post-VAR)", "Calibración Perfecta"),
       col = c(col_pre, col_post, "#6c757d"), lty = c(1,1,2), pch = c(16,17,NA), lwd = 3, bty = "n", cex = 1.1)
dev.off()

cat("\n========================================================================\n")
cat(">> EJECUCIÓN MAESTRA Y ORDENAMIENTO DE CARPETAS FINALIZADO CON ÉXITO\n")
cat("========================================================================\n")
cat("Tus resultados limpios y estructurados se encuentran en:\n")
cat(ruta_resultados, "\n")
cat("  ├── Tablas_CSV/\n")
cat("  ├── Graficos_Globales/\n")
cat("  └── Curvas_Fiabilidad/\n")
cat("       ├── 1718_Pre_VAR/\n")
cat("       └── 1819_Post_VAR/\n")
cat("========================================================================\n")