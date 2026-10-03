# ==============================================================================
# 05_COMPARATIVO_VAR.R
# ==============================================================================
# Proyecto : ¿Están bien calibradas las casas de apuestas?
# Curso    : Estadística Industrial · Universidad del Magdalena · 2026-II
# Autores  : Mateo Valencia, Samuel Lopez, Camilo Henriquez,
#            Valeria De Jesus Gutierrez Nuñez
#
# Qué hace : Compara la temporada sin VAR (2017/18) con la temporada con VAR (2018/19):
#            cambio del Brier y del ECE por operador, y curvas de calibración del
#            mercado promedio superpuestas para local, empate y visitante.
# Lee      : datos/Base_Analitica_Entregable_1.csv
# Genera   : Resultados/Tablas_CSV/Tabla_Delta_Precision_VAR.csv y Tabla_Delta_Fiabilidad_VAR.csv
#            Resultados/Graficos_Globales/Grafico_A a Grafico_E
# Antes    : 04_analisis_temporadas.R
# Después  : reporte/Reporte-De-Calibracion.Rmd (lo ejecuta ejecutar_todo.R)
# ==============================================================================

# 0. CONFIGURACIÓN (rutas y parámetros centralizados en 00_configuracion.R)
# ------------------------------------------------------------------------------
if (!exists("CONFIG_CARGADA")) {
  source(if (file.exists("scripts/00_configuracion.R")) "scripts/00_configuracion.R" else "00_configuracion.R",
         encoding = "UTF-8")
}

# 1. RUTAS Y CARPETAS (definidas en 00_configuracion.R)
# ------------------------------------------------------------------------------
# dir_tablas y dir_graf_glob

# Paleta de colores para comparaciones (Azul=Pre-VAR, Rojo=Post-VAR)
col_pre  <- "#2b6a9e"   # Azul Acero
col_post <- "#e07a5f"   # Terracota

# 2. LECTURA Y SEPARACIÓN ESTRUCTURAL (380 PARTIDOS POR TEMPORADA)
# ------------------------------------------------------------------------------
cat("\n>> Iniciando cálculos comparativos (Delta) Pre-VAR vs Post-VAR...\n")
base_datos <- read.csv(ruta_base_analitica, stringsAsFactors = FALSE)

base_1718 <- base_datos[1:partidos_por_temporada, ]
base_1819 <- base_datos[(partidos_por_temporada + 1):nrow(base_datos), ]

preparar_verdad <- function(df) {
  df$Obs_Local  <- ifelse(df$Resultado_Final == "H", 1, 0)
  df$Obs_Empate <- ifelse(df$Resultado_Final == "D", 1, 0)
  df$Obs_Vis    <- ifelse(df$Resultado_Final == "A", 1, 0)
  return(df)
}

base_1718 <- preparar_verdad(base_1718)
base_1819 <- preparar_verdad(base_1819)

cols_pot <- grep("_P_Local_Pot$", names(base_1718), value = TRUE)
casas <- gsub("_P_Local_Pot$", "", cols_pot)

# 3. MOTOR DE CÁLCULO DE MÉTRICAS (INTERNALIZADO)
# ------------------------------------------------------------------------------
calcular_metricas <- function(datos) {
  res_prec <- data.frame(Casa_Apuestas = character(), Brier = numeric(), stringsAsFactors = FALSE)
  res_fiab <- data.frame(Casa_Apuestas = character(), ECE_Loc = numeric(), ECE_Emp = numeric(), ECE_Vis = numeric(), ECE_Prom = numeric(), stringsAsFactors = FALSE)
  
  calc_ece <- function(p, o) {
    b <- cut(p, breaks = seq(0, 1, length.out = 11), include.lowest = TRUE, labels = FALSE)
    esp <- tapply(p, b, mean, na.rm = TRUE); obs <- tapply(o, b, mean, na.rm = TRUE); cnt <- tapply(o, b, length)
    val <- !is.na(esp)
    sum(abs(esp[val] - obs[val]) * (cnt[val] / sum(cnt[val])))
  }
  
  for(c in casas) {
    p_H <- datos[[paste0(c, "_P_Local_Pot")]]; p_D <- datos[[paste0(c, "_P_Empate_Pot")]]; p_A <- datos[[paste0(c, "_P_Vis_Pot")]]
    f <- complete.cases(p_H, p_D, p_A); N <- sum(f)
    
    brier <- sum((p_H[f] - datos$Obs_Local[f])^2 + (p_D[f] - datos$Obs_Empate[f])^2 + (p_A[f] - datos$Obs_Vis[f])^2) / N
    res_prec <- rbind(res_prec, data.frame(Casa_Apuestas = c, Brier = brier))
    
    ece_H <- calc_ece(p_H[f], datos$Obs_Local[f]); ece_D <- calc_ece(p_D[f], datos$Obs_Empate[f]); ece_A <- calc_ece(p_A[f], datos$Obs_Vis[f])
    res_fiab <- rbind(res_fiab, data.frame(Casa_Apuestas = c, ECE_Loc = ece_H, ECE_Emp = ece_D, ECE_Vis = ece_A, ECE_Prom = mean(c(ece_H, ece_D, ece_A))))
  }
  return(list(prec = res_prec, fiab = res_fiab))
}

met_1718 <- calcular_metricas(base_1718)
met_1819 <- calcular_metricas(base_1819)

# 4. CONSTRUCCIÓN DE LA MATRIZ DELTA Y EXPORTACIÓN
# ------------------------------------------------------------------------------
delta_prec <- merge(met_1718$prec, met_1819$prec, by = "Casa_Apuestas", suffixes = c("_pre", "_post"))
delta_prec$Delta_Brier <- delta_prec$Brier_post - delta_prec$Brier_pre
delta_prec <- delta_prec[order(delta_prec$Delta_Brier, decreasing = TRUE), ]

delta_fiab <- merge(met_1718$fiab, met_1819$fiab, by = "Casa_Apuestas", suffixes = c("_pre", "_post"))
delta_fiab$Delta_ECE_Prom <- delta_fiab$ECE_Prom_post - delta_fiab$ECE_Prom_pre
delta_fiab$Delta_ECE_Emp <- delta_fiab$ECE_Emp_post - delta_fiab$ECE_Emp_pre
delta_fiab <- delta_fiab[order(delta_fiab$Delta_ECE_Prom, decreasing = TRUE), ]

write.csv(delta_prec, file.path(dir_tablas, "Tabla_Delta_Precision_VAR.csv"), row.names = FALSE)
write.csv(delta_fiab, file.path(dir_tablas, "Tabla_Delta_Fiabilidad_VAR.csv"), row.names = FALSE)

# 5. GRÁFICO A: DUMBBELL PLOT (PRECISIÓN) - [CORREGIDO SUPERPOSICIÓN LEYENDA]
# ------------------------------------------------------------------------------
dp <- delta_prec[order(delta_prec$Brier_pre, decreasing = FALSE), ]
y_coords <- 1:nrow(dp)
xlims <- c(min(dp$Brier_pre, dp$Brier_post) - 0.005, max(dp$Brier_pre, dp$Brier_post) + 0.005)

png(file.path(dir_graf_glob, "Grafico_A_Impacto_Brier_Mancuernas.png"), width = 1100, height = 650, res = 120)
par(mar = c(5, 12, 4, 2), bg = "#fcfcfc")

# Se aumenta el límite Y superior para dar espacio exclusivo a la leyenda
plot(0, 0, type = "n", xlim = xlims, ylim = c(0.5, nrow(dp) + 1.5), yaxt = "n", 
     xlab = "Brier Score (Valores mayores indican más error)", ylab = "", 
     main = "Colapso de Precisión Predictiva (Efecto VAR)\nDesplazamiento del Error Cuadrático Medio por Operador")
grid(nx = NULL, ny = NA, col = "gray85", lty = 2)
axis(2, at = y_coords, labels = dp$Casa_Apuestas, las = 2, tick = FALSE, cex.axis = 0.95)

segments(dp$Brier_pre, y_coords, dp$Brier_post, y_coords, col = "gray75", lwd = 4)
points(dp$Brier_pre, y_coords, col = col_pre, pch = 16, cex = 2)
points(dp$Brier_post, y_coords, col = col_post, pch = 16, cex = 2)

# Leyenda con fondo blanco para no cruzar líneas de grilla
legend("topleft", legend = c("17/18 (Pre-VAR)", "18/19 (Post-VAR)", "Magnitud de Degradación"), 
       col = c(col_pre, col_post, "gray75"), pch = c(16, 16, NA), lty = c(NA, NA, 1), lwd = c(NA, NA, 4), 
       bg = "white", box.col = "gray80")
dev.off()

# 6. GRÁFICO B: BARRAS DIVERGENTES (ECE PROMEDIO) - [CORREGIDO EJE X]
# ------------------------------------------------------------------------------
df <- delta_fiab[order(delta_fiab$Delta_ECE_Prom, decreasing = FALSE), ]
col_div <- ifelse(df$Delta_ECE_Prom > 0, col_post, "#81b29a") 
max_xlim <- max(df$Delta_ECE_Prom) * 1.15

png(file.path(dir_graf_glob, "Grafico_B_Impacto_ECE_Divergente.png"), width = 1100, height = 650, res = 120)
par(mar = c(5, 14, 4, 2), bg = "#fcfcfc") # Margen izquierdo ampliado a 14
barplot(df$Delta_ECE_Prom, names.arg = df$Casa_Apuestas, horiz = TRUE, las = 2, 
        col = col_div, border = NA, xlim = c(0, max_xlim),
        main = "Pérdida de Calibración Estructural del Mercado\nCrecimiento Neto del Error Esperado de Calibración (Delta ECE)", 
        xlab = "Aumento Porcentual del Error (Delta)")
grid(nx = NULL, ny = NA, col = "gray85", lty = 2)
barplot(df$Delta_ECE_Prom, names.arg = df$Casa_Apuestas, horiz = TRUE, las = 2, col = col_div, border = NA, add = TRUE, axes=FALSE)
abline(v = 0, lwd = 2, col = "black")
dev.off()

# FUNCIÓN AUXILIAR PARA CURVAS DE SUPERPOSICIÓN
calc_b <- function(p, o) {
  b <- seq(0, 1, length.out = 11); idx <- cut(p, breaks = b, include.lowest = TRUE, labels = FALSE)
  df <- data.frame(Bin = 1:10, Esperado = NA, Observado = NA)
  df$Esperado[match(names(tapply(p, idx, mean, na.rm=T)), df$Bin)] <- tapply(p, idx, mean, na.rm=T)
  df$Observado[match(names(tapply(o, idx, mean, na.rm=T)), df$Bin)] <- tapply(o, idx, mean, na.rm=T)
  return(na.omit(df))
}

# 7. GRÁFICO C: SUPERPOSICIÓN DE LA FRONTERA (EMPATE)
# ------------------------------------------------------------------------------
p_pre_D <- base_1718$Promedio_Apertura_P_Empate_Pot; o_pre_D <- base_1718$Obs_Empate
p_post_D <- base_1819$Promedio_Apertura_P_Empate_Pot; o_post_D <- base_1819$Obs_Empate
curva_pre_D <- calc_b(p_pre_D, o_pre_D); curva_post_D <- calc_b(p_post_D, o_post_D)

png(file.path(dir_graf_glob, "Grafico_C_Superposicion_Mercado_Empate.png"), width = 1100, height = 650, res = 120)
par(mar = c(5, 5, 4, 2), bg = "#fcfcfc")
plot(NULL, xlim = c(0, 0.45), ylim = c(0, 0.45), 
     xlab = "Probabilidad Asignada al Empate (Mercado Global)", ylab = "Frecuencia Empírica Observada",
     main = "Descalibración del Empate: Curva de Frontera del Mercado\n(Promedio Apertura Pre-VAR vs Post-VAR)", 
     cex.main = 1.3, cex.lab = 1.1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#f8f9fa", border = NA)
grid(col = "#e9ecef", lty = 1, lwd = 1.5)
abline(a = 0, b = 1, lty = 2, col = "#6c757d", lwd = 2)
lines(curva_pre_D$Esperado, curva_pre_D$Observado, col = col_pre, lwd = 3, type = "b", pch = 16, cex = 1.6)
lines(curva_post_D$Esperado, curva_post_D$Observado, col = col_post, lwd = 3, type = "b", pch = 17, cex = 1.6)
legend("topleft", legend = c("Mercado 17/18 (Pre-VAR)", "Mercado 18/19 (Post-VAR)", "Calibración Perfecta"),
       col = c(col_pre, col_post, "#6c757d"), lty = c(1,1,2), pch = c(16,17,NA), lwd = 3, bg = "white", box.col = "gray80", cex = 1.1)
dev.off()

# 8. GRÁFICO D: SUPERPOSICIÓN DE LA FRONTERA (VICTORIA LOCAL)
# ------------------------------------------------------------------------------
p_pre_H <- base_1718$Promedio_Apertura_P_Local_Pot; o_pre_H <- base_1718$Obs_Local
p_post_H <- base_1819$Promedio_Apertura_P_Local_Pot; o_post_H <- base_1819$Obs_Local
curva_pre_H <- calc_b(p_pre_H, o_pre_H); curva_post_H <- calc_b(p_post_H, o_post_H)

png(file.path(dir_graf_glob, "Grafico_D_Superposicion_Mercado_Local.png"), width = 1100, height = 650, res = 120)
par(mar = c(5, 5, 4, 2), bg = "#fcfcfc")
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), 
     xlab = "Probabilidad Asignada al Local (Mercado Global)", ylab = "Frecuencia Empírica Observada",
     main = "Descalibración Victoria Local: Curva de Frontera del Mercado\n(Promedio Apertura Pre-VAR vs Post-VAR)", 
     cex.main = 1.3, cex.lab = 1.1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#f8f9fa", border = NA)
grid(col = "#e9ecef", lty = 1, lwd = 1.5)
abline(a = 0, b = 1, lty = 2, col = "#6c757d", lwd = 2)
lines(curva_pre_H$Esperado, curva_pre_H$Observado, col = col_pre, lwd = 3, type = "b", pch = 16, cex = 1.6)
lines(curva_post_H$Esperado, curva_post_H$Observado, col = col_post, lwd = 3, type = "b", pch = 17, cex = 1.6)
legend("topleft", legend = c("Mercado 17/18 (Pre-VAR)", "Mercado 18/19 (Post-VAR)", "Calibración Perfecta"),
       col = c(col_pre, col_post, "#6c757d"), lty = c(1,1,2), pch = c(16,17,NA), lwd = 3, bg = "white", box.col = "gray80", cex = 1.1)
dev.off()

# 9. GRÁFICO E: SUPERPOSICIÓN DE LA FRONTERA (VICTORIA VISITANTE)
# ------------------------------------------------------------------------------
p_pre_A <- base_1718$Promedio_Apertura_P_Vis_Pot; o_pre_A <- base_1718$Obs_Vis
p_post_A <- base_1819$Promedio_Apertura_P_Vis_Pot; o_post_A <- base_1819$Obs_Vis
curva_pre_A <- calc_b(p_pre_A, o_pre_A); curva_post_A <- calc_b(p_post_A, o_post_A)

png(file.path(dir_graf_glob, "Grafico_E_Superposicion_Mercado_Visitante.png"), width = 1100, height = 650, res = 120)
par(mar = c(5, 5, 4, 2), bg = "#fcfcfc")
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), 
     xlab = "Probabilidad Asignada al Visitante (Mercado Global)", ylab = "Frecuencia Empírica Observada",
     main = "Descalibración Victoria Visitante: Curva de Frontera del Mercado\n(Promedio Apertura Pre-VAR vs Post-VAR)", 
     cex.main = 1.3, cex.lab = 1.1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#f8f9fa", border = NA)
grid(col = "#e9ecef", lty = 1, lwd = 1.5)
abline(a = 0, b = 1, lty = 2, col = "#6c757d", lwd = 2)
lines(curva_pre_A$Esperado, curva_pre_A$Observado, col = col_pre, lwd = 3, type = "b", pch = 16, cex = 1.6)
lines(curva_post_A$Esperado, curva_post_A$Observado, col = col_post, lwd = 3, type = "b", pch = 17, cex = 1.6)
legend("topleft", legend = c("Mercado 17/18 (Pre-VAR)", "Mercado 18/19 (Post-VAR)", "Calibración Perfecta"),
       col = c(col_pre, col_post, "#6c757d"), lty = c(1,1,2), pch = c(16,17,NA), lwd = 3, bg = "white", box.col = "gray80", cex = 1.1)
dev.off()

cat("\n>> Comparación Finalizada. Los 5 gráficos se encuentran en Graficos_Globales/\n")