# ==============================================================================
# EJECUTAR_TODO.R  ·  SCRIPT MAESTRO
# Ejecuta el flujo completo, de los CSV de Football-Data al reporte de calibración
# ==============================================================================
# Proyecto : ¿Están bien calibradas las casas de apuestas?
# Curso    : Estadística Industrial · Universidad del Magdalena · 2026-II
# Autores  : Mateo Valencia, Samuel Lopez, Camilo Henriquez,
#            Valeria De Jesus Gutierrez Nuñez
#
# Cómo usarlo:
#   1. Abrir el proyecto con PROYECTO-APUESTOS-V2.Rproj
#   2. Ejecutar:  source("scripts/ejecutar_todo.R")
#
# Para otra liga o temporada: editar solo scripts/00_configuracion.R
# y poner los CSV de Football-Data correspondientes en datos_crudos/.
# ==============================================================================

inicio <- Sys.time()

# 1. CONFIGURACIÓN
# ------------------------------------------------------------------------------
source(if (file.exists("scripts/00_configuracion.R")) "scripts/00_configuracion.R" else "00_configuracion.R",
       encoding = "UTF-8")

paquetes <- c("dplyr", "tidyr", "ggplot2", "knitr", "rmarkdown")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) > 0) {
  stop("Faltan paquetes. Ejecuta primero: install.packages(c(",
       paste0('"', faltan, '"', collapse = ", "), "))")
}

# 2. SECUENCIA DE SCRIPTS
# ------------------------------------------------------------------------------
# Cada script corre en su propio entorno, para que las variables de uno no
# interfieran con las del siguiente. Todos comparten la configuración.
pasos <- c(
  "01_control_auditoria.R",       # auditoría de los CSV crudos
  "02_unificacion_base.R",        # base unificada (datos/Base_Unificada_Limpia_VAR.csv)
  "03_margen_probabilidades.R",   # base analítica (datos/Base_Analitica_Entregable_1.csv)
  "04_analisis_temporadas.R",     # métricas y curvas por temporada
  "05_comparativo_VAR.R"          # comparación sin VAR / con VAR
)

for (paso in pasos) {
  cat("\n\n#######################################################################\n")
  cat(">> EJECUTANDO", paso, "\n")
  cat("#######################################################################\n")
  source(file.path(raiz, "scripts", paso), local = new.env(parent = globalenv()), encoding = "UTF-8")
}

# 3. REPORTE DE CALIBRACIÓN (ENTREGABLE 3)
# ------------------------------------------------------------------------------
cat("\n\n#######################################################################\n")
cat(">> GENERANDO EL REPORTE DE CALIBRACIÓN\n")
cat("#######################################################################\n")
rmarkdown::render(ruta_reporte,
                  params = list(ruta_csv = ruta_base_analitica, temporada_var = temporada_var),
                  envir = new.env(), quiet = TRUE)

# 4. RESUMEN FINAL
# ------------------------------------------------------------------------------
cat("\n========================================================================\n")
cat(">> FLUJO COMPLETO FINALIZADO en", round(as.numeric(difftime(Sys.time(), inicio, units = "mins")), 1), "minutos\n")
cat("========================================================================\n")
cat("Entregable 1 (base analítica):   ", ruta_base_analitica, "\n")
cat("Entregable 3 (reporte):          ", sub("\\.Rmd$", ".html", ruta_reporte), "\n")
cat("Tablas y gráficos:               ", ruta_resultados, "\n")
cat("========================================================================\n")

