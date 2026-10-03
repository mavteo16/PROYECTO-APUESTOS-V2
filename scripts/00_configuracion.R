# ==============================================================================
# 00_CONFIGURACION.R
# Parámetros y rutas del proyecto (único lugar que se edita para cambiar liga,
# temporadas o casas de apuestas)
# ==============================================================================
# Proyecto : ¿Están bien calibradas las casas de apuestas?
# Curso    : Estadística Industrial · Universidad del Magdalena · 2026-II
# Autores  : Mateo Valencia, Samuel Lopez, Camilo Henriquez,
#            Valeria De Jesus Gutierrez Nuñez
#
# Uso      : Lo cargan automáticamente todos los scripts (01 a 05) y el reporte.
#            No hace falta ejecutarlo a mano.
#
# Todas las rutas son RELATIVAS a la carpeta del proyecto. Abrir siempre el
# proyecto desde PROYECTO-APUESTOS-V2.Rproj para que funcionen en cualquier equipo.
# ==============================================================================

# 1. CARPETA RAÍZ DEL PROYECTO
# ------------------------------------------------------------------------------
# Funciona tanto si R está en la carpeta del proyecto como en la carpeta scripts/
if (dir.exists("datos_crudos")) {
  raiz <- normalizePath(".", winslash = "/")
} else if (dir.exists(file.path("..", "datos_crudos"))) {
  raiz <- normalizePath("..", winslash = "/")
} else {
  stop("No se encontró la carpeta 'datos_crudos'. Abre el proyecto con PROYECTO-APUESTOS-V2.Rproj ",
       "o fija como directorio de trabajo la carpeta del proyecto.")
}

# 2. PARÁMETROS DEL ANÁLISIS (editar aquí para cambiar el alcance)
# ------------------------------------------------------------------------------
liga        <- "SP1"                         # código de Football-Data (SP1 = LaLiga)
temporadas  <- c("2017-2018", "2018-2019")   # en el orden en que se analizan
temporada_var <- "2018/19"                   # primera temporada con VAR en la liga
partidos_por_temporada <- 380                # 20 equipos x 19 rivales x 2 vueltas

# Casas de apuestas comparadas (nombre en el proyecto = prefijo de Football-Data)
casas_proyecto <- c(Bet365 = "B365", Bwin = "BW", Interwetten = "IW",
                    Pinnacle = "PS", WilliamHill = "WH", BetVictor = "VC")

# Prefijos que el script de control marca como "usados en el análisis"
# (las casas comparadas + cuota de cierre de Pinnacle)
operadores_analisis <- c(unname(casas_proyecto), "PSC")

# 3. RUTAS DE ENTRADA
# ------------------------------------------------------------------------------
dir_crudos   <- file.path(raiz, "datos_crudos")
rutas_crudas <- file.path(dir_crudos, paste0(liga, " ", temporadas, ".csv"))

# 4. RUTAS DE SALIDA
# ------------------------------------------------------------------------------
dir_datos  <- file.path(raiz, "datos")
ruta_base_unificada <- file.path(dir_datos, "Base_Unificada_Limpia_VAR.csv")
ruta_base_analitica <- file.path(dir_datos, "Base_Analitica_Entregable_1.csv")   # Entregable 1

ruta_resultados <- file.path(raiz, "Resultados")
dir_tablas      <- file.path(ruta_resultados, "Tablas_CSV")
dir_graf_glob   <- file.path(ruta_resultados, "Graficos_Globales")
dir_curvas_1718 <- file.path(ruta_resultados, "Curvas_Fiabilidad", "1718_Pre_VAR")
dir_curvas_1819 <- file.path(ruta_resultados, "Curvas_Fiabilidad", "1819_Post_VAR")

dir_reporte  <- file.path(raiz, "reporte")
ruta_reporte <- file.path(dir_reporte, "Reporte-De-Calibracion.Rmd")                # Entregable 3

for (d in c(dir_datos, dir_tablas, dir_graf_glob, dir_curvas_1718, dir_curvas_1819)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

CONFIG_CARGADA <- TRUE