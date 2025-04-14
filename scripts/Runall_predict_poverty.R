


# Definir la lista de archivos a ejecutar
archivos <- c(
  # "Preprocesamiento.Rmd",
  "Descriptive.Rmd",
  "Modelo_probabilidad_lineal.R",
  "Regresion_lineal_enet.Rmd",
  "Logisitic_regression.Rmd",
  "CARTs.Rmd",
  "Random_forest.Rmd",
  "XGboost"
)

# Función para ejecutar cada archivo según su tipo
ejecutar_archivo <- function(archivo) {
  cat("\n=== EJECUTANDO:", archivo, "===\n")
  
  if (grepl("\\.Rmd$", archivo)) {
    # Para archivos RMarkdown
    rmarkdown::render(archivo, quiet = TRUE)
    cat("Renderizado completo:", archivo, "\n")
  } else if (grepl("\\.R$", archivo)) {
    # Para archivos R estándar
    source(archivo)
    cat("Ejecución completa:", archivo, "\n")
  } else {
    warning("Tipo de archivo no reconocido: ", archivo)
  }
}

# Verificar que todos los archivos existan antes de ejecutar
archivos_existen <- sapply(archivos, file.exists)
if (!all(archivos_existen)) {
  stop("Los siguientes archivos no se encontraron:\n",
       paste(archivos[!archivos_existen], collapse = "\n"))
}

# Ejecutar todos los archivos en orden
invisible(lapply(archivos, ejecutar_archivo))

cat("\n¡Todos los análisis se completaron exitosamente!\n")