# INTEGRANTES 
- **María Camila Caraballo** - 201613424 - mc.caraballo@uniandes.edu.co  
- **Javier Amaya Nieto** - 202214392 - j.amayan@uniandes.edu.co  
- **Mateo Isaza Díaz** - 202412526 - m.isazad@uniandes.edu.co  
- **Nicolás Moreno Enriquez** - 201615907 - na.morenoe@uniandes.edu.co  

# PROPÓSITO
Responder a los planteamientos del problem set 2 del curso de Big data y Machine learning 2025-1. 

# INTRODUCCIÓN
En 2024, más de 16 millones de personas en Colombia viven en pobreza, revirtiendo una década de avances sociales debido al impacto de la pandemia, según el Banco Mundial. Factores como ubicación geográfica, distribución del ingreso y educación influyen en este riesgo, como señala Lipton (1995), mientras que políticas públicas y modelos econométricos buscan optimizar la focalización de subsidios para evitar costos sociales, especialmente tras la crisis (Haughton, 2009; Vallejo, 2021). El aprendizaje automático ha aportado soluciones innovadoras que buscan predecir la pobreza utilizando características disponibles en diferentes bases de datos. En este ejercicio buscamos predecir pobreza utilizando datos de la Encuesta de Medición de Pobreza Monetaria y Desigualdad del Departamento Administrativo Nacional de Estadística (DANE) correspondiente al año 2018.  

# RECOMENDACIONES PARA LA EJECUCIÓN DEL CÓDIGO
El código generado en este proyecto empieza con un código que realiza la limpieza de las variables y agrega información proveniente de la base de datos a nivel de personas, para tener variables agregadas a nivel de hogares(Preprocesamiento.R). Posteriormente se crearon scripts separados para diferentes algoritmos: MPL con EN, regresión lineal con EN, regresión logística(GLM, LDA, QDA, KNN y diferentes estrategias de balanceo de las clases), CART, random forest y XGboost. Finalmente, tenemos un archivo "master_predict_poverty.R" que se encarga de implementar todo el código.     

# CARACTERÍSTICAS DEL AMBIENTE DE DESARROLLO
R version 4.4.1 (2024-06-14)
Platform: aarch64-apple-darwin20
Running under: macOS 15.3.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: America/Bogota
tzcode source: internal

# REPOSITORIO
Nuestro repositorio se ha denominado "taller_2_bdml" y está conformado por los siguientes elementos:

# ESTRUCTURA
```plaintext
taller_2_bdml/
├───data
│   ├── bd_test_limpia.csv
│   ├── bd_train_limpia.csv
│   ├── sample_submission.csv
│   ├── test_hogares.csv
│   ├── test_personas.csv
│   ├── train_hogares.csv
│   ├── train_personas.csv
├───results
│   ├───predictions
│   └───tables
├── document
│   ├── Documento_predict_poverty.pdf
├── scripts
│   ├──Runall_predict_poverty.R
│   ├──Preprocesamiento.Rmd
│   ├──Descriptive.Rmd
│   ├──Modelo_probabilidad_lineal.R
│   ├──Regresion_lineal_enet.Rmd
│   ├──Logisitic_regression.Rmd
│   ├──CARTs.Rmd
│   ├──Random_forest.Rmd
│   ├──XGboost.Rmd
├── README
└── .gitignore
```

