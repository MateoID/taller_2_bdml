
#Cargamos bases de datos 
raw_data_path <- '../data'
predictions_path <- '../results/predictions'

# Datos 
bd_train <- read.csv(file.path(raw_data_path, 'bd_train_limpia.csv'))
bd_test <- read.csv(file.path(raw_data_path, 'bd_test_limpia.csv'))



# Cargar librerías necesarias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Manipulación de datos
  caret,        # Preprocesamiento
  recipes,      # Ingeniería de variables
  naniar,       # Manejo de valores missing
  themis        # Balanceo de clases
)

## 1. Selección y transformación inicial de variables --------------------------

# Eliminar variables no relevantes o problemáticas
db_prep <- bd_train %>% 
  select(
    -id,                # Identificador no predictivo
    -Lp,                # Línea de pobreza (si es redundante con Pobre)
    -Ingtotug,          # Ingreso total (puede ser redundante)
    -pago_amort,        # Valores extremos (p0=0, p75=0)
    -renta_h,           # Valores extremos (p0=0, p75=400000 vs p100=600000000)
    -renta_r            # Valores extremos (p0=0, p75=300000 vs p100=300000000)
  )

## 2. Conversión de tipos y limpieza ------------------------------------------

# Convertir variables a tipos adecuados
db_prep <- db_prep %>%
  mutate(
    # Variable objetivo
    Pobre = factor(Pobre, levels = c(0, 1), labels = c("No_pobre", "pobre")),
    
    # Variables categóricas nominales
    across(c(Clase, Dominio, Depto), as.factor),
    
    # Variables binarias (convertir a factor)
    across(c(tiene_empleado_publico, tiene_patron, tiene_cuenta_propia,
             tiene_emp_domestico, tiene_jornalero, tiene_sin_remuneracion,
             aux_trans, ind_prima, prima_serv, prima_nav, prima_vac,
             ind_viaticos, pension, ocupado, ind_oficio, ind_arriendo,
             hr_extr, otro_tr, rem_ext, reg_cotiz, cotiz_pen, ing_otros),
           ~factor(ifelse(.x == 1, "Si", "No"))),
    
    # Variable ordinal (educación)
    max_educ = factor(max_educ, levels = c(0, 1, 2), 
                      labels = c("Ninguna", "Primaria", "Secundaria+"),
                      ordered = TRUE),
    
    # Discretizar variables numéricas con valores extremos
    num_room = cut(num_room, breaks = c(0, 1, 2, 3, 4, 5, Inf),
                   num_bed = cut(num_bed, breaks = c(0, 1, 2, 3, Inf)),
                   
                   # Transformar variables numéricas con distribución sesgada
                   suma_antiguedad = log1p(suma_antiguedad),
                   promedio_antiguedad = log1p(promedio_antiguedad)
    ))
    
    ## 3. Receta de preprocesamiento ----------------------------------------------
    
    recipe_prep <- recipe(Pobre ~ ., data = db_prep) %>%
      # Imputación de valores missing (si los hubiera)
      step_impute_median(all_numeric_predictors()) %>%
      step_impute_mode(all_nominal_predictors()) %>%
      
      # Crear dummies para variables nominales
      step_dummy(all_nominal_predictors(), -all_outcomes(), -max_educ) %>%
      
      # Tratamiento para variable ordinal (educación)
      step_ordinalscore(max_educ) %>%
      
      # Normalización de variables numéricas
      step_normalize(all_numeric_predictors()) %>%
      
      # Eliminar predictores con varianza cercana a cero
      step_nzv(all_predictors()) %>%
      
      # Balanceo de clases (SMOTE)
      step_smote(Pobre, over_ratio = 0.8)
    
    ## 4. Aplicar transformaciones y división de datos ----------------------------
    
    # Preparar receta
    prep_recipe <- prep(recipe_prep, training = db_prep)
    
    # Aplicar a los datos
    db_processed <- bake(prep_recipe, new_data = NULL)
    
    # Dividir en train/test (80%/20%)
    set.seed(123)
    trainIndex <- createDataPartition(db_processed$Pobre, p = 0.8, list = FALSE)
    train_data <- db_processed[trainIndex, ]
    test_data <- db_processed[-trainIndex, ]
    
    # Verificar proporciones
    prop.table(table(train_data$Pobre))
    prop.table(table(test_data$Pobre))
    
    ## 5. Inspección final --------------------------------------------------------
    glimpse(train_data)
    summary(train_data)
    
    
    
    # función six stats
    sixStats <- function(data, lev = NULL, model = NULL) {
      # Calcula las métricas estándar
      twoClass <- twoClassSummary(data, lev, model)
      default <- defaultSummary(data, lev, model)
      
      # Calcula el F1 Score
      f1 <- MLmetrics::F1_Score(data$obs, data$pred, positive = lev[1])
      
      # Combina todas las métricas
      c(twoClass, default, F1 = f1)
    }
  
    # Configuración del entrenamiento
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = sixStats,
      classProbs = TRUE,
      savePredictions = TRUE,
      verboseIter = TRUE
    )
    
    # Grid de hiperparámetros
    ada_grid <- expand.grid(
      mfinal = c(50, 100, 150),    # Número de iteraciones
      maxdepth = c(2, 3, 4),       # Profundidad de árboles débiles
      coeflearn = c("Breiman")     # Algoritmo de boosting
    )
    
    # Entrenamiento del modelo
    ada_model <- train(
      Pobre ~ .,
      data = train_data,
      method = "AdaBoost.M1",
      trControl = ctrl,
      tuneGrid = ada_grid,
      metric = "F1"
    )
    
    ada_model
    
    # Evaluación
    confusionMatrix(predict(ada_model, test_data), test_data$Pobre)    
    
    
        
##### Procesamiento de la base test######
    
# Preprocesamiento:
    # Preprocesar bd_test de la misma manera que bd_train
    bd_test_prep <- bd_test %>%
      select(
        -id,                # Identificador no predictivo
      ) %>%
      mutate(
        # Variables categóricas nominales
        across(c(Clase, Dominio, Depto), as.factor),
        
        # Variables binarias (convertir a factor)
        across(c(tiene_empleado_publico, tiene_patron, tiene_cuenta_propia,
                 tiene_emp_domestico, tiene_jornalero, tiene_sin_remuneracion,
                 aux_trans, ind_prima, prima_serv, prima_nav, prima_vac,
                 ind_viaticos, pension, ocupado, ind_oficio, ind_arriendo,
                 hr_extr, otro_tr, rem_ext, reg_cotiz, cotiz_pen, ing_otros),
               ~factor(ifelse(.x == 1, "Si", "No"))),
        
        # Variable ordinal (educación)
        max_educ = factor(max_educ, levels = c(0, 1, 2), 
                          labels = c("Ninguna", "Primaria", "Secundaria+"),
                          ordered = TRUE),
        
        # Discretizar variables numéricas con valores extremos
        num_room = cut(num_room, breaks = c(0, 1, 2, 3, 4, 5, Inf),
                       num_bed = cut(num_bed, breaks = c(0, 1, 2, 3, Inf)),
                       
                       # Transformar variables numéricas con distribución sesgada
                       suma_antiguedad = log1p(suma_antiguedad),
                       promedio_antiguedad = log1p(promedio_antiguedad)
        ))
    
    # Aplicar la receta de preprocesamiento a bd_test
    bd_test_processed <- bake(prep_recipe, new_data = bd_test_prep)
    
    # Hacer predicciones usando el modelo AdaBoost
    predictions <- predict(ada_model, bd_test_processed)
    length(predictions)
    
    # Agregar las predicciones a bd_test
    bd_test$Predicted_Pobre <- predictions
    
 # agregar la columna de predicciones a bd_test
  bd_test$pobre <- predictions
  
  # Crear la base final con solo id y Predicted_Pobre
  submission <- bd_test %>%
    select(id, pobre)
  
  # Convertir Predicted_Pobre a 1 y 0
  final_submission <- submission %>%
    mutate(pobre = ifelse(pobre == "pobre", 1, 0))
  
  # revisando la proporción de pobres(inicialmente 20%)
  prop.table(table(final_submission$pobre))

  # Create descriptive filename
  filename <- sprintf("%s/ADAB_mfinal150_maxdepth4_coeflearn_Breiman_%s.csv", 
                      predictions_path,
                      format(Sys.time(), "%Y%m%d_%H%M"))
  
  # 6. Save with proper format
  write.csv(submission, 
            file = filename,
            row.names = FALSE,
            quote = FALSE)    


    
    
    
    
    
    
    
    
    
    
    
    
    