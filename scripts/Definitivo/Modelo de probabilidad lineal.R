#--------------------------------------------------------------------------------------------------------------#
#
#                                   Modelo de probabilidad lineal
#
#--------------------------------------------------------------------------------------------------------------#

#Cargamos bases de datos 
bd_train <- read.csv("data/bd_train_limpia.csv")
bd_test <- read.csv("data/bd_test_limpia.csv")


#Sembramos la semilla 
set.seed(123)

split_interno <- createDataPartition(bd_train$Pobre, p = 0.8, list = FALSE)
sub_train <- bd_train[split_interno, ]
sub_test  <- bd_train[-split_interno, ]

#Especificación del modelo 1
mpl_model <- Pobre ~ Clase + Dominio + num_room + num_bed + propiedad + pago_amort + renta_h + 
  renta_r + Nper + Depto + suma_antiguedad + promedio_antiguedad + 
  tiene_empleado_publico + tiene_patron + tiene_cuenta_propia + 
  tiene_emp_domestico + tiene_jornalero + tiene_sin_remuneracion + 
  n_posiciones_lab_distintas + aux_trans + ind_prima + prima_serv + prima_nav + 
  prima_vac + ind_viaticos + ocupado + ind_oficio + ind_arriendo + pet_trabajo + 
  max_educ + hr_extr + otro_tr + rem_ext + reg_cotiz + cotiz_pen + ing_otros + 
  edad_prom + perc_fem


#Omite los valores faltantes
bd_train <- na.omit(bd_train)

#Establecer funcion para la evaluación 
sixStats <- function(data, lev = NULL, model = NULL) {
  # Calcula las métricas estándar
  twoClass <- twoClassSummary(data, lev, model)
  default <- defaultSummary(data, lev, model)
  
  # Calcula el F1 Score
  f1 <- F1_Score(data$obs, data$pred, positive = lev[2])
  
  # Combina todas las métricas
  c(twoClass, default, F1 = f1)
}


#Definición de hiper-parámetros 

tune_grid <- expand.grid(
  alpha=c(0.8, 1, 0.02),
  lambda=c(0, 0.01, 0.005)
)


fitControl <- trainControl( 
  method = "cv",
  classProbs = TRUE,
  savePredictions = TRUE,
  summaryFunction = sixStats,
  number = 5)


#Transformacion a factores
bd_train$Pobre <- as.factor(bd_train$Pobre)
levels(bd_train$Pobre) <- c("No", "Si")


#Entrenamiento del modelo
modelo_mpl <- train(
  mpl_model,
  data=bd_train,
  method='glmnet',
  trControl=fitControl,
  tuneGrid=tune_grid, 
  metric = "F1",      
  maximize = TRUE,    
  preProcess = c("center", "scale")
)

#Resultado del modelo 
print(modelo_mpl)

#Path
predictions_path <- '../results/predictions'

# Obtener predicciones (clases: "No_Pobre" o "Pobre")
predict_mpl <- predict(modelo_mpl, newdata = bd_test)
bd_test$prob_pobre <- predict_mpl

#Establecemos la clasificación
mpl <- bd_test %>%
  select(id, prob_pobre) %>%
  mutate(prob_pobre = as.numeric(prob_pobre)) %>%
  mutate(Pobre = ifelse(prob_pobre >= 0.5, 1,0)) %>% 
  select(id, Pobre)

#Creamos el  archivo
filename <- sprintf("%s/MPL_EN_lambda_00_alpha_00%s.csv", 
                    predictions_path,
                    format(Sys.time(), "%Y%m%d_%H%M"))

#Guardamos el archivo para la predicción final
write.csv(mpl, 
          file = filename,
          row.names = FALSE,
          quote=FALSE)

#---------- Validation set --------------------

sub_test$Pobre <- as.factor(sub_test$Pobre)
levels(sub_test$Pobre) <- c("No", "Si")

# Predicción de probabilidades
pred_probs <- predict(modelo_mpl, newdata = sub_test, type = "prob")

# Umbral de decisión de Bayes
bayes <- 0.5

# Convertir probabilidades a clases
pred_clases <- ifelse(pred_probs$Si >= bayes, "Si", "No")
pred_clases <- factor(pred_clases, levels = c("No", "Si")) 

#Calcula el F1 score
eval_data <- data.frame(sub_test$Pobre, pred_clases)
f1_val_set <- F1_Score(eval_data$sub_test.Pobre, eval_data$pred_clases, positive = "Si")
cat("F1 Score: ", round(f1_val_set, 3), "\n")


