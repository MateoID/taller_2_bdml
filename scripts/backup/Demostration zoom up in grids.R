



sixStats <- function(data, lev = NULL, model = NULL) {
  # Calcula las métricas estándar
  twoClass <- twoClassSummary(data, lev, model)
  default <- defaultSummary(data, lev, model)
  
  # Calcula el F1 Score
  f1 <- F1_Score(data$obs, data$pred, positive = lev[1])
  
  # Combina todas las métricas
  c(twoClass, default, F1 = f1)
}

# Control de entrenamiento
ctrl_c1 <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = sixStats,
  classProbs = TRUE,      # Necesario para ROC/F1
  savePredictions = TRUE, # Para calcular métricas
  verboseIter = FALSE
)

# especificamos la grilla de los alphas
grid <- expand.grid(cp = seq(0, 0.04, by = 0.01))


# Declarando la semilla para reproducibilidad
set.seed(123)  

# Entrenamiento del modelo
cv_tree_1 <- train(
  Pobre ~ Cabecera + Dominio + num_room + 
    num_bed + propiedad + pago_amort + renta_h + 
    renta_r + Nper + Depto + suma_antiguedad + promedio_antiguedad + 
    tiene_empleado_publico + tiene_patron + tiene_cuenta_propia + 
    tiene_emp_domestico + tiene_jornalero + tiene_sin_remuneracion+ 
    n_posiciones_lab_distintas + aux_trans + ind_prima + prima_serv + prima_nav +
    prima_vac + ind_viaticos + ind_oficio + ind_arriendo + pet_trabajo +
    max_educ:ocupado + hr_extr + otro_tr + rem_ext + reg_cotiz + cotiz_pen + ing_otros +
    edad_prom + perc_fem,
  data = bd_train,
  method = "rpart", 
  trControl = ctrl_c1, 
  tuneGrid = grid, 
  metric= "F1" 
)
cv_tree_1

# RESULTADOS CART-1 
# 
# 164960 samples
# 14 predictor
# 2 classes: 'pobre', 'No_pobre' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 131968, 131969, 131967, 131968, 131968 
# Resampling results across tuning parameters:
#   
#   cp    ROC        Sens       Spec       Accuracy   Kappa      F1       
# 0.00  0.8465254  0.4355317  0.9257519  0.8276128  0.4015906  0.5028536
# 0.01  0.7709428  0.3147409  0.9554935  0.8272187  0.3336207  0.4215800
# 0.02  0.7702644  0.3202822  0.9518782  0.8254365  0.3329065  0.4233201
# 0.03  0.7702644  0.3202822  0.9518782  0.8254365  0.3329065  0.4233201
# 0.04  0.7702644  0.3202822  0.9518782  0.8254365  0.3329065  0.4233201


# CART 2
# especificamos la grilla de los alphas
grid_2 <- expand.grid(cp = seq(0, 0.0115, by = 0.002))

### Entrenando el modelo
# Declarando la semilla para reproducibilidad
set.seed(123)  

# Entrenamiento del modelo
cv_tree_2 <- train(
  Pobre ~ max_educ:ocupado + num_room + Nper +tiene_empleado_publico + tiene_patron + tiene_cuenta_propia +
    ind_prima + prima_serv + prima_nav + prima_vac + ind_viaticos + ocupado + cotiz_pen + edad_prom,
  data = bd_train,
  method = "rpart", 
  trControl = ctrl_c2, 
  tuneGrid = grid_2, 
  metric= "F1" 
)
cv_tree_2

# RESULTADOS CART 2 
# 
# 164960 samples
# 14 predictor
# 2 classes: 'pobre', 'No_pobre' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 131968, 131969, 131967, 131968, 131968 
# Resampling results across tuning parameters:
#   
#   cp    ROC        Sens       Spec       Accuracy   Kappa      F1       
# 0.00  0.8465254  0.4355317  0.9257519  0.8276128  0.4015906  0.5028536
# 0.01  0.7709428  0.3147409  0.9554935  0.8272187  0.3336207  0.4215800
# 0.02  0.7702644  0.3202822  0.9518782  0.8254365  0.3329065  0.4233201
# 0.03  0.7702644  0.3202822  0.9518782  0.8254365  0.3329065  0.4233201
# 0.04  0.7702644  0.3202822  0.9518782  0.8254365  0.3329065  0.4233201
# 
# F1 was used to select the optimal model using the largest value.
# The final value used for the model was cp = 0.


# CART 3
# especificamos la grilla de los alphas
grid_cart_c3 <- expand.grid(cp = seq(0, 0.0001, by = 0.00001))

cv_tree_c3 <- train(
  Pobre ~ Cabecera + Dominio + num_room + 
    num_bed + propiedad + pago_amort + renta_h + 
    renta_r + Nper + Depto + suma_antiguedad + promedio_antiguedad + 
    tiene_empleado_publico + tiene_patron + tiene_cuenta_propia + 
    tiene_emp_domestico + tiene_jornalero + tiene_sin_remuneracion+ 
    n_posiciones_lab_distintas + aux_trans + ind_prima + prima_serv + prima_nav +
    prima_vac + ind_viaticos + ocupado + ind_oficio + ind_arriendo + pet_trabajo +
    max_educ + edad_prom + hr_extr + otro_tr + rem_ext + reg_cotiz + cotiz_pen + ing_otros +
    perc_fem,  # Usa todas las variables (o ajusta la fórmula)
  data = bd_train,
  method = "rpart",
  trControl = ctrl_c3,
  tuneGrid = grid_cart_c3,
  metric = "F1" 
)

cv_tree_c3


# RESULTADOS CART 3
# 164960 samples
# 38 predictor
# 2 classes: 'pobre', 'No_pobre' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 131968, 131968, 131967, 131968, 131969 
# Addtional sampling using up-sampling
# 
# Resampling results across tuning parameters:
#   
#   cp     ROC        Sens       Spec       Accuracy   Kappa      F1       
# 0e+00  0.8516703  0.7247153  0.8510338  0.8257456  0.5141100  0.6247937
# 1e-05  0.8565144  0.7551477  0.8462057  0.8279765  0.5281778  0.6373637
# 2e-05  0.8627086  0.7848229  0.8396950  0.8287100  0.5386523  0.6472044
# 3e-05  0.8734682  0.8083210  0.8329341  0.8280068  0.5442067  0.6529879
# 4e-05  0.8823787  0.8290635  0.8238691  0.8249091  0.5442484  0.6546753
# 5e-05  0.8873398  0.8386021  0.8201931  0.8238785  0.5450166  0.6559407
# 6e-05  0.8907490  0.8463540  0.8160699  0.8221326  0.5439306  0.6557906
# 7e-05  0.8908720  0.8532582  0.8113327  0.8197260  0.5414186  0.6546007
# 8e-05  0.8910434  0.8556505  0.8094530  0.8187015  0.5401987  0.6539488
# 9e-05  0.8906384  0.8577702  0.8064364  0.8167131  0.5371463  0.6520320
# 1e-04  0.8907658  0.8604349  0.8032303  0.8146823  0.5342052  0.6502351
# 
# F1 was used to select the optimal model using the largest value.
# The final value used for the model was cp = 5e-05.


# CART 4
# especificamos la grilla de los alphas
grid_cart_c4 <- expand.grid(cp = seq(0, 0.0002, by = 0.00002))

cv_tree_c4 <- train(
  Pobre ~ Cabecera + Dominio + num_room + 
    num_bed + propiedad + pago_amort + renta_h + 
    renta_r + Nper + Depto + suma_antiguedad + promedio_antiguedad + 
    tiene_empleado_publico + tiene_patron + tiene_cuenta_propia + 
    tiene_emp_domestico + tiene_jornalero + tiene_sin_remuneracion+ 
    n_posiciones_lab_distintas + aux_trans + ind_prima + prima_serv + prima_nav +
    prima_vac + ind_viaticos + ocupado + ind_oficio + ind_arriendo + pet_trabajo +
    max_educ + edad_prom + hr_extr + otro_tr + rem_ext + reg_cotiz + cotiz_pen + ing_otros +
    perc_fem,  # Usa todas las variables (o ajusta la fórmula)
  data = bd_train,
  method = "rpart",
  trControl = ctrl_c4,
  tuneGrid = grid_cart_c4,
  metric = "F1" 
)

cv_tree_c4

# RESULTADOS CART 4
# 164960 samples
# 38 predictor
# 2 classes: 'pobre', 'No_pobre' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 131967, 131968, 131969, 131968, 131968 
# Addtional sampling using SMOTE
# 
# Resampling results across tuning parameters:
#   
#   cp       ROC        Sens       Spec       Accuracy   Kappa      F1       
# 0.00000  0.8896307  0.6103440  0.9085844  0.8488785  0.5237246  0.6178908
# 0.00002  0.8946197  0.6276347  0.9206509  0.8619908  0.5598930  0.6454863
# 0.00004  0.8941808  0.6322678  0.9212497  0.8633972  0.5647443  0.6494888
# 0.00006  0.8907003  0.6342361  0.9196959  0.8625485  0.5634183  0.6487924
# 0.00008  0.8874988  0.6394445  0.9161715  0.8607723  0.5609678  0.6476888
# 0.00010  0.8844869  0.6409284  0.9134125  0.8588628  0.5570194  0.6450709
# 0.00012  0.8815380  0.6409886  0.9115632  0.8573957  0.5537168  0.6427932
# 0.00014  0.8803782  0.6421092  0.9100700  0.8564258  0.5518567  0.6416116
# 0.00016  0.8797070  0.6465907  0.9071747  0.8550073  0.5501441  0.6409585
# 0.00018  0.8783969  0.6461970  0.9061818  0.8541343  0.5480194  0.6394201
# 0.00020  0.8776387  0.6468631  0.9049084  0.8532493  0.5462775  0.6383053