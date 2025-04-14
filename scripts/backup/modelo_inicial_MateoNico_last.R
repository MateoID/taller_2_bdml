`

```{r}

#Selección de variables relevantes
variables_modelo <- c("Pobre", "Clase", "Dominio", "num_room", "num_bed", "propiedad",
                      "pago_amort", "renta_h", "renta_r", "Nper", "Depto", "suma_antiguedad", 
                      "promedio_antiguedad", "tiene_empleado_publico",
                      "tiene_patron", "tiene_cuenta_propia", "tiene_emp_domestico",       
                      "tiene_jornalero", "tiene_sin_remuneracion", 
                      "n_posiciones_lab_distintas", "aux_trans", "ind_prima",
                      "prima_serv", "prima_nav", "prima_vac", "ind_viaticos", 
                      "ocupado", "ind_oficio", "ind_arriendo", "pet_trabajo",                
                      "max_educ", "hr_extr", "otro_tr", "rem_ext", "reg_cotiz",
                      "cotiz_pen", "ing_otros", "edad_prom", "perc_fem")

#Preparación de la base de entrenamiento
setDT(bd_train)
train <- bd_train[, .SD, .SDcols = c("id", variables_modelo)]

#Preparación del conjunto de prueba 
variables_modelo_te <- setdiff(variables_modelo, "Pobre")
setDT(bd_test)
test <- bd_test[, .SD, .SDcols = c("id", variables_modelo_te)]
```


```{r}
set.seed(123)

train <- na.omit(train)

# Boosting con XGBoost
# Preparar matrices para XGBoost
train_numeric <- train[, lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = variables_modelo[-1]]
train_matrix <- xgb.DMatrix(
  data = as.matrix(train_numeric),
  label = as.numeric(train$Pobre) - 1
)

# Entrenar modelo XGBoost
boost_model <- xgboost(
  data = train_matrix,
  max.depth = 6,
  nrounds = 100,
  objective = "binary:logistic",
  verbose = 0
)
```


```{r}
#Evaluación del modelo en la base de test
test <- na.omit(test)

test_numeric <- test[, lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = variables_modelo_te]
test_matrix <- xgb.DMatrix(
  data = as.matrix(test_numeric[, variables_modelo_te, with = FALSE])
)

boost_pred <- predict(boost_model, newdata = test_matrix)

df_pred <- bd_test %>%
  select(id) %>%
  mutate(prob = boost_pred) %>%
  mutate(Pobre = ifelse(prob > 0.5, 1, 0)) %>%
  select(id, Pobre)
```


```{r}
#Entrenamiento de modelo de Random Forest
model_tree <- Pobre ~ Clase + Dominio + num_room + num_bed + propiedad + pago_amort + renta_h + 
  renta_r + Nper + Depto + suma_antiguedad + promedio_antiguedad + 
  tiene_empleado_publico + tiene_patron + tiene_cuenta_propia + 
  tiene_emp_domestico + tiene_jornalero + tiene_sin_remuneracion + 
  n_posiciones_lab_distintas + aux_trans + ind_prima + prima_serv + prima_nav + 
  prima_vac + ind_viaticos + ocupado + ind_oficio + ind_arriendo + pet_trabajo + 
  max_educ + hr_extr + otro_tr + rem_ext + reg_cotiz + cotiz_pen + ing_otros + 
  edad_prom + perc_fem
```

```{r}

#Entrenamiento y definición de hiperparámetros para el modelo de Random Forest
set.seed(404)

fitControl <- trainControl( 
  method = "cv",
  number = 5)

tree_grid <- expand.grid(
  mtry = c(7, 8, 10),
  splitrule = "gini",
  min.node.size = c(5, 7, 9)
)

tree_model <- train(
  model_tree,  # Fórmula del modelo
  data = bd_train,  # Dataset de entrenamiento
  method = "ranger",  # Usamos el motor ranger para Random Forests
  trControl = fitControl,  # Especificamos los controles de validación cruzada definidos antes
  tuneGrid = tree_grid,
  num.trees = 250,
  importance = "impurity"  # Calculamos la importancia de variables por permutación
)
```

```{r}
#Predicciones del modelo de Random Forest 
predictions <- predict(tree_model, bd_test)

df_pred_arbol <- bd_test %>%
  select(id) %>%
  mutate(Pobre = as.numeric(!!predictions)) %>%
  mutate(Pobre = ifelse(Pobre==2, 1, 0))
```

```{r}




```






