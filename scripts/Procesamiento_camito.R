rm(list = ls())
cat("\f")
setwd("C:/Users/mc-c2/OneDrive - Universidad de los andes/Universidad/MECA/Big Data y Machine Learning/Problem Sets/Ejercicios")


#configuracion de paquetes
require(pacman)

p_load(
  tidyverse,
  dplyr,
  caret,
  glmnet,
  Metrics,
  skimr,
  ranger, 
  caret
)


#Cargamos los datos 
# Datos training
hogares_tr <- read.csv('train_hogares.csv')
personas_tr <- read.csv('train_personas.csv')

# Datos test
hogares_te <- read.csv('test_hogares.csv')
personas_te <- read.csv('test_personas.csv')

#Preprocesamiento total 

#redefinicion de PET por valores faltantes
personas_te <- personas_te %>%
  mutate(
    Pet_nueva = case_when(
      personas_te$Clase == 1 & personas_te$P6040 >= 12 ~ 1,
      personas_te$Clase == 2 & personas_te$P6040 >= 10 ~ 1,
      TRUE ~ 0
  )
)

#Asignacion de variables binarias
personas_final <- personas_te %>%
  group_by(personas_te$id) %>%
  summarise(
    
    #Porcentaje de mujeres
    n_miembros = n(), #Cuenta el numero de miembros en el hogar
    n_fem = sum(P6020==2, na.rm = TRUE), #cuenta la cantidad de mujeres en el hogar 
    perc_fem = (n_fem/n_miembros)*100, #genera la proporcion de mujeres por hogar, 
    
    #Indicador de oficio 
    ind_oficio = as.integer(any(Oficio %in% c(2,4,6,9,12,20,21), na.rm = TRUE)),
    
    #Indicador de prima de servicios 
    ind_prima = as.integer(any(P6545 == 1, na.rm = TRUE)),
    
    #Indicador de viaticos 
    ind_viaticos = as.integer(any(P6630s3 == 1, na.rm = TRUE)), 
    
    #Indicador de arriendo
    ind_arriendo = as.integer(any(P7495 == 1, na.rm = TRUE)), 
    
    #Personas en edad de trabajar
    pet_trabajo = sum(Pet_nueva, na.rm = TRUE)
  )

personas_final %>%
  select(where(~ all(. %in% c(0, 1, NA)))) %>%  # Filtra binarias
  summarise(across(everything(), list("1" = ~ sum(. == 1), "0" = ~ sum(. == 0))))
            
rm(personas_final)
            
            
            