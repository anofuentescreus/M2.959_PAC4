# Carreguem les llibreries
library(dplyr)
library(tidyr)

# Carreguem les dades
data <- read.csv("depression_dataset.csv")

# Seleccionem les variables rellevants
variables_rellevants <- data %>%
  select(Age, Academic.Pressure, Work.Pressure, Study.Satisfaction, 
         Job.Satisfaction, Sleep.Duration, Work.Study.Hours, Financial.Stress, Dietary.Habits, Depression)

# Convertim variables categòriques a valors numèrics i imputem els valors nuls amb la mitjana
variables_rellevants <- variables_rellevants %>%
  mutate(
    Academic.Pressure = as.numeric(Academic.Pressure),  
    Work.Pressure = as.numeric(Work.Pressure),          
    Study.Satisfaction = as.numeric(Study.Satisfaction),
    Job.Satisfaction = as.numeric(Job.Satisfaction),    
    Financial.Stress = as.numeric(Financial.Stress),    
    Sleep.Duration = case_when(
      Sleep.Duration == "More than 8 hours" ~ 8.5,    
      Sleep.Duration == "Less than 5 hours" ~ 4.5,    
      Sleep.Duration == "5-6 hours" ~ 5.5,             
      Sleep.Duration == "7-8 hours" ~ 7.5,             
      TRUE ~ as.numeric(Sleep.Duration)                
    ),
    Work.Study.Hours = as.numeric(Work.Study.Hours),    
    Dietary.Habits = case_when(
      Dietary.Habits == "Healthy" ~ 3,
      Dietary.Habits == "Moderate" ~ 2,
      Dietary.Habits == "Unhealthy" ~ 1,
      TRUE ~ NA_real_
    ),
    Depression = case_when(
      Depression == "Yes" ~ 1,  
      Depression == "No" ~ 0   
    )
  )

# Imputem els valors nuls amb la mitjana de cada columna
variables_rellevants <- variables_rellevants %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Calculem la matriu de correlació
matriu_correlacio <- cor(variables_rellevants, use = "everything", method = "pearson")

# Mostrem la matriu de correlació
print(matriu_correlacio)

# Guardem el resultat com a CSV
write.csv(matriu_correlacio, "matriu_correlacio.csv", row.names = TRUE)