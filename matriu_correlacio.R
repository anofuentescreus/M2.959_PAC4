# Carreguem les llibreries
library(dplyr)
library(tidyr)

# Carreguem les dades
data <- read.csv("depression_dataset.csv")

# Seleccionem les variables rellevants
variables_rellevants <- data %>%
  select(Age, Academic.Pressure, Work.Pressure, Study.Satisfaction, 
         Job.Satisfaction, Sleep.Duration, Work.Study.Hours, Financial.Stress, Dietary.Habits, Depression)

# Convertim variables categòriques a valors numèrics
variables_rellevants <- variables_rellevants %>%
  mutate(
    Academic.Pressure = as.numeric(Academic.Pressure),  # Ja numèrica
    Work.Pressure = as.numeric(Work.Pressure),          # Ja numèrica
    Study.Satisfaction = as.numeric(Study.Satisfaction),# Ja numèrica
    Job.Satisfaction = as.numeric(Job.Satisfaction),    # Ja numèrica
    Financial.Stress = as.numeric(Financial.Stress),    # Ja numèrica
    Sleep.Duration = case_when(
      Sleep.Duration == "More than 8 hours" ~ 8.5,    # Assignem 8.5 a "More than 8 hours"
      Sleep.Duration == "Less than 5 hours" ~ 4.5,    # Assignem 4.5 a "Less than 5 hours"
      Sleep.Duration == "5-6 hours" ~ 5.5,             # Assignem 5.5 a "5-6 hours"
      Sleep.Duration == "7-8 hours" ~ 7.5,             # Assignem 7.5 a "7-8 hours"
      TRUE ~ as.numeric(Sleep.Duration)                # Convertim altres valors numèrics directament
    ),
    Work.Study.Hours = as.numeric(Work.Study.Hours),    # Ja numèrica
    Dietary.Habits = case_when(
      Dietary.Habits == "Healthy" ~ 3,
      Dietary.Habits == "Moderate" ~ 2,
      Dietary.Habits == "Unhealthy" ~ 1,
      TRUE ~ NA_real_
    ),
    Depression = case_when(
      Depression == "Yes" ~ 1,  # Assignem 1 per "Yes"
      Depression == "No" ~ 0,   # Assignem 0 per "No"
    )
  )

# Calculem la matriu de correlació ignorant valors nuls
matriu_correlacio <- cor(variables_rellevants, use = "pairwise.complete.obs", method = "pearson")

# Mostrem la matriu de correlació
print(matriu_correlacio)

# Guardem el resultat com a CSV
write.csv(matriu_correlacio, "matriu_correlacio.csv", row.names = TRUE)
