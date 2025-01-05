# Instal·lem i carreguem dplyr
library(dplyr)

# Carreguem les dades
data <- read.csv("depression_dataset.csv")

# Seleccionem les variables d'interès
gender <- data %>%
  select(Depression, Gender, Academic.Pressure, Work.Pressure, Financial.Stress)

# Agrupem per Depression i Gender i calculem mitjanes
dataset_gender <- gender %>%
  group_by(Depression, Gender) %>%
  summarise(
    Academic_Pressure_mean = mean(Academic.Pressure, na.rm = TRUE),
    Work_Pressure_mean = mean(Work.Pressure, na.rm = TRUE),
    Financial_Stress_mean = mean(Financial.Stress, na.rm = TRUE)
  )

# Exportem el dataset preprocessat
write.csv(dataset_gender, "dataset_gender.csv", row.names = FALSE)



