# Carreguem les llibreries
library(dplyr)
library(tidyr)

# Carreguem les dades
data <- read.csv("depression_dataset.csv")

# Filtrem, preprocessem i agrupem les dades
work_study_hours <- data %>%
  select(Work.Study.Hours, Depression) %>%
  na.omit() %>%
  mutate(Work.Study.Hours = as.integer(Work.Study.Hours)) %>%
  group_by(Work.Study.Hours, Depression) %>%
  summarise(Count = n(), .groups = "drop") %>%
  spread(key = Depression, value = Count, fill = 0)

# Guardem el resultat com a CSV
write.csv(work_study_hours, "work_study_hours.csv", row.names = FALSE)
