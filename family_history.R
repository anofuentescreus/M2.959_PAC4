# Instal·lem i carreguem dplyr
install.packages("dplyr")
library(dplyr)

# Carreguem les dades
data <- read.csv("depression_dataset.csv")

# Agrupem les dades segons història familiar de malalties mentals i depressió
family_history <- data %>%
  group_by(Family.History.of.Mental.Illness, Depression) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Family.History.of.Mental.Illness) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Guardem el resultat com a CSV
write.csv(family_history, "family_history.csv", row.names = FALSE)

