library(dplyr)
library(tidyr)

# Deskriptive Statistiken berechnen
deskriptive_statistiken <- data_imputed_jc %>%
  select_if(~!inherits(., "Date") & !inherits(., "POSIXt")) %>% # Datum und Zeit ausschließen
  summarise_all(list(
    Minimum = ~min(., na.rm = TRUE),
    Q1 = ~quantile(., 0.25, na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    Mittelwert = ~mean(., na.rm = TRUE),
    Q3 = ~quantile(., 0.75, na.rm = TRUE),
    Maximum = ~max(., na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistik"),
    names_pattern = "(.+)_(.+)" # Nur zwei Teile: Variable und Statistik
  ) %>%
  pivot_wider(
    names_from = Statistik,
    values_from = value,
    values_fn = list(value = ~mean(.x, na.rm = TRUE)) # Bei Duplikaten den Durchschnitt berechnen
  )

# Ergebnisse anzeigen
print(deskriptive_statistiken, n = 100)

# Optional: Ergebnisse in eine CSV-Datei speichern
write.csv(deskriptive_statistiken, "deskriptive_statistiken.csv", row.names = FALSE)
library(knitr)
kable(deskriptive_statistiken, format = "latex", booktabs = TRUE)

