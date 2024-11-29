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

# Plot mit allen wichtigen Variablen:
# Bibliotheken laden
library(ggplot2)
library(tidyr)

# Wähle die relevanten Variablen aus (plus "Sleep.Score")
selected_vars <- data_imputed_jc_clean %>%
  select(Sleep.Score, Resting.Heart.Rate.Score, Temperature.Score, HRV.Balance.Score,
         Recovery.Index.Score, pres, Meet.Daily.Targets.Score, tavg, tsun, 
         Respiratory.Rate, Previous.Day.Activity.Score, date_numeric)

# Umwandeln ins lange Format
data_long <- selected_vars %>%
  pivot_longer(cols = -Sleep.Score, names_to = "Variable", values_to = "Value")

# Facettenplot erstellen
ggplot(data_long, aes(x = Value, y = Sleep.Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(
    title = "Zusammenhang zwischen Schlafwert und Prädiktoren",
    x = "Wert der Prädiktorvariable",
    y = "Schlafwert"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11)
  )
