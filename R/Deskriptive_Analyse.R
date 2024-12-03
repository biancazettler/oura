# Lade vorbereitende Skripte und benötigte Bibliotheken
source("R/Variablenselektion_ZV_Schlafwert.R")
library(dplyr)
library(tidyr)
library(ggplot2)

################## DESKRIPTIVE STATISTIKEN ###################

# Berechnung der deskriptiven Statistiken, ohne Datum und Faktorvariablen
deskriptive_statistiken <- data_imputed %>%
  select_if(~ !inherits(., "Date") & !inherits(., "POSIXt") & !is.factor(.)) %>%  # Datum, Zeit und Faktorvariablen ausschließen
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
    names_pattern = "(.+)_(.+)"
  ) %>%
  pivot_wider(
    names_from = Statistik,
    values_from = value,
    values_fn = list(value = ~mean(.x, na.rm = TRUE))
  )

# Ergebnisse anzeigen
print(deskriptive_statistiken, n = 100)

# Optional: Ergebnisse in eine CSV-Datei speichern
write.csv(deskriptive_statistiken, "deskriptive_statistiken.csv", row.names = FALSE)

################## VISUALISIERUNG: ZUSAMMENHÄNGE ###################

# Deutsche Namen der Variablen
german_names <- c(
  "date_numeric" = "Datum",
  "HRV.Balance.Score" = "Herzfrequenzvariabilität",
  "Meet.Daily.Targets.Score" = "Aktivität",
  "Previous.Day.Activity.Score" = "Vortagsaktivität",
  "Recovery.Index.Score" = "Erholungsindex",
  "Respiratory.Rate" = "Atemfrequenz",
  "Resting.Heart.Rate.Score" = "Ruheherzfrequenz",
  "tavg" = "Durchschnittstemperatur",
  "Temperature.Score" = "Körpertemperatur",
  "tsun" = "Sonnenscheindauer",
  "Sleep.Score" = "Schlafwert"
)

# Auswahl relevanter Variablen und Umbenennung
selected_vars <- data_imputed %>%
  select(Sleep.Score, Resting.Heart.Rate.Score, Temperature.Score, HRV.Balance.Score,
         Recovery.Index.Score, Meet.Daily.Targets.Score, tavg, tsun, 
         Respiratory.Rate, Previous.Day.Activity.Score, date_numeric) %>%
  rename_with(~ german_names[.], everything())

# Daten ins lange Format umwandeln
data_long <- selected_vars %>%
  pivot_longer(cols = -Schlafwert, names_to = "Variable", values_to = "Value")

# Facettenplot erstellen
pdf("Zusammenhang_Schlafwert_Einflussgrößen.pdf", width = 10, height = 8)
ggplot(data_long, aes(x = Value, y = Schlafwert)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(
    title = "Zusammenhang zwischen Schlafwert und Einflussgrößen",
    x = "Wert der Einflussgröße",
    y = "Schlafwert"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 11)
  )
dev.off()

################## VERTEILUNG DES SLEEP SCORES ###################

# Histogramm mit Dichteplot
pdf("Dichteplot_Sleep_Score.pdf", width = 8, height = 6)
ggplot(data_imputed, aes(x = Sleep.Score)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +
  labs(
    title = "Verteilung des Schlafwerts",
    x = "Schlafwert",
    y = "Dichte"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
dev.off()

# Boxplot
pdf("Boxplot_Sleep_Score.pdf", width = 8, height = 6)
ggplot(data_imputed, aes(y = Sleep.Score)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Boxplot des Schlafwerts (Sleep Score)",
    y = "Sleep Score"
  ) +
  theme_minimal()
dev.off()

# Q-Q-Plot
pdf("QQPlot_Sleep_Score.pdf", width = 8, height = 6)
ggplot(data_imputed, aes(sample = Sleep.Score)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(
    title = "Q-Q-Plot des Schlafwerts",
    x = "Theoretische Quantile",
    y = "Beobachtete Quantile"
  ) +
  theme_minimal()
dev.off()
