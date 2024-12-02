source("R/variablenselektion_zv_sleepscore.R")


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
library(dplyr)

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

# Wähle die relevanten Variablen aus (plus "Schlafwert")
selected_vars <- data_imputed_jc_clean %>%
  select(Sleep.Score, Resting.Heart.Rate.Score, Temperature.Score, HRV.Balance.Score,
         Recovery.Index.Score, Meet.Daily.Targets.Score, tavg, tsun, 
         Respiratory.Rate, Previous.Day.Activity.Score, date_numeric) %>%
  rename_with(~ german_names[.], everything())

# Umwandeln ins lange Format
data_long <- selected_vars %>%
  pivot_longer(cols = -Schlafwert, names_to = "Variable", values_to = "Value")

# Facettenplot erstellen und als PDF speichern
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

# Deutsche Namen für die Variablen (inklusive "Datum")
german_names <- c(
  "Atemfrequenz", 
  "Datum", 
  "Durchschnittstemperatur", 
  "Erholungsindex", 
  "Herzfrequenzvariabilitätsindex", 
  "Körpertemperaturindex", 
  "Ruheherzfrequenzindex", 
  "Sonnenscheindauer", 
  "Vortagsaktivitätsindex",
  "Luftdruck"
)

# Wähle die relevanten Variablen aus (plus "Sleep.Score")
selected_vars <- data_imputed_jc_clean %>%
  select(Sleep.Score, Respiratory.Rate, date_numeric, tavg, 
         Recovery.Index.Score, HRV.Balance.Score, Temperature.Score, Resting.Heart.Rate.Score, 
         tsun, Previous.Day.Activity.Score, pres)

# Umwandeln ins lange Format
data_long <- selected_vars %>%
  pivot_longer(cols = -Sleep.Score, names_to = "Variable", values_to = "Value")

# Variablen und Titel zuweisen
data_long$Variable <- factor(data_long$Variable, 
                             levels = unique(data_long$Variable),
                             labels = german_names)

# Facettenplot erstellen
final_plot <- ggplot(data_long, aes(x = Value, y = Sleep.Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  facet_wrap(~ Variable, scales = "free_x", ncol = 3) + # 3 Spalten
  labs(
    title = "Zusammenhang zwischen Schlafwert und Einflussgrößen",
    x = "Wert der Einflussgröße",
    y = "Schlafwert"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 13)
  )

# Speichern als PDF
pdf("Zusammenhang_Schlafwert_Einflussgrößen.pdf", width = 14, height = 16)
print(final_plot)
dev.off()




# Analyse der Verteilung des Sleep Scores
# Dichte-Plot erstellen und speichern
pdf("Dichteplot_Sleep_Score.pdf", width = 8, height = 6)

ggplot(data_imputed_jc_clean, aes(x = Sleep.Score)) +
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


# Speichern des Boxplots als PDF
pdf("boxplot_sleep_score.pdf", width = 8, height = 6) # Breite und Höhe für gute Lesbarkeit
ggplot(data_imputed_jc_clean, aes(y = Sleep.Score)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot des Schlafwerts (Sleep Score)",
       y = "Sleep Score") +
  theme_minimal()
dev.off()


library(ggplot2)
# Speichern des Q-Q-Plots als PDF
pdf("qqplot_sleep_score.pdf", width = 8, height = 6)  # Breite und Höhe für gute Lesbarkeit
ggplot(data_imputed_jc_clean, aes(sample = Sleep.Score)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q-Plot des Schlafwerts",
       x = "Theoretische Quantile",
       y = "Beobachtete Quantile") +
  theme_minimal()
dev.off()


