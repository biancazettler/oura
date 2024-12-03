# Lade vorbereitende Daten und Abhängigkeiten
source("R/Datenvorbereitung.R")
# Lade notwendige Bibliotheken
library(corrplot) # Für die Visualisierung von Korrelationen
library(car)      # Für VIF-Berechnungen
library(ggplot2)  # Für Plots

################### VARIABLENSELEKTION ###################
################### KORRELATIONSANALYSE ###################

# Korrelation zwischen Sleep.Score und anderen Variablen
cor_matrix_sleep <- cor(data_imputed %>%
                          select(Sleep.Score, Total.Sleep.Duration, REM.Sleep.Score, Deep.Sleep.Score,
                                 Light.Sleep.Duration, Sleep.Latency.Score, Total.Sleep.Score,
                                 Sleep.Latency, REM.Sleep.Duration), 
                        use = "complete.obs")

# Zeige nur die Korrelationen mit Sleep.Score
print(cor_matrix_sleep[, "Sleep.Score", drop = FALSE])
print(cor_matrix_sleep)

### Korrelationen
# Entferne alle Spalten mit "Schlaf" im Namen, außer "Sleep.Score", da dies die Zielgröße (ZG) ist
data_clean <- data_imputed %>% rename(schlaf = Sleep.Score)
data_clean <- data_clean %>% select(-matches("Sleep"))
data_clean <- data_clean %>% rename(Sleep.Score = schlaf)

# Korrelationsanalyse
library(corrplot)
numeric_columns <- sapply(data_clean, is.numeric)
cor_num <- cor(data_clean[, numeric_columns], use = "complete.obs")

# Zeige die Korrelationswerte für Sleep.Score
print(cor_num[, "Sleep.Score", drop = FALSE])

# Speichere den Korrelationsplot als PDF
pdf("correlation_plot_all.pdf", width = 12, height = 12)
corrplot(cor_num, method = "circle", tl.cex = 0.8, tl.col = "black")
dev.off()

# Grafik anzeigen lassen
corrplot(cor_num, method = "circle", tl.cex = 0.8, tl.col = "black")


################### VISUALISIERUNG DER WICHTIGSTEN KORRELATIONEN ###################

# Berechnung der wichtigsten Korrelationen
correlations <- data_imputed %>%
  select(Sleep.Score, Resting.Heart.Rate.Score, Respiratory.Rate,
         HRV.Balance.Score, Recovery.Index.Score, Previous.Day.Activity.Score,
         tavg, tsun, date_numeric, Temperature.Score) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  select(Sleep.Score) %>%
  rownames_to_column(var = "Variable") %>%
  filter(Variable != "Sleep.Score") %>%
  rename(Correlation = Sleep.Score) %>%
  arrange(desc(abs(Correlation)))

# Variablen auf Deutsch umbenennen
variable_labels <- c(
  "Resting.Heart.Rate.Score" = "Ruheherzfrequenzindex",
  "Respiratory.Rate" = "Atemfrequenz",
  "HRV.Balance.Score" = "Herzfrequenzvariabilitätsindex",
  "Recovery.Index.Score" = "Erholungsindex",
  "Previous.Day.Activity.Score" = "Vortagsaktivitätsindex",
  "tavg" = "Durchschnittstemperatur",
  "tsun" = "Sonnenscheindauer",
  "date_numeric" = "Datum",
  "Temperature.Score" = "Körpertemperaturindex"
)

correlations$Variable <- factor(correlations$Variable, levels = names(variable_labels), labels = variable_labels)

# Plot erstellen
correlation_plot <- ggplot(correlations, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", aes(fill = Correlation), width = 0.8) +
  coord_flip() +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkorange", midpoint = 0) +
  theme_minimal() +
  labs(
    title = "Korrelationen mit dem Schlafwert",
    x = "Variablen",
    y = "Korrelationskoeffizient",
    fill = "Korrelation"
  )

# Plot speichern
ggsave("correlation_plot.png", plot = correlation_plot, width = 12, height = 8, dpi = 300)
print(correlation_plot)


### Multikollinearitätsprüfung
library(car)
head(data_clean)
# Lineares Modell zur Berechnung von VIF erstellen
vif_model <- lm(Sleep.Score ~ ., data = data_clean[, c("Sleep.Score","date_numeric", 
                                                       "Resting.Heart.Rate.Score", 
                                                       "Respiratory.Rate", 
                                                       "HRV.Balance.Score", 
                                                       "Recovery.Index.Score", 
                                                       "Temperature.Score", 
                                                       "Previous.Day.Activity.Score", 
                                                       "Meet.Daily.Targets.Score",
                                                       "tavg", "tsun")])

# VIF-Werte berechnen
vif_values <- vif(vif_model)
print(vif_values)

### LASSO Regularisierung
library(glmnet)

# Daten vorbereiten
x <- as.matrix(data_clean[, c("date_numeric", "Resting.Heart.Rate.Score", 
                              "Respiratory.Rate", "HRV.Balance.Score", 
                              "Recovery.Index.Score", "Temperature.Score", 
                              "Previous.Day.Activity.Score", 
                              "Meet.Daily.Targets.Score", "tavg", "tsun")])
y <- data_clean$Sleep.Score

# Fehlende Werte in x durch Spaltenmittelwert ersetzen
x[is.na(x)] <- colMeans(x, na.rm = TRUE)[col(x)[is.na(x)]]

# Lasso-Modell erstellen
lasso_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- lasso_model$lambda.min
print(paste("Bestes Lambda:", best_lambda))

# Lasso-Koeffizienten anzeigen
lasso_coefficients <- coef(lasso_model, s = best_lambda)
print("Lasso-Koeffizienten:")
print(lasso_coefficients)
