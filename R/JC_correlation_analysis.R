#source("R/BZ_pre_processing.R")
source("R/JC_pre_processing.R")

# correlation analysis
# 1. data with scores and no na's


cor_scores <- cor(scores_data_imputed_jc, use = "complete.obs")
print(cor_scores)

library(corrplot)
corrplot(cor_scores, method = "circle")

# use fewer scores 

main_scores <- scores_data_imputed_jc %>%
  select(Sleep.Score, Activity.Score, HRV.Balance.Score, Temperature.Score,
         Resting.Heart.Rate.Score)

cor_scores_main <- cor(main_scores, use = "complete.obs")
print(cor_scores_main)

corrplot(cor_scores_main, method = "circle")

# 2. data with scores and imputed nas

cor_scores <- cor(scores_imputed_nas_jc, use = "complete.obs")
print(cor_scores)

library(corrplot)
corrplot(cor_scores, method = "circle")

# use fewer scores 

main_scores_imputed <- scores_imputed_nas_jc %>%
  select(Sleep.Score, Activity.Score, HRV.Balance.Score, Temperature.Score,
         Resting.Heart.Rate.Score)

cor_scores_main_im <- cor(main_scores_imputed, use = "complete.obs")
print(cor_scores_main_im)

corrplot(cor_scores_main_im, method = "circle")

# Korrelation zwischen Sleep.Score und anderen Variablen mit daten mit imputierten nas
cor_matrix_sleep <- cor(data_imputed_jc %>% select(Sleep.Score, Total.Sleep.Duration, REM.Sleep.Score,
                                           Deep.Sleep.Score, Light.Sleep.Duration,
                                           Sleep.Latency.Score, Total.Sleep.Score, Sleep.Latency,
                                           REM.Sleep.Duration), use = "complete.obs")
print(# Zeigt nur die Spalte "Sleep.Score" mit allen Zeilennamen
  cor_matrix_sleep[, "Sleep.Score", drop = FALSE]
)
print(cor_matrix_sleep)

# (relativ) hohe korrelation von sleep score mit total sleep duration, rem sleep score,
# deep sleep score, total sleep score, rem sleep duration und rem sleep score -> 
# sleep score aussagekräftig für die gesamt schlafqualität als zielgröße

# 3. with data with na's

# Korrelationen berechnen und anzeigen
cor_matrix <- cor(data_jc %>% select(Sleep.Score, Activity.Score, Average.Resting.Heart.Rate, Average.HRV, Temperature.Deviation...C.), use = "complete.obs")
print(cor_matrix)

# Korrelationen visualisieren (mit ggplot2 oder corrplot)
library(corrplot)
corrplot(cor_matrix, method = "circle")

# Berechnung der Korrelationen zwischen Sleep.Score und potenziellen Prädiktoren
cor_matrix_prädiktoren <- cor(data_imputed_jc %>% 
                                select(Sleep.Score, Activity.Score, Total.Burn, Steps, 
                                       Average.Resting.Heart.Rate, Lowest.Resting.Heart.Rate, 
                                       Average.HRV, Temperature.Deviation...C.), 
                              use = "complete.obs")
print(cor_matrix_prädiktoren)

# Optional: Visualisierung der Korrelationen
library(corrplot)
# Berechnung der Korrelationen zwischen den Prädiktoren
cor(data_imputed_jc[, c("Average.Resting.Heart.Rate", "Lowest.Resting.Heart.Rate", "Average.HRV")])

# VIF-Werte berechnen
library(car)
vif_model <- lm(sqrt_Sleep_Score ~ Average.Resting.Heart.Rate + Lowest.Resting.Heart.Rate + Average.HRV, 
                data = data_imputed_jc)
vif(vif_model)

# zu hohe Korrelation zwischen lowest resting heart rate und average resting heart rate 
# -> nur eins aufnehmen:
# Berechnung der Korrelationen zwischen den Prädiktoren
cor(data_imputed_jc[, c("Average.Resting.Heart.Rate", "Average.HRV")])

# VIF-Werte berechnen
vif_model_easy <- lm(sqrt_Sleep_Score ~ Average.Resting.Heart.Rate + Average.HRV, 
                data = data_imputed_jc)
vif(vif_model_easy)
# korrelation ist ok

################ daten mit imputierten werten:

# Korrelation aller Daten (für ZG Sleep.Score)
# für Korrelationsanalyse: alle Werte in numerische umwandeln
# Konvertiere die Spalte `date` in das Datumsformat
data_imputed_jc$date <- as.Date(data_imputed_jc$date, format = "%Y-%m-%d")
# Konvertiere `Bedtime.Start` und `Bedtime.End` in das POSIXct-Datumszeitformat
data_imputed_jc$Bedtime.Start <- as.POSIXct(data_imputed_jc$Bedtime.Start, format = "%Y-%m-%dT%H:%M:%S")
data_imputed_jc$Bedtime.End <- as.POSIXct(data_imputed_jc$Bedtime.End, format = "%Y-%m-%dT%H:%M:%S")

# Umwandlung von `date` in die Anzahl der Tage seit einem festen Startdatum
start_date <- as.Date("2021-02-09")
data_imputed_jc$date_numeric <- as.numeric(data_imputed_jc$date - start_date)

# Optional: Extrahiere Wochentag oder Monat
data_imputed_jc$weekday <- as.numeric(format(data_imputed_jc$date, "%w"))  # 0 = Sonntag, 1 = Montag, ...
data_imputed_jc$month <- as.numeric(format(data_imputed_jc$date, "%m"))

# Extrahiere die Stunde als numerischen Wert
data_imputed_jc$Bedtime.Start_numeric <- as.numeric(format(data_imputed_jc$Bedtime.Start, "%H")) + 
  as.numeric(format(data_imputed_jc$Bedtime.Start, "%M")) / 60
data_imputed_jc$Bedtime.End_numeric <- as.numeric(format(data_imputed_jc$Bedtime.End, "%H")) + 
  as.numeric(format(data_imputed_jc$Bedtime.End, "%M")) / 60


data_imputed_jc[] <- lapply(data_imputed_jc, function(x) {
  if (is.character(x) | is.factor(x)) {
    # Konvertiere in numerisch (NA bei Fehlern)
    as.numeric(as.character(x))
  } else {
    x # Lasse den ursprünglichen Wert, wenn schon numerisch
  }
})
# Datentypen aller Spalten anzeigen
str(data_imputed_jc)


# Führe die Korrelationsanalyse mit den numerischen Variablen durch
numeric_columns <- sapply(data_imputed_jc, is.numeric)
cor_num <- cor(data_imputed_jc[, numeric_columns], use = "complete.obs")
print(cor_num)
print(# Zeigt nur die Spalte "Sleep.Score" mit allen Zeilennamen
  cor_num[, "Sleep.Score", drop = FALSE]
)

library(corrplot)
corrplot(cor_num, method = "circle")

# Plot der wichtigsten Korrelationen:

# Korrelationen berechnen
correlations <- data_imputed_jc_clean %>%
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

# Zuordnung der deutschen Namen
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

# Variablen-Namen ersetzen
correlations$Variable <- factor(correlations$Variable, levels = names(variable_labels), labels = variable_labels)

# Plot erstellen
correlation_plot <- ggplot(correlations, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", aes(fill = Correlation), width = 0.8) +
  coord_flip() +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkorange", midpoint = 0) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1), limits = c(-0.5, 0.5)) +
  theme_minimal() +
  labs(
    title = "Korrelationen mit dem Schlafwert",
    x = "Variablen",
    y = "Korrelationskoeffizient mit dem Schlafwert",
    fill = "Korrelation"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

# Plot anzeigen
print(correlation_plot)

# Speichere den Plot als PNG
png("correlation_plot.png", width = 1200, height = 800, res = 150)
print(correlation_plot) # Ersetzt 'correlation_plot' durch den Namen deines Plots
dev.off()

# Speichere den Plot als PDF
pdf("correlation_plot.pdf", width = 10, height = 7)  # Breite und Höhe des Plots
print(correlation_plot)  # Ersetze 'correlation_plot' durch den Namen deines Plots
dev.off()  # Schließt die Datei und speichert das PDF

# Plot der wichtigsten Korrelationen für zv atemfrequenz:

# Korrelationen berechnen
correlations_resp <- data_imputed_jc_clean %>%
  select(Temperature.Score, Resting.Heart.Rate.Score, Respiratory.Rate,
         HRV.Balance.Score, Recovery.Index.Score, pres,
         tavg, tsun, date_numeric) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  select(Respiratory.Rate) %>%
  rownames_to_column(var = "Variable") %>%
  filter(Variable != "Respiratory.Rate") %>%
  rename(Correlation = Respiratory.Rate) %>%
  arrange(desc(abs(Correlation)))

# Zuordnung der deutschen Namen
variable_labels_resp <- c(
  "Resting.Heart.Rate.Score" = "Ruheherzfrequenzindex",
  "HRV.Balance.Score" = "Herzfrequenzvariabilitätsindex",
  "Recovery.Index.Score" = "Erholungsindex",
  "pres" = "Luftdruck",
  "tavg" = "Durchschnittstemperatur",
  "tsun" = "Sonnenscheindauer",
  "date_numeric" = "Datum",
  "Temperature.Score" = "Körpertemperaturindex"
)

# Variablen-Namen ersetzen
correlations_resp$Variable <- factor(correlations_resp$Variable, levels = names(variable_labels_resp), labels = variable_labels_resp)

# Plot erstellen
correlation_plot_resp <- ggplot(correlations_resp, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", aes(fill = Correlation), width = 0.8) +
  coord_flip() +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkorange", midpoint = 0) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.1), limits = c(-0.5, 0.5)) +
  theme_minimal() +
  labs(
    title = "Korrelationen mit der Atemfrequenz",
    x = "Variablen",
    y = "Korrelationskoeffizient mit der Atemfrequenz",
    fill = "Korrelation"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

# Plot anzeigen
print(correlation_plot_resp)

# Speichere den Plot als PNG
png("correlation_plot_resp.png", width = 1200, height = 800, res = 150)
print(correlation_plot_resp) # Ersetzt 'correlation_plot' durch den Namen deines Plots
dev.off()

# Speichere den Plot als PDF
pdf("correlation_plot_resp.pdf", width = 10, height = 7)  # Breite und Höhe des Plots
print(correlation_plot_resp)  # Ersetze 'correlation_plot' durch den Namen deines Plots
dev.off()  # Schließt die Datei und speichert das PDF
