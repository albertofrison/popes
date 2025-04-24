# Carichiamo le librerie necessarie
library(readr)      # per leggere i file CSV
library(dplyr)      # per manipolazione dati
library(ggplot2)    # per la visualizzazione
library(lubridate)  # per la gestione delle date

# Scarichiamo il dataset dei papi da GitHub
url <- "https://raw.githubusercontent.com/ksreyes/popes/main/popes.csv"
download.file(url, destfile = "popes.csv", mode = "wb")

# Carichiamo il file CSV in un dataframe
popes <- read_csv("popes.csv")

# Mettiamo la data di morte di Papa Francesco
popes[267,]$end <- as.Date("2025-04-21")
popes[267,]$age_end <- as.numeric(difftime(as.Date("2025-04-21"), popes[267,]$birth, units = "days")) / 365.25
#popes[267,]$tenure <- as.numeric(difftime(as.Date("2025-04-21"), popes[267,]$birth, units = "days")) / 365.25

# Convertiamo le date in formato Date
popes <- popes %>%
  mutate(
    start = as.Date(start, format = "%Y-%m-%d"),
    end = as.Date(end, format = "%Y-%m-%d")
  )

# Calcoliamo la durata del pontificato in anni (arrotondata a una cifra decimale)
popes <- popes %>%
  mutate(duration_years = round(as.numeric(difftime(end, start, units = "days")) / 365.25, 1))

# Creiamo il grafico con colore in base allo stato di canonizzazione
ggplot(popes, aes(x = start, y = duration_years, color = canonization)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "gray40", linetype = "dashed") +
  #scale_color_manual(values = c("Canonizzato" = "darkgreen", "Non canonizzato" = "firebrick")) +
  labs(
    title = "Durata dei Pontificati nel Tempo",
    subtitle = "Colorati per stato di canonizzazione",
    x = "Anno di inizio del pontificato",
    y = "Durata del pontificato (anni)",
    color = "Stato di canonizzazione"
  ) +
  theme_minimal(base_size = 14)


# Istogramma della durata dei pontificati
hist(popes$duration_years,
     breaks = 20,
     col = "skyblue",
     border = "white",
     main = "Distribuzione della Durata dei Pontificati",
     xlab = "Durata (anni)",
     ylab = "Numero di papi")
  
