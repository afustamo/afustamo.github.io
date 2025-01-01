# 1 LEZIONE ####

print("Hello world!")

library(httr)

download.file(
  "https://zenodo.org/records/7956006/files/Agrimonia_Dataset_v_3_0_0.Rdata?download=1",
  destfile = "agrimonia_dataset.Rdata"
)

download.file(
  "https://zenodo.org/records/7956006/files/Metadata_monitoring_network_registry_v_2_0_1.csv?download=1",
  destfile = "metadata.csv"
)

load("Agrimonia_Dataset_v_3_0_0.Rdata")
# oppure load("agrimonia_dataset.Rdata") dipende se lo avete scaricato da R

head(AgrImOnIA_Dataset_v_3_0_0)

download.file(
  "https://zenodo.org/records/7956006/files/Metadata_Agrimonia_v_3_0_0.csv?download=1",
  "info.csv"
)

a <- read.csv("info.csv", sep = ";")

table(AgrImOnIA_Dataset_v_3_0_0$IDStations)
table(AgrImOnIA_Dataset_v_3_0_0$LA_land_use)
df <- data.frame(v1 = c("a", "a", "b", "b", "c", "a"))
table(df$v1)

AgrImOnIA_Dataset_v_3_0_0$LA_land_use == "131"

sum(c(1, 2))
sum(AgrImOnIA_Dataset_v_3_0_0$LA_land_use == "131")

AgrImOnIA_Dataset_v_3_0_0$IDStations
unique(AgrImOnIA_Dataset_v_3_0_0$IDStations)

AgrImOnIA_Dataset_v_3_0_0$IDStations[c(1, 2193)]

AgrImOnIA_Dataset_v_3_0_0$IDStations[AgrImOnIA_Dataset_v_3_0_0$LA_land_use ==
                                       "131"]

unique(AgrImOnIA_Dataset_v_3_0_0$IDStations[AgrImOnIA_Dataset_v_3_0_0$LA_land_use ==
                                              "131"])

library(ggplot2)

sub_df <- subset(AgrImOnIA_Dataset_v_3_0_0, IDStations == "576")

plot(sub_df$Time, sub_df$AQ_pm10, type = "l")

ggplot(sub_df) +
  geom_line(aes(x = Time, y = AQ_pm10)) +
  geom_hline(yintercept = 40,
             col = "red",
             linetype = 2)

# quante volte viene superato il limite di WHO ?
sum(sub_df$AQ_pm10 > 40, na.rm = T)

# 2 LEZIONE ####

library(readr)
library(dplyr)

# a <- read.csv("info.csv",sep = ";")
a2 <- read_delim("info.csv", delim = ";")

class(a2)
class(2)
b <- 2
class(b) <- "integer"

# funzioni dplyr più comuni
# - **`select()`**: Seleziona specifiche colonne.
# - **`filter()`**: Filtra le righe in base a una condizione.
# - **`mutate()`**: Crea nuove colonne o modifica quelle esistenti.
# - **`arrange()`**: Ordina le righe in base a una o più colonne.
# - **`summarise()`**: Riassume i dati, solitamente in combinazione con `group_by()`.

# cheat sheet
# https://nyu-cdsc.github.io/learningr/assets/data-transformation.pdf

AgrImOnIA_Dataset_v_3_0_0 %>%
  filter(LA_land_use == 131)

AgrImOnIA_Dataset_v_3_0_0[AgrImOnIA_Dataset_v_3_0_0$LA_land_use == 131, ] #seleziona tutte le colonne

AgrImOnIA_Dataset_v_3_0_0[AgrImOnIA_Dataset_v_3_0_0$LA_land_use == 131, 4] #seleziona la quarta colonna

AgrImOnIA_Dataset_v_3_0_0 %>%
  filter(LA_land_use == 131) %>%
  select(Time)

ggplot(AgrImOnIA_Dataset_v_3_0_0[AgrImOnIA_Dataset_v_3_0_0$LA_land_use ==
                                   131, ]) +
  geom_histogram(aes(y = AQ_pm10))

summary(AgrImOnIA_Dataset_v_3_0_0[AgrImOnIA_Dataset_v_3_0_0$LA_land_use ==
                                    131, 6]) #tutti NA

ggplot(AgrImOnIA_Dataset_v_3_0_0[AgrImOnIA_Dataset_v_3_0_0$IDStations ==
                                   "576", ]) +
  geom_histogram(
    aes(x = AQ_pm10),
    col = "blue",
    fill = "lightblue",
    binwidth = .05
  )

# qual'è il numero di stazioni di Agrimonia
length(unique(AgrImOnIA_Dataset_v_3_0_0$IDStations))

n_NA_df <- AgrImOnIA_Dataset_v_3_0_0 %>%
  group_by(IDStations) %>%
  summarise(n_NA = sum(is.na(AQ_pm10)))

staz_pm10 <- n_NA_df$IDStations[n_NA_df$n_NA != 2192]

avg_pm10_df <- AgrImOnIA_Dataset_v_3_0_0 %>%
  # filter(!is.na(AQ_pm10)) %>%
  filter(IDStations %in% staz_pm10) %>%
  group_by(IDStations) %>%
  summarise(
    avg_pm10 = mean(AQ_pm10, na.rm = T),
    min_pm10 = min(AQ_pm10, na.rm = T),
    var_pm10 = var(AQ_pm10, na.rm = T)
  )

ggplot(AgrImOnIA_Dataset_v_3_0_0[
  AgrImOnIA_Dataset_v_3_0_0$IDStations =="576", ]) +
  geom_boxplot(aes(y = AQ_pm10))

ggplot(AgrImOnIA_Dataset_v_3_0_0[
  AgrImOnIA_Dataset_v_3_0_0$IDStations %in% staz_pm10[1:5], ]) +
  geom_boxplot(aes(y = AQ_pm10, col = IDStations))

# SELEZIONARE UNA STAZIONE TRA QUELLE CHE MISURANO IL PM10

# VISUALIZZARE LA SERIE STORICA DEL PM10 DI QUELLA STAZIONE

# VISUALIZZARE IL BOXPLOT RISPETTO AD UN'ALTRA STAZIONE A CASO

# SELEZIONARE SOLO LE RIGHE LE CUI CONCENTRAZIONI SUPERANO LA MEDIANA
