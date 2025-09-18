
source("Sol_27_08_2025.R")

#I. Plotting FPro

library(sf)
library(ggplot2)
library(rnaturalearth)
library(readr)

results_df2 <- data3[, c("Country", "Year", "FPro")]

library(dplyr)

country_fit <- results_df2 %>%
  group_by(Country) %>%
  summarise(
    mean_FPro = mean(as.numeric(FPro), na.rm = TRUE),
    n_years = n()
  )

# Load shapefile of the world
world <- ne_countries(scale = "medium", returnclass = "sf")

# Calculate centroids (optional if you still want annotations later)
world$centroid <- st_centroid(world$geometry)
coords <- st_coordinates(world$centroid)
world$longitude <- coords[, "X"]
world$latitude  <- coords[, "Y"]

# Subset Europe
europe_clean <- subset(
  world,
  name %in% c(unique(world$name[world$continent == "Europe"]), "Cyprus") &
    longitude > -22 & longitude < 46 &
    latitude > 32 & latitude < 70
)

C <- country_fit[,-3]
colnames(C) <- c("name", "FPro")

C$name[C$name == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
C$name[C$name == "United Kingdom"] <- "United Kingdom"  # already fine
setdiff(C$name, europe_clean$name)
europe_clean <- merge(europe_clean, C, by = "name", all.x = TRUE)

ggplot(europe_clean) +
  geom_sf(aes(fill = FPro), color = "black") +
  scale_fill_gradient(
    name = "FPro",
    low = "#CCE5FF",  # light blue
    high = "#003366",   # dark blue
    na.value = "gray90",
    limits = c(0.58, 0.68),
    breaks = seq(0.58, 0.68, by = 0.02)
  ) +
  coord_sf(xlim = c(-22, 46), ylim = c(32, 70), expand = FALSE) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(size=20, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 17),
  ) +
  labs(
    title = "Country-level FPro (EFSA-derived)",
    x = "Longitude",
    y = "Latitude"
  )


ggsave("map_FPro.png", width = 10, height = 8, units = "in", dpi = 600, bg = "white")




#Anaemia I. Plotting Anaemia prevalence

library(sf)
library(ggplot2)
library(rnaturalearth)
library(readr)

results_df2 <- data3[, c("Country", "Year", "Value")]

library(dplyr)

country_fit <- results_df2 %>%
  group_by(Country) %>%
  summarise(
    mean_anaemia = mean(as.numeric(Value), na.rm = TRUE),
    n_years = n()
  )

# Load shapefile of the world
world <- ne_countries(scale = "medium", returnclass = "sf")

# Calculate centroids (optional if you still want annotations later)
world$centroid <- st_centroid(world$geometry)
coords <- st_coordinates(world$centroid)
world$longitude <- coords[, "X"]
world$latitude  <- coords[, "Y"]

# Subset Europe
europe_clean <- subset(
  world,
  name %in% c(unique(world$name[world$continent == "Europe"]), "Cyprus") &
    longitude > -22 & longitude < 46 &
    latitude > 32 & latitude < 70
)

C <- country_fit[,-3]
colnames(C) <- c("name", "Anaemia")

C$name[C$name == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
C$name[C$name == "United Kingdom"] <- "United Kingdom"  # already fine
setdiff(C$name, europe_clean$name)
europe_clean <- merge(europe_clean, C, by = "name", all.x = TRUE)

ggplot(europe_clean) +
  geom_sf(aes(fill = Anaemia), color = "black") +
  scale_fill_gradient(
    name = "Anaemia",
    low = "#FFCCCC",  # light red
    high = "#990000", # dark red
    na.value = "gray90",
    limits = c(8, 24),
    breaks = seq(8, 24, by = 4),
  ) +
  coord_sf(xlim = c(-22, 46), ylim = c(32, 70), expand = FALSE) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(size=20, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 17),
  ) +
  labs(
    title = "Country-level Anaemia (in %)",
    x = "Longitude",
    y = "Latitude"
  )


ggsave("map_Anaemia_27_08_2025.png", width = 10, height = 8, units = "in", dpi = 600, bg = "white")



#Model III. Predicted Anaemia prevalence

model <- lm(Value ~ FPro+Age+Underweight+Obesity+Overweight+Growth_GNI+GDP+Pop_Growth, data = data3,weights = Tot_Pop) #i.e. ALL 27_08_2025_confounders
#model <- lm(Value ~ FPro, data = data3,weights = Tot_Pop) #i.e. FPro
#model <- lm(Value ~ FPro + Pop_Growth, data = data3,weights = Tot_Pop)
data3$predicted <- predict(model)
data3$abs_error <- abs(as.numeric(data3$Value) - data3$predicted)
data3$rel_error <- data3$abs_error / as.numeric(data3$Value)

results_df2 <- data3[, c("Country", "Year", "Value", "predicted", "abs_error","rel_error")]

library(dplyr)

country_fit <- data3 %>%
  group_by(Country) %>%
  summarise(
    mean_predicted = mean(predicted, na.rm = TRUE),
    n_years = n()
  )


library(sf)
library(ggplot2)
library(rnaturalearth)
library(readr)

# Load shapefile of the world
world <- ne_countries(scale = "medium", returnclass = "sf")

# Calculate centroids (optional if you still want annotations later)
world$centroid <- st_centroid(world$geometry)
coords <- st_coordinates(world$centroid)
world$longitude <- coords[, "X"]
world$latitude  <- coords[, "Y"]
# Subset Europe
europe_clean <- subset(
  world,
  name %in% c(unique(world$name[world$continent == "Europe"]), "Cyprus") &
    longitude > -22 & longitude < 46 &
    latitude > 32 & latitude < 70
)

C <- country_fit[,-3]
colnames(C) <- c("name", "mean_predicted")

C$name[C$name == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
C$name[C$name == "United Kingdom"] <- "United Kingdom"  # already fine
setdiff(C$name, europe_clean$name)
europe_clean <- merge(europe_clean, C, by = "name", all.x = TRUE)
ggplot(europe_clean) +
  geom_sf(aes(fill = mean_predicted), color = "black") +
  scale_fill_gradient(
    name = "Pred. An.",
    low = "#DEB887",  # light brown
    high = "#654321", # dark brown
    na.value = "gray90"
  ) +
  coord_sf(xlim = c(-22, 46), ylim = c(32, 70), expand = FALSE) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(size=20, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 17),
  ) +
  labs(
    title = "Country-level prediction of Anaemia (in %)",
    x = "Longitude",
    y = "Latitude"
  )



ggsave("map_Anaemia_Prediction_all_27_08_2025_confounders.png", width = 10, height = 8, units = "in", dpi = 400, bg = "white")




#Model IV. MAE for ALL 27_08_2025_confounders - light orange

model <- lm(Value ~ FPro+Age+Underweight+Obesity+Overweight+Growth_GNI+GDP+Pop_Growth, data = data3,weights = Tot_Pop) #i.e. ALL 27_08_2025_confounders
#model <- lm(Value ~ FPro, data = data3,weights = Tot_Pop) #i.e. FPro
#model <- lm(Value ~ FPro + Pop_Growth, data = data3,weights = Tot_Pop)
data3$predicted <- predict(model)
data3$abs_error <- abs(as.numeric(data3$Value) - data3$predicted)
data3$rel_error <- data3$abs_error / as.numeric(data3$Value)

results_df2 <- data3[, c("Country", "Year", "Value", "predicted", "abs_error","rel_error")]

library(dplyr)

country_fit <- data3 %>%
  group_by(Country) %>%
  summarise(
    mean_abs_error = mean(abs_error, na.rm = TRUE),
    n_years = n()
  )


library(sf)
library(ggplot2)
library(rnaturalearth)
library(readr)

# Load shapefile of the world
world <- ne_countries(scale = "medium", returnclass = "sf")

# Calculate centroids (optional if you still want annotations later)
world$centroid <- st_centroid(world$geometry)
coords <- st_coordinates(world$centroid)
world$longitude <- coords[, "X"]
world$latitude  <- coords[, "Y"]
# Subset Europe
europe_clean <- subset(
  world,
  name %in% c(unique(world$name[world$continent == "Europe"]), "Cyprus") &
    longitude > -22 & longitude < 46 &
    latitude > 32 & latitude < 70
)

C <- country_fit[,-3]
colnames(C) <- c("name", "MAE")

C$name[C$name == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
C$name[C$name == "United Kingdom"] <- "United Kingdom"  # already fine
setdiff(C$name, europe_clean$name)
europe_clean <- merge(europe_clean, C, by = "name", all.x = TRUE)
ggplot(europe_clean) +
  geom_sf(aes(fill = MAE), color = "black") +
  scale_fill_gradient(
    name = "MAE (%)",
     low = "#FFE5B4",   # light orange
    high = "#CC6600" ,  # dark orange
    na.value = "gray90"
  ) +
  coord_sf(xlim = c(-22, 46), ylim = c(32, 70), expand = FALSE) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(size=20, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 17),
  ) +
  labs(
    title = "Country-level MAE of residuals for pred. of Anaemia",
    x = "Longitude",
    y = "Latitude"
  )

ggsave("map_MAE_Anaemia_ORANGE_all_27_08_2025_confounders.png", width = 10, height = 8, units = "in", dpi = 400, bg = "white")



