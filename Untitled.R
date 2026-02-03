install.packages("tidyr")
install.packages(c("tidyverse", "ggridges", "patchwork", "reshape2", "scales"))
library(tidyverse)
library(tidyverse)
library(ggridges)
library(patchwork)
library(reshape2)
library(scales)
my_theme <- theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  )
df <- read_csv("billboard_24years_lyrics_spotify.csv")
clean_df <- df %>%
filter(!is.na(valence), !is.na(loudness), !is.na(energy)) %>%
distinct(song, band_singer, year, .keep_all = TRUE) %>%
mutate(year = as.numeric(year))
print(head(clean_df))
#1
p1 <- clean_df %>%
  group_by(year) %>%
  summarise(mean_valence = mean(valence, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_valence)) +
  geom_line(color = "#2E86C1", size = 1.2) + 
  geom_point(color = "#2E86C1", size = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE, size = 0.5) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Figure 1: The Resilience of Positivity",
    subtitle = "Average Valence shows an upward trend (2000-2023)",
    x = "Year",
    y = "Avg. Valence (0-1)"
  ) +
  my_theme
print(p1)
#2
p2 <- clean_df %>%
  ggplot(aes(x = year, y = loudness)) +
  geom_point(alpha = 0.1, color = "red", size = 1) +
  geom_smooth(method = "loess", color = "#E67E22", size = 1.2, fill = "#E67E22", alpha = 0.2) +
  labs(
    title = "Figure 2: The Loudness War",
    subtitle = "Loudness levels (dB) are consistent, but density shifts",
    x = "Year",
    y = "Loudness (dB)"
  ) +
  my_theme
print(p2)
#3
p3 <- clean_df %>%
  mutate(year_factor = as.factor(year)) %>%
  filter(year %% 2 == 0) %>% 
  ggplot(aes(x = energy, y = year_factor, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Energy", option = "C") +
  labs(
    title = "Figure 3: Evolution of Energy",
    subtitle = "Energy levels remain high and concentrated", 
    x = "Energy (0-1)",
    y = "Year"
  ) +
  my_theme +
  theme(legend.position = "none")
print(p3)
#4
library(tidyverse)
vars <- c("acousticness", "danceability", "energy", "loudness", "valence")
cor_matrix <- clean_df %>%
  select(all_of(vars)) %>%
  cor(use = "complete.obs", method = "pearson")
plot_data <- as.data.frame(cor_matrix) %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "value") %>%
  mutate(
    Var1 = factor(Var1, levels = vars),
    Var2 = factor(Var2, levels = vars)
  )

p4 <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low = "#2C7BB6", mid = "white", high = "#D7191C",
    midpoint = 0, limits = c(-1, 1),
    name = "Correlation (Pearson's r)"
  ) +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  labs(
    title = "Figure 4: Feature Correlation Heatmap",
    subtitle = "Correlations among Spotify audio features (Billboard year-end hits)",
    x = NULL, y = NULL
  ) +
  my_theme +
  coord_fixed() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

print(p4)