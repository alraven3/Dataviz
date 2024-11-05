library(tidyverse)
library(cowplot)
library(readr)
library(usdatasets)
library(hrbrthemes)
library(patchwork)
library(ggtext)
library(stringr)

#data preparation

cia_factbook <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-22/cia_factbook.csv')


cia_factbook <- usdatasets::cia_factbook_tbl_df |> 
  dplyr::mutate(
    dplyr::across(
      c("area", "internet_users"),
      as.integer
    )
  )


cia_factbook$int_users_per_km2 = cia_factbook$internet_users/cia_factbook$area
cia_factbook$percent_internet_users = (cia_factbook$internet_users*100)/cia_factbook$population

data <- na.omit(cia_factbook)

data$int_users_per_km2 <- round(data$int_users_per_km2, digits=2)
data$percent_internet_users <- round(data$percent_internet_users, digits=2)


all <- readr::read_csv('https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/refs/heads/master/all/all.csv')


colnames(all)[colnames(all) == 'name'] <- 'country'
colnames(all)[colnames(all) == 'sub-region'] <- 'sub_region'

country_mapping <- c("Russian Federation" = "Russia",
                     "United States of America" = "United States",
                     "Iran, Islamic Republic of" = "Iran",
                     "Bolivia, Plurinational State of"="Bolivia",
                     "Tanzania, United Republic of"="Tanzania",
                     "Venezuela, Bolivarian Republic of"="Venezuela",
                     "Myanmar"="Burma",
                     "Congo"="Congo, Republic of the",
                     "Viet Nam"="Vietnam",
                     "Côte d'Ivoire"="Cote d'Ivoire",
                     "United Kingdom of Great Britain and Northern Ireland"="United Kingdom",
                     "Syrian Arab Republic"="Syria",
                     "Korea, Republic of"="Korea, South",
                     "Czechia"="Czech Republic",
                     "Netherlands, Kingdom of the"="Netherlands",
                     "Moldova, Republic of"="Moldova",
                     "North Macedonia"="Macedonia",
                     "Bahamas"="Bahamas, The",
                     "Gambia"="Gambia, The",
                     "Brunei Darussalam"="Brunei")

all <- all %>%
  mutate(country = recode(country, !!!country_mapping))

df <- data %>%
  left_join(all %>% select(country, region, sub_region), by = "country")

df <- df %>%
  mutate(
    region = ifelse(country == "Turkey", "Europe", region),
    sub_region = ifelse(country == "Turkey", "Southern Europe", sub_region)
  ) %>%
  mutate(
    region = ifelse(country == "Gaza Strip", "Asia", region),
    sub_region = ifelse(country == "Gaza Strip", "Middle East", sub_region)
  ) %>%
  mutate(
    region = ifelse(country == "West Bank", "Asia", region),
    sub_region = ifelse(country == "West Bank", "Middle East", sub_region)
  ) %>%
  mutate(
    region = ifelse(country == "Laos", "Asia", region),
    sub_region = ifelse(country == "Laos", "South-eastern Asia", sub_region)
  ) %>%
  mutate(
    region = ifelse(country == "Swaziland", "Africa", region),
    sub_region = ifelse(country == "Swaziland", "Southern Africa", sub_region)
  ) %>%
  mutate(sub_region = if_else(sub_region == "Western Asia", "Middle East", sub_region))%>%
  mutate(sub_region = if_else(sub_region == "Latin America and the Caribbean", "Latin America", sub_region))%>%
  mutate(sub_region = if_else(sub_region == "Australia and New Zealand", "Australasia", sub_region))


# Calculate median values by sub-region

median_internet_stats <- df %>%
  group_by(sub_region) %>%
  summarise(
    median_users_per_km2 = median(int_users_per_km2, na.rm = TRUE),
    median_percent_users = median(percent_internet_users, na.rm = TRUE)
  )


# Plotting

annotation1 = "Bar colors represent the median"
annotation2= "percent of internet users:"
annotation3 = "the greener, the closer to 100%"
annotation4 = "According to the UN, almost half of the world's"
annotation5 = "population, namely the 3.7 billion people"
annotation6 = "living in the poor countries of the Global South,"
annotation7 = "lack access to the internet!"


p <- ggplot(median_internet_stats, aes(x = median_users_per_km2, y = reorder(sub_region, median_users_per_km2), 
                                        fill = median_percent_users)) +
  geom_bar(stat = "identity", width = .6,linewidth=0) +
  scale_fill_gradient(low = "black", high = "seagreen1", name = "Median Percent of Internet Users") +  # Gradient colors
  labs(
    x = "Median Internet Users per Square Kilometer (km²)",
    caption = "Visualization: Alexandre Bovey\nData: CIA Factbook, UN") +
  annotate("text", x = 68, y = 14, label = annotation1, size=4.5, color = "black",family="Nunito",hjust=0.1)+
  annotate("text", x = 68, y = 13, label = annotation2, size=4.5, color = "black",family="Nunito",hjust=0.13)+
  annotate("text", x = 68, y = 12, label = annotation3, size=4.5, color = "seagreen3",family="Nunito",hjust=0.1)+
  annotate("text", x = 47, y = 7, label = annotation4, size=4.5, color = "black",family="Nunito",hjust=0.1)+
  annotate("text", x = 45, y = 6, label = annotation5, size=4.5, color = "black",family="Nunito",hjust=0.1)+
  annotate("text", x = 47, y = 5, label = annotation6, size=4.5, color = "black",family="Nunito",hjust=0.1)+
  annotate("text", x = 42, y = 4, label = annotation7, size=4.5, color = "black",family="Nunito",hjust=0.1)+
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", size = 12),
        plot.background = element_rect(color="#fee8c8", fill="#fee8c8"),
        panel.grid.major = element_line(color="darkgrey",size = 0.25,linetype = 2),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Nunito"),
        plot.caption.position = "plot",
        legend.position="none",
        axis.text = element_text(size = 11, color = "black"),
        plot.caption = element_text(hjust=0,size=11, color="black", lineheight = 1.2),
        plot.margin = margin(t = 40, r = 5, b = 10, l = 7))

ggdraw() + 
  draw_plot(p) + 
  draw_label("TidyTuesday 2024, #43: Internet Users by Country", x = 0.5, y = .97, hjust = 0.5, vjust = 1, fontfamily = "Viga", size = 18)

Annotation1 = "Bar colors represent the mean"

p2 <- ggplot(mean_internet_stats, aes(x = mean_users_per_km2, y = reorder(sub_region, mean_users_per_km2), 
                                       fill = mean_percent_users)) +
  geom_bar(stat = "identity", width = .6,linewidth=0) +
  scale_fill_gradient(low = "black", high = "seagreen1", name = "Mean Percent of Internet Users") +  # Gradient colors
  labs(
    x = "Mean Internet Users per Square Kilometer (km²)",
    caption = "Visualization: Alexandre Bovey\nData: CIA Factbook, UN") +
  annotate("text", x = 200, y = 14, label = Annotation1, size=4.5, color = "black",family="Nunito",hjust=0.1)+
  annotate("text", x = 200, y = 13, label = annotation2, size=4.5, color = "black",family="Nunito",hjust=0.13)+
  annotate("text", x = 200, y = 12, label = annotation3, size=4.5, color = "seagreen3",family="Nunito",hjust=0.1)+
  annotate("text", x = 100, y = 7, label = annotation4, size=4.5, color = "black",family="Nunito",hjust=0.1)+
  annotate("text", x = 98, y = 6, label = annotation5, size=4.5, color = "black",family="Nunito",hjust=0.1)+
  annotate("text", x = 100, y = 5, label = annotation6, size=4.5, color = "black",family="Nunito",hjust=0.1)+
  annotate("text", x = 90, y = 4, label = annotation7, size=4.5, color = "black",family="Nunito",hjust=0.1)+
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", size = 12),
        plot.background = element_rect(color="#fee8c8", fill="#fee8c8"),
        panel.grid.major = element_line(color="darkgrey",size = 0.25,linetype = 2),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(family = "Nunito"),
        plot.caption.position = "plot",
        legend.position="none",
        axis.text = element_text(size = 11, color = "black"),
        plot.caption = element_text(hjust=0,size=11, color="black", lineheight = 1.2),
        plot.margin = margin(t = 40, r = 5, b = 10, l = 7))

ggdraw() + 
  draw_plot(p2) + 
  draw_label("TidyTuesday 2024, #43: Internet Users by Country", x = 0.5, y = .97, hjust = 0.5, vjust = 1, fontfamily = "Viga", size = 18)
