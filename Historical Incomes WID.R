#https://r-graph-gallery.com/web-stacked-area-chart-inline-labels.html

library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(hrbrthemes)
library(scales)
library(ggstream)
library(extrafont)
loadfonts(device = "win", quiet=TRUE)
library(stringr)
library(showtext)
library(ggtext)
library(gghighlight)
library(patchwork)
library(RColorBrewer)


WID_income <- read_excel("Datajournalism, viz, ... projects/WID Historical National Income per adult.xlsx")

WID_income <- janitor::row_to_names(WID_income, 1) 

#National Income : Total population | average income or wealth | adults | individual | Euro € | ppp | constant (2023)			

WID_income <- WID_income %>% select(-Percentile)

df <- WID_income %>%
  pivot_longer(cols = -Year, 
               names_to = "Region", 
               values_to = "Income")

df$Income <- as.numeric(df$Income)
df$Income <- round(df$Income, digits=2)

# ggplot(df, aes(x=Year, y=Income,group = Region)) +
#   geom_line()+
#   theme_minimal()

df2 <- df %>%
  group_by(Region) %>%
  filter(!any(is.na(Income))) %>%
  ungroup()

# ggplot(df2, aes(x=Year, y=Income,group = Region)) +
#   geom_line()+
#   theme_minimal()
# 
# ggplot(df2%>%filter(Region != "World"), aes(x=Year, y=Income,group = Region,fill = Region)) +
#   geom_area()+
#   theme_minimal()



df2$Year <- as.numeric(df2$Year)
#sorting the stacked elements based on value in 2023
sorted_df2 <- df2 %>%
  filter(Year==2023) %>%
  arrange(Income)

df2$Region <- factor(df2$Region, levels = sorted_df2$Region)
df2 <- df2%>%filter(Region != "World")

ggplot(, aes(x=Year, y=Income, fill=Region)) + 
  geom_area()+
  labs(title = "theme_ft_rc")+
  theme_ft_rc()+
  scale_y_continuous(labels = label_comma(big.mark = " ",
                                           decimal.mark = "."))

#Color palette
pal=c('#df004f', '#e63f58', '#ec5e62', '#f1786c', '#f69076', '#f9a780', '#fcbd8a', '#fed395', '#ffe99f', '#ffffaa')

# Stacking order
order <- c("Europe","Oceania","Eastern Europe","MENA","East Asia",
           "Russia & Central Asia","Latin America",
           "South & South-East Asia","Sub-Saharan Africa","North America")



font2 <- "Quattrocento Sans"


caption_text  <- str_glue("**Data:**  World Inequality Database<br>",
                          "**Visualization:** Alexandre Bovey <br><br>")

#the smoothed version might look nice but it's not really rigorous/quite misleading
  
df2 %>% 
  arrange(Income) %>%
  mutate(Region = factor(Region, levels=order)) %>% 
  ggplot(aes(Year, Income, fill = Region, label = Region, color = Region)) +
  geom_stream(type = "ridge" ,bw=1) +
  
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(clip = "off")+
  ylab("")+
  xlab("")+
  scale_x_continuous(breaks=c(1820,1850,1900,1950,2000),labels = c("1820","1850","1900","1950","2000"))+
  #Title
  annotate("text", x = 1850, y = 250000,
           label = "Average Income Per Adult\n from 1820 to 2023",
           hjust=0,
           size=7,
           lineheight=.9,
           #fontface="bold", 
           family="Merriweather",
           color="palegreen") +
  #Regions
  annotate("text", x = 2025, y = 330000,
           label = "Europe €40 013",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold", family=font2,
           color=pal[1]) +
  annotate("text", x = 2025, y = 275000,
           label = "Oceania €36 326",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[2]) +
  annotate("text", x = 2025, y = 235000,
           label = "Eastern Europe €26 671",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[3]) +
  annotate("text", x = 2025, y = 205000,
           label = "MENA €23 903",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[4]) +
  annotate("text", x = 2025, y = 170000,
           label = "East Asia €23 060",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[5]) +
  annotate("text", x = 2025, y = 145000,
           label = "Russia & Central Asia €20 852",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[6]) +
  annotate("text", x = 2025, y = 120000,
           label = "Latin America €16 235",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[7]) +
  annotate("text", x = 2025, y = 105000,
           label = "South & South-East Asia €10 322",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[8]) +
  annotate("text", x = 2025, y = 90000,
           label = "Sub-Saharan Africa €6 172",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[9]) +
  annotate("text", x = 2025, y = 50000,
           label = "North America €60 670",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[10])+
  labs(caption = caption_text)+
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(20,150,1,5),
        plot.background = element_rect(fill = '#252A32'),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = '#252A32'),
        axis.text.x = element_text(color = "white",family = font2,size = 10),
        legend.position = "none",
        plot.caption = element_markdown(size = 10,family = font2,hjust = 0,color = "lavender"))

order2 <- c("North America","Europe","Oceania","Eastern Europe","MENA","East Asia",
           "Russia & Central Asia","Latin America",
           "South & South-East Asia","Sub-Saharan Africa")


df2 %>% 
  arrange(Income) %>%
  mutate(Region = factor(Region, levels=order2)) %>% 
  ggplot(aes(Year, Income, fill = Region, label = Region, color = Region)) +
  geom_area() +
  
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(clip = "off")+
  ylab("")+
  xlab("")+
  scale_x_continuous(breaks=c(1820,1850,1900,1950,2000,2023),labels = c("1820","1850","1900","1950","2000","2023"))+
  #Title
  annotate("text", x = 1850, y = 200000,
           label = "Average Income Per Adult\n from 1820 to 2023",
           hjust=0,
           size=7,
           lineheight=.9,
           #fontface="bold", 
           family="Merriweather",
           color="palegreen") +
  #Regions
  annotate("text", x = 2025, y = 225000,
           label = "North America €60 670",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold", family=font2,
           color=pal[1]) +
  annotate("text", x = 2025, y = 180000,
           label = "Europe €40 013",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[2]) +
  annotate("text", x = 2025, y = 140000,
           label = "Oceania €36 326",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[3]) +
  annotate("text", x = 2025, y = 110000,
           label = "Eastern Europe €26 671",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[4]) +
  annotate("text", x = 2025, y = 85000,
           label = "MENA €23 903",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[5]) +
  annotate("text", x = 2025, y = 60000,
           label = "East Asia €23 060",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[6]) +
  annotate("text", x = 2025, y = 40000,
           label = "Russia & Central Asia €20 852",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[7]) +
  annotate("text", x = 2025, y = 25000,
           label = "Latin America €16 235",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[8]) +
  annotate("text", x = 2025, y = 13000,
           label = "South & South-East Asia €10 322",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[9]) +
  annotate("text", x = 2025, y = 4000,
           label = "Sub-Saharan Africa €6 172",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal[10])+
  geom_segment(aes(x = 2000, y = 0, xend = 2000, yend = 169500+20000),color="lavender") +
  geom_segment(aes(x = 1950, y = 0, xend = 1950, yend = 54000+20000),color="lavender") +
  geom_segment(aes(x = 1900, y = 0, xend = 1900, yend = 22000+20000),color="lavender") +
  geom_segment(aes(x = 1850, y = 0, xend = 1850, yend = 3200+20000),color="lavender") +
  labs(caption = caption_text)+
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(20,150,1,5),
        plot.background = element_rect(fill = '#252A32'),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = '#252A32'),
        axis.text.x = element_text(color = "lavender",family = font2, size = 10),
        legend.position = "none",
        plot.caption = element_markdown(size = 10,family = font2,hjust = 0,color = "lavender"))

#'#df004f', '#e63f58', '#ec5e62', '#f1786c', '#f69076', '#f9a780', '#fcbd8a', '#fed395', '#ffe99f', '#ffffaa'


#'#00429d', '#1c58a7', '#2d6eb0', '#3d84b8', '#4e9abf', '#61b1c5', '#78c7c8', '#95ddc9', '#bbf2c3', '#ffffaa'

order3 <- c("Sub-Saharan Africa","South & South-East Asia","Latin America","Russia & Central Asia",
            "East Asia","MENA","Eastern Europe","Oceania","Europe","North America")
pal2=c('#ffffaa', '#ffe99f', '#fed395', '#fcbd8a', '#f9a780', '#f69076', '#f1786c', '#ec5e62', '#e63f58', '#df004f')
df2 %>% 
  arrange(Income) %>%
  mutate(Region = factor(Region, levels=order3)) %>% 
  ggplot(aes(Year, Income, fill = Region, label = Region, color = Region)) +
  geom_area() +
  
  scale_fill_manual(values=pal2) +
  scale_color_manual(values=pal2) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(clip = "off")+
  ylab("")+
  xlab("")+
  scale_x_continuous(breaks=c(1820,1850,1900,1950,2000,2023),labels = c("1820","1850","1900","1950","2000","2023"))+
  #Title
  annotate("text", x = 1850, y = 200000,
           label = "Average Income Per Adult\n from 1820 to 2023",
           hjust=0,
           size=7,
           lineheight=.9,
           #fontface="bold", 
           family="Merriweather",
           color="palegreen") +
  #Regions
  annotate("text", x = 2025, y = 260000,
           label = "Sub-Saharan Africa €6 172",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold", family=font2,
           color=pal2[1]) +
  annotate("text", x = 2025, y = 250000,
           label = "South & South-East Asia €10 322",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal2[2]) +
  annotate("text", x = 2025, y = 235000,
           label = "Latin America €16 235",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal2[3]) +
  annotate("text", x = 2025, y = 220000,
           label = "Russia & Central Asia €20 852",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal2[4]) +
  annotate("text", x = 2025, y = 200000,
           label = "East Asia €23 060",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal2[5]) +
  annotate("text", x = 2025, y = 175000,
           label = "MENA €23 903",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal2[6]) +
  annotate("text", x = 2025, y = 150000,
           label = "Eastern Europe €26 671",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal2[7]) +
  annotate("text", x = 2025, y = 120000,
           label = "Oceania €36 326",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal2[8]) +
  annotate("text", x = 2025, y = 80000,
           label = "Europe €40 013",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal2[9]) +
  annotate("text", x = 2025, y = 25000,
           label = "North America €60 670",
           hjust=0,
           size=4,
           lineheight=.8,
           fontface="bold",family=font2,
           color=pal2[10])+
  geom_segment(aes(x = 2000, y = 0, xend = 2000, yend = 169500+20000),color="lavender") +
  geom_segment(aes(x = 1950, y = 0, xend = 1950, yend = 54000+20000),color="lavender") +
  geom_segment(aes(x = 1900, y = 0, xend = 1900, yend = 22000+20000),color="lavender") +
  geom_segment(aes(x = 1850, y = 0, xend = 1850, yend = 3200+20000),color="lavender") +
  labs(caption = caption_text)+
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(20,150,1,5),
        plot.background = element_rect(fill = '#252A32'),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = '#252A32'),
        axis.text.x = element_text(color = "lavender",family = font2, size = 10),
        legend.position = "none",
        plot.caption = element_markdown(size = 10,family = font2,hjust = 0,color = "lavender"))
#line chart = doesn't work = "spaghetti"

ggplot(df2,aes(Year, Income, fill = Region, label = Region, color = Region)) +
  geom_line()

#small multiple line chart

ggplot(df2,aes(x = Year, y = Income, color = Region, group = Region)) +
  geom_line()+
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("#e2c0a9", 1))) +
  facet_wrap(~ Region)+
  labs(title = "Average Income Per Adult from 1820 to 2023",
       subtitle = "Euro € | ppp | constant (2023)") +
  # plot_layout(heights = c(1, 2)) +
  plot_annotation(
    caption = caption_text,
    theme=theme(plot.caption = element_markdown(hjust=1, margin=margin(20,0,0,0), size=10, color="black", lineheight = 1.2),
                plot.margin = margin(5,20,5,20),plot.background = element_rect(color="#fee8c8", fill="#fee8c8"),
                text = element_text(family = "Quattrocento Sans"))
  )+
  scale_x_continuous(breaks = c(1850,1900,1950,2000),
                     labels = c("1850","1900","1950","2000")
  ) +
  coord_cartesian(clip = "off")+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_text(color="black", size=9),
        legend.position="none",
        strip.text.x = element_text(face="bold",size = 12),
        plot.background = element_rect(color="#fee8c8", fill="#fee8c8"),
        plot.title = element_markdown(hjust=.5,size=16, color="black",lineheight=.8, face="bold", margin=margin(10,0,10,0)),
        plot.subtitle = element_markdown(hjust=.5,size=13, color="black",lineheight = 1, margin=margin(10,0,10,0)),
        plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color="black", lineheight = 1.2),
        plot.caption.position = "plot",
        plot.margin = margin(3,10,0,10),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="#f2b58a",size = 0.25,linetype = 2),
        text = element_text(family = "Quattrocento Sans"),
        axis.title.y = element_blank(),   
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank(),    
        axis.line.y = element_blank()
  )


#small multiple area chart

#light 
colors <- brewer.pal(n = 12, name = "Set3")[1:10]  # Select the first 10 colors

ggplot(df2, aes(x = Year, y = Income, fill = Region)) +
  geom_area(alpha = 0.6, position = "identity") +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(fill = alpha("#e2c0a9", 1))) +
  facet_wrap(~ Region) +
  labs(title = "Average Income Per Adult from 1820 to 2023",
       subtitle = "Euro € | ppp | constant (2023)") +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.caption = element_markdown(hjust = 1, margin = margin(20, 0, 0, 0), size = 10, color = "black", lineheight = 1.2),
      plot.margin = margin(5, 20, 5, 20),
      plot.background = element_rect(color = "#fee8c8", fill = "#fee8c8"),
      text = element_text(family = "Quattrocento Sans")
    )
  ) +
  scale_x_continuous(breaks = c(1850, 1900, 1950, 2000),
                     labels = c("1850", "1900", "1950", "2000")) +
  scale_fill_manual(values = colors) +  # Apply the color palette here
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color = "black", size = 9),
    legend.position = "none",
    strip.text.x = element_text(face = "bold", size = 10),
    plot.background = element_rect(color = "#fee8c8", fill = "#fee8c8"),
    plot.title = element_markdown(family = "Merriweather",hjust = .5, size = 16, color = "black", lineheight = .8, face = "bold", margin = margin(10, 0, 10, 0)),
    plot.subtitle = element_markdown(hjust = .5, size = 13, color = "black", lineheight = 1, margin = margin(10, 0, 10, 0)),
    plot.caption = element_markdown(hjust = .5, margin = margin(60, 0, 0, 0), size = 8, color = "black", lineheight = 1.2),
    plot.caption.position = "plot",
    plot.margin = margin(3, 10, 0, 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#f2b58a", size = 0.25, linetype = 2),
    text = element_text(family = "Quattrocento Sans"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )

#dark
colors <- c("#FF0000","#FF8700","#FFD300","#DEFF0A","#A1FF0A",
            "#0AFF99","#0AEFFF","#147DF5","#580AFF","#BE0AFF")

ggplot(df2, aes(x = Year, y = Income, fill = Region)) +
  geom_area(alpha = 0.6, position = "identity") +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(fill = alpha("steelblue", 1))) +
  facet_wrap(~ Region) +
  labs(title = "Average Income Per Adult from 1820 to 2023",
       subtitle = "Euro € | ppp | constant (2023)") +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.caption = element_markdown(hjust = 1, margin = margin(20, 0, 0, 0), size = 10, color = "lavender", lineheight = 1.2),
      plot.margin = margin(5, 20, 5, 20),
      plot.background = element_rect(fill = "#252A32", color = "#252A32"),  # Background color
      text = element_text(color = "lavender", family = "Quattrocento Sans")  # Text color
    )
  ) +
  scale_x_continuous(breaks = c(1850, 1900, 1950, 2000),
                     labels = c("1850", "1900", "1950", "2000")) +
  scale_fill_manual(values = colors) +  # Apply the color palette here
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#252A32", color = "#252A32"),  # Panel background
    plot.background = element_rect(fill = "#252A32", color = "#252A32"),
    axis.title = element_blank(),
    axis.text = element_text(color = "lavender", size = 9,family = "Quattrocento Sans"),
    legend.position = "none",
    strip.text.x = element_text(face = "bold", size = 10, color = "lavender"),
    plot.title = element_markdown(hjust = .5, size = 16, color = "lavender", lineheight = .8, face = "bold",family = "Merriweather", margin = margin(10, 0, 10, 0)),
    plot.subtitle = element_markdown(hjust = .5, size = 13, color = "lavender", lineheight = 1, margin = margin(10, 0, 10, 0)),
    plot.caption = element_markdown(hjust = .5, margin = margin(60, 0, 0, 0), size = 8, color = "lavender", lineheight = 1.2),
    plot.caption.position = "plot",
    plot.margin = margin(3, 10, 0, 10),
    text = element_text(family = "Quattrocento Sans"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "darkgrey", size = 0.25, linetype = 2),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )

