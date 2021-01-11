# library packages
library(ggthemes)
library(gridExtra)
library(kableExtra)
library(lubridate)
library(plm)
library(sf)
library(spdep)
library(tidyverse)
library(units)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sp)
library(car)
library(fs)
library(janitor)
library(broom)
library(corrr)
library(pixiedust)
library(kableExtra)

# Load data
death <- read_csv("/Users/jasontseyuk-fai/Desktop/GIS/西班牙/death3.csv")
load("/Users/jasontseyuk-fai/Desktop/GIS/provinces_spain.RData")

# Join provincial data to mortality data and convert to simple features:
death_spain <- death %>% 
  left_join(provinces_spain %>% st_drop_geometry(),
            by = c("province", "CCAA", "ID_INE"))


# descriptive statistics
data.frame(Variable = c("COVID-19 Mortality",
                        "COVID-19 Incidence",
                        "Area", 
                        "GDPpc", 
                        "Older", 
                        "Population Density"),
           Note = c("Mortality in reported cases of COVID-19 per 100,000 people",
                    "Incidence in reported cases of COVID-19 per 100,000 people",
                    "Area of province in sq.km",
                    "GDP per capita in €1,000s",
                    "Percentage of people aged 65 and older in the province",
                    "Population density in the province in people per sq.km"),
           Min = c(min(death_spain$mortality),
                   min(death_spain$Incidence),
                   min(provinces_spain %>% 
                         st_area() %>%
                         set_units(km^2)),
                   min(provinces_spain$GDPpc),
                   min(provinces_spain$Older),
                   min(provinces_spain$Density)),
           Mean = c(mean(death_spain$mortality),
                    mean(death_spain$Incidence),
                    mean(provinces_spain %>% 
                           st_area() %>%
                           set_units(km^2)),
                    mean(provinces_spain$GDPpc),
                    mean(provinces_spain$Older),
                    mean(provinces_spain$Density)),
           Max = c(max(death_spain$mortality),
                   max(death_spain$Incidence),
                   max(provinces_spain %>% 
                         st_area() %>%
                         set_units(km^2)),
                   max(provinces_spain$GDPpc),
                   max(provinces_spain$Older),
                   max(provinces_spain$Density)),
           SD = c(sd(death_spain$mortality),
                  sd(death_spain$Incidence),
                  sd(provinces_spain$GDPpc),
                  sd(provinces_spain %>% 
                       st_area() %>%
                       set_units(km^2)),
                  sd(provinces_spain$Older),
                  sd(provinces_spain$Density)),
           Source = c("ProvidencialData19",
                      "ProvidencialData19",
                      "INE",
                      "INE",
                      "INE",
                      "INE")) %>%
  kable(#"latex",
    "html",
    booktabs = TRUE,
    digits = 2,
    caption = "\\label{tab:descriptive-statistics} Descriptive statistics") %>%
  #kable_styling(latex_options = c("striped", "scale_down")) %>%
  kable_styling(bootstrap_options = c("striped", "condensed")) %>%
  column_spec(2, width = "15em") %>%
  column_spec(7, width = "5em") %>%
  footnote(general = c("ProvidencialData19: https://www.datoscovid.es/pages/providencialdata19", 
                       "INE (Instituto Nacional de Estadistica): https://www.ine.es/"))

# Retrieve the coordinates of Madrid
madrid_xy <-  st_centroid(provinces_spain %>% filter(province == "Madrid")) %>% st_coordinates()

# mapping mean weekly mortality of COVID‐19
week11.plot <- death_spain %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  group_by(province, week = isoweek(Date)) %>% 
  summarise(ID_INE = first(ID_INE), mean_weekly_mortality = mean(mortality)) %>%
  filter(week == 11) %>%
  left_join(provinces_spain,
            by = c("ID_INE")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_weekly_mortality), color = "gray69") +
  scale_fill_distiller(name = "Mean Weekly Mortality", 
                       palette = "BuPu", 
                       direction = 1,
                       limits = c(0, 90)) +
  ggtitle("March 13 - March 15") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])

week12.plot <- death_spain %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  group_by(province, week = isoweek(Date)) %>% 
  summarise(ID_INE = first(ID_INE), mean_weekly_mortality = mean(mortality)) %>%
  filter(week == 12) %>%
  left_join(provinces_spain,
            by = c("ID_INE")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_weekly_mortality), color = "gray69") +
  scale_fill_distiller(name = "Mean Weekly Mortality", 
                       palette = "BuPu",
                       direction = 1,
                       limits = c(0, 90)) +
  ggtitle("March 16 - March 22") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6) +
  geom_segment(x = madrid_xy[1] - 3.4,
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
week13.plot <- death_spain %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  group_by(province, week = isoweek(Date)) %>% 
  summarise(ID_INE = first(ID_INE), mean_weekly_mortality = mean(mortality)) %>%
  filter(week == 13) %>%
  left_join(provinces_spain,
            by = c("ID_INE")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_weekly_mortality), color = "gray69") +
  scale_fill_distiller(name = "Mean Weekly Mortality", 
                       palette = "BuPu", 
                       direction = 1,
                       limits = c(0, 90)) +
  ggtitle("March 23 - March 29") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf_text(label = "Madrid",
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
week14.plot <- death_spain %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  group_by(province, week = isoweek(Date)) %>% 
  summarise(ID_INE = first(ID_INE), mean_weekly_mortality = mean(mortality)) %>%
  filter(week == 14) %>%
  left_join(provinces_spain,
            by = c("ID_INE")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_weekly_mortality), color = "gray69") +
  scale_fill_distiller(name = "Mean Weekly Mortality", 
                       palette = "BuPu", 
                       direction = 1,
                       limits = c(0, 90)) +
  ggtitle("March 30 - April 5") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
week15.plot <- death_spain %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  group_by(province, week = isoweek(Date)) %>% 
  summarise(ID_INE = first(ID_INE), mean_weekly_mortality = mean(mortality)) %>%
  filter(week == 15) %>%
  left_join(provinces_spain,
            by = c("ID_INE")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_weekly_mortality), color = "gray69") +
  scale_fill_distiller(name = "Mean Weekly Mortality", 
                       palette = "BuPu", 
                       direction = 1,
                       limits = c(0, 90)) +
  ggtitle("April 6 - April 10") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
# Create grid layout
lay = rbind(c(1, 1, 2, 2),
            c(1, 1, 2, 2),
            c(3, 3, 4, 4),
            c(3, 3, 4, 4),
            c(NA, 5, 5, NA),
            c(NA, 5, 5, NA))
grid.arrange(week11.plot, week12.plot, week13.plot, week14.plot, week15.plot, layout_matrix = lay)

# mapping mean weekly incidence of COVID‐19
week11_in.plot <- death_spain %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  group_by(province, week = isoweek(Date)) %>% 
  summarise(ID_INE = first(ID_INE), mean_weekly_incidence = mean(Incidence)) %>%
  filter(week == 11) %>%
  left_join(provinces_spain,
            by = c("ID_INE")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_weekly_incidence), color = "gray78") +
  scale_fill_distiller(name = "Mean Weekly Incidence", 
                       palette = "Blues", 
                       direction = 1,
                       limits = c(0.755, 988.71)) +
  ggtitle("March 13 - March 15") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
week12_in.plot <- death_spain %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  group_by(province, week = isoweek(Date)) %>% 
  summarise(ID_INE = first(ID_INE), mean_weekly_incidence = mean(Incidence)) %>%
  filter(week == 12) %>%
  left_join(provinces_spain,
            by = c("ID_INE")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_weekly_incidence), color = "gray78") +
  scale_fill_distiller(name = "Mean Weekly Incidence", 
                       palette = "Blues",
                       direction = 1,
                       limits = c(0.755, 988.71)) +
  ggtitle("March 16 - March 22") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6) +
  geom_segment(x = madrid_xy[1] - 3.4,
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
week13_in.plot <- death_spain %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  group_by(province, week = isoweek(Date)) %>% 
  summarise(ID_INE = first(ID_INE), mean_weekly_incidence = mean(Incidence)) %>%
  filter(week == 13) %>%
  left_join(provinces_spain,
            by = c("ID_INE")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_weekly_incidence), color = "gray78") +
  scale_fill_distiller(name = "Mean Weekly Incidence", 
                       palette = "Blues", 
                       direction = 1,
                       limits = c(0.755, 988.71)) +
  ggtitle("March 23 - March 29") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf_text(label = "Madrid",
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
week14_in.plot <- death_spain %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  group_by(province, week = isoweek(Date)) %>% 
  summarise(ID_INE = first(ID_INE), mean_weekly_incidence = mean(Incidence)) %>%
  filter(week == 14) %>%
  left_join(provinces_spain,
            by = c("ID_INE")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_weekly_incidence), color = "gray78") +
  scale_fill_distiller(name = "Mean Weekly Incidence", 
                       palette = "Blues", 
                       direction = 1,
                       limits = c(0.755, 988.71)) +
  ggtitle("March 30 - April 5") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
week15_in.plot <- death_spain %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  group_by(province, week = isoweek(Date)) %>% 
  summarise(ID_INE = first(ID_INE), mean_weekly_incidence = mean(Incidence)) %>%
  filter(week == 15) %>%
  left_join(provinces_spain,
            by = c("ID_INE")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = mean_weekly_incidence), color = "gray78") +
  scale_fill_distiller(name = "Mean Weekly Incidence", 
                       palette = "Blues", 
                       direction = 1,
                       limits = c(0.755, 988.71)) +
  ggtitle("April 6 - April 11") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm")) +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])

# Create grid layout
lay_in = rbind(c(1, 1, 2, 2),
            c(1, 1, 2, 2),
            c(3, 3, 4, 4),
            c(3, 3, 4, 4),
            c(NA, 5, 5, NA),
            c(NA, 5, 5, NA))
grid.arrange(week11_in.plot, week12_in.plot, week13_in.plot, week14_in.plot, week15_in.plot, layout_matrix = lay_in)


# mapping spatial distribution of control variables

provinces_spain <- provinces_spain %>%
  mutate(GDPpc = GDPpc/1000)

# GDP per capita:
q <- quantile(provinces_spain$GDPpc)
gdppc.plot <- provinces_spain %>%
  mutate(GDPpc = GDPpc,
         GDPpc = case_when(GDPpc < q[2] ~ "1st",
                           GDPpc > q[2] & GDPpc <= q[3]  ~ "2nd",
                           GDPpc > q[3] & GDPpc <= q[4] ~ "3rd",
                           GDPpc > q[4] ~ "4th")) %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  ggplot() +
  geom_sf(aes(fill = GDPpc), color = "snow1") +
  scale_fill_manual(name = "Quartile", values=c("#CCCCFF","#99CCFF", "#6699FF", "#3333CC")) +
  ggtitle("GDP per capita") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom") +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
# Older adults:
q <- quantile(provinces_spain$Older)
older.plot <- provinces_spain %>% 
  mutate(Older = Older,
         Older = case_when(Older < q[2] ~ "1st",
                           Older > q[2] & Older <= q[3]  ~ "2nd",
                           Older > q[3] & Older <= q[4] ~ "3rd",
                           Older > q[4] ~ "4th")) %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  ggplot() +
  geom_sf(aes(fill = Older), color = "snow1") +
  scale_fill_manual(name = "Quartile", values=c("#CCCCFF","#99CCFF", "#6699FF", "#3333CC")) +
  ggtitle("Percentage of older adults") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom") +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
# Population density:
q <- as.numeric(quantile(provinces_spain$Density))
density.plot <- provinces_spain %>% 
  mutate(Density = as.numeric(Density),
         Density = case_when(Density < q[2] ~ "1st",
                             Density > q[2] & Density <= q[3]  ~ "2nd",
                             Density > q[3] & Density <= q[4] ~ "3rd",
                             Density > q[4] ~ "4th")) %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  ggplot() +
  geom_sf(aes(fill = Density), color = "snow1") +
  scale_fill_manual(name = "Quartile", values=c("#CCCCFF","#99CCFF", "#6699FF", "#3333CC")) +
  ggtitle("Population density") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom") +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
# Transit systems:
transit.plot <- provinces_spain %>% 
  mutate(Transit = factor(Transit, 
                          levels = c(0, 1),
                          labels = c("No",
                                     "Yes"))) %>%
  filter(CCAA != "Canarias", province != "Baleares") %>%
  ggplot() +
  geom_sf(aes(fill = Transit), color = "snow1") +
  scale_fill_manual(name = "Transit", values=c("#99CCFF", "#3333CC")) +
  ggtitle("Mass transit systems") +
  theme_bw(base_size = 9) +
  theme(axis.text = element_blank(),
        legend.position = "bottom") +
  geom_sf_text(label = "Madrid", 
               x = madrid_xy[1] - 4.7, 
               y = madrid_xy[2] + 0.6,
               family = "serif") +
  geom_segment(x = madrid_xy[1] - 3.4, 
               y = madrid_xy[2] + 0.6, 
               xend = madrid_xy[1], 
               yend = madrid_xy[2])
# Define layout for plot
lay <- rbind(c(1, 2),
             c(3, 4))
grid.arrange(gdppc.plot, older.plot, density.plot, transit.plot, layout_matrix = lay)


# Correlation matrix
Correlation <- death_spain %>%
  dplyr::select(mortality,Incidence,Density,Older,gdppc) %>%
  correlate() %>%
  focus(-mortality, mirror = TRUE) 

#visualise the correlation matrix
rplot(Correlation)


# Change variable attribute

death_model <- provinces_spain%>%
  left_join(.,
            death, 
            by = c("province", "CCAA", "ID_INE"))

isitfactor <- death_model %>%
  dplyr::select(Transit)%>%
  summarise_all(class)

death_model<- death_model %>%
  mutate(Transit=as.factor(Transit))


# linear regression
Regressiondata1<- death_model%>%
  clean_names()%>%
  dplyr::select(mortality,incidence,density,older,gdppc,transit)

model1 <- lm(mortality ~ incidence + 
               density + older + gdppc + transit, data = Regressiondata1)

#show the summary of those outputs
dust(model1) %>%
  sprinkle(col = 2:4, round = 3) %>%
  sprinkle(col = 5, fn = quote(pvalString(value))) %>%
  sprinkle_colnames(term = "Term",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "T-statistic",
                    p.value = "P-value") %>%
  kable() %>%
  kable_styling()


# Homoscedasticity
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model1)


#calculate the centroids 
coordsW <- death_model%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)


#nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)


LWard_knn <- knn_wards %>%
  knn2nb()

#plot them
plot(LWard_knn, st_geometry(coordsW), col="blue")
plot(death_model)



Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")

# run a moran’s I test on the residuals
Nearest_neighbour <- death_model %>%
  st_drop_geometry()%>%
  dplyr::select(model1resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

# The Spatial Lag (lagged dependent variable) model
slag_dv_model2_knn4 <- lagsarlm(mortality ~ Incidence + Density + Older + gdppc + Transit, data = death_model, nb2listw(LWard_knn, style="C"), 
                                method = "eigen")


#show the summary of those outputs
dust(slag_dv_model2_knn4) %>%
  sprinkle(col = 2:4, round = 3) %>%
  sprinkle(col = 5, fn = quote(pvalString(value))) %>%
  sprinkle_colnames(term = "Term",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "T-statistic",
                    p.value = "P-value") %>%
  kable() %>%
  kable_styling()





