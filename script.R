# Data Visualization Graphics Repository
# Created by Julen Corera, Airam Fernandez, Amaiur Milla
#
# Datasets:
#   mpg       -> incluido en ggplot2 (coches, consumo, cilindros)
#               class, manufacturer, drv, year, cyl, displ, hwy, cty
#   gapminder -> install.packages("gapminder")
#               country, continent, year, lifeExp, pop, gdpPercap

library(tidyverse)
library(ggridges)    # install.packages("ggridges")
library(gapminder)   # install.packages("gapminder")
library(treemapify)  # install.packages("treemapify")

# Datasets
cars <- mpg
gap  <- gapminder

####################
# COUNTING AMOUNTS #
####################

## Bar - número de coches por clase ##
ggplot(cars, aes(x = reorder(class, class, function(x) -length(x)))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Número de modelos por clase de coche",
    x = "Clase",
    y = "Cantidad"
  ) +
  theme_minimal()

## Dots - cantidad de modelos por fabricante ##
cars %>%
  count(manufacturer) %>%
  ggplot(aes(x = n, y = reorder(manufacturer, n))) +
  geom_point(size = 4, color = "steelblue") +
  labs(
    title = "Número de modelos por fabricante",
    x = "Cantidad",
    y = "Fabricante"
  ) +
  theme_minimal()

## Grouped bars - consumo medio en ciudad vs carretera por clase ##
cars %>%
  group_by(class) %>%
  summarise(Ciudad = mean(cty), Carretera = mean(hwy)) %>%
  pivot_longer(cols = c(Ciudad, Carretera), names_to = "Tipo", values_to = "Consumo") %>%
  ggplot(aes(x = class, y = Consumo, fill = Tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Consumo medio ciudad vs carretera por clase",
    x = "Clase",
    y = "Millas por galón (mpg)",
    fill = "Tipo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Stacked bars - distribución de tracción por clase ##
ggplot(cars, aes(x = class, fill = drv)) +
  geom_bar(position = "stack") +
  labs(
    title = "Distribución de tracción por clase de coche",
    x = "Clase",
    y = "Cantidad",
    fill = "Tracción"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


################
# DISTRIBUTION #
################

## Histogram - distribución del consumo en carretera ##
ggplot(cars, aes(x = hwy)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(
    title = "Distribución del consumo en carretera",
    x = "Millas por galón (hwy)",
    y = "Frecuencia"
  ) +
  theme_minimal()

## Density plot - densidad del consumo en ciudad ##
ggplot(cars, aes(x = cty)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(
    title = "Densidad del consumo en ciudad",
    x = "Millas por galón (cty)",
    y = "Densidad"
  ) +
  theme_minimal()

## Boxplot - consumo en carretera por clase ##
ggplot(cars, aes(x = reorder(class, hwy, median), y = hwy)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Consumo en carretera por clase de coche",
    x = "Clase",
    y = "Millas por galón (hwy)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Violin plot - consumo en carretera por clase ##
ggplot(cars, aes(x = reorder(class, hwy, median), y = hwy)) +
  geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.6) +
  geom_boxplot(width = 0.12, outlier.shape = NA) +
  labs(
    title = "Distribución del consumo en carretera por clase (violin)",
    x = "Clase",
    y = "Millas por galón (hwy)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Ridgeline plot - distribución consumo carretera por clase ##
ggplot(cars, aes(x = hwy, y = reorder(class, hwy, median), fill = class)) +
  geom_density_ridges(alpha = 0.6, show.legend = FALSE) +
  labs(
    title = "Distribución del consumo en carretera por clase",
    x = "Millas por galón (hwy)",
    y = "Clase"
  ) +
  theme_minimal()

###############
# TIME SERIES #
###############

## Line - evolución de la esperanza de vida media mundial ##
gap %>%
  group_by(year) %>%
  summarise(mean_life = mean(lifeExp)) %>%
  ggplot(aes(x = year, y = mean_life)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolución de la esperanza de vida media mundial",
    x = "Año",
    y = "Esperanza de vida (años)"
  ) +
  theme_minimal()

## Line múltiple - esperanza de vida por continente ##
gap %>%
  group_by(year, continent) %>%
  summarise(mean_life = mean(lifeExp), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_life, color = continent)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Evolución de la esperanza de vida por continente",
    x = "Año",
    y = "Esperanza de vida media (años)",
    color = "Continente"
  ) +
  theme_minimal()

## Area chart - PIB per cápita medio por continente ##
gap %>%
  group_by(year, continent) %>%
  summarise(mean_gdp = mean(gdpPercap), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_gdp, fill = continent)) +
  geom_area(alpha = 0.6) +
  labs(
    title = "PIB per cápita medio por continente a lo largo del tiempo",
    x = "Año",
    y = "PIB per cápita medio (USD)",
    fill = "Continente"
  ) +
  theme_minimal()

## Column - población mundial total por año ##
gap %>%
  group_by(year) %>%
  summarise(total_pop = sum(pop), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total_pop)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Evolución de la población mundial",
    x = "Año",
    y = "Población total"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Line + column - población y esperanza de vida media mundial ##
gap %>%
  group_by(year) %>%
  summarise(
    total_pop = sum(pop),
    mean_life = mean(lifeExp),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year)) +
  geom_col(aes(y = total_pop / 100000000), fill = "lightblue", alpha = 0.7) +
  geom_line(aes(y = mean_life), color = "steelblue", linewidth = 1.2) +
  geom_point(aes(y = mean_life), color = "steelblue", size = 2) +
  labs(
    title = "Población mundial y esperanza de vida media",
    subtitle = "Las columnas muestran población escalada y la línea esperanza de vida",
    x = "Año",
    y = "Esperanza de vida / Población escalada"
  ) +
  theme_minimal()

#############
# MAGNITUDE #
#############

## Lollipop - consumo medio en carretera por clase ##
cars %>%
  group_by(class) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  ggplot(aes(x = mean_hwy, y = reorder(class, mean_hwy))) +
  geom_segment(aes(x = 0, xend = mean_hwy, yend = class), color = "grey60") +
  geom_point(size = 4, color = "steelblue") +
  labs(
    title = "Consumo medio en carretera por clase",
    x = "Millas por galón (hwy)",
    y = "Clase"
  ) +
  theme_minimal()

## Diverging bar - diferencia de consumo respecto a la media global ##
cars %>%
  group_by(class) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  mutate(diff = mean_hwy - mean(mean_hwy)) %>%
  ggplot(aes(x = diff, y = reorder(class, diff), fill = diff > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    labels = c("TRUE" = "Sobre la media", "FALSE" = "Bajo la media")) +
  labs(
    title = "Desviación del consumo en carretera respecto a la media",
    x = "Diferencia respecto a la media (mpg)",
    y = "Clase",
    fill = ""
  ) +
  theme_minimal()

## Paired column

## Paired bar

library(fmsb)       # install.packages("fmsb")
library(ggforce)    # install.packages("ggforce")

## Radar chart - perfil medio de coches por clase ##
radar_data <- cars %>%
  group_by(class) %>%
  summarise(
    Ciudad = mean(cty),
    Carretera = mean(hwy),
    Cilindros = mean(cyl),
    Motor = mean(displ),
    .groups = "drop"
  )

radar_scaled <- radar_data %>%
  column_to_rownames("class") %>%
  mutate(across(everything(), scales::rescale))

radar_plot_data <- rbind(
  max = rep(1, ncol(radar_scaled)),
  min = rep(0, ncol(radar_scaled)),
  radar_scaled
)

radarchart(
  radar_plot_data,
  axistype = 1,
  pcol = "steelblue",
  pfcol = scales::alpha("lightblue", 0.4),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey40",
  title = "Perfil medio de las clases de coche"
)

## Bullet chart

###############
# PROPORTIONS #
###############

## Stacked bars - cantidad de coches por clase y tracción ##
ggplot(cars, aes(x = class, fill = drv)) +
  geom_bar(position = "stack") +
  labs(
    title = "Cantidad de coches por clase y tipo de tracción",
    x = "Clase",
    y = "Cantidad",
    fill = "Tracción"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Pie chart - proporción de coches por tracción ##
cars %>%
  count(drv) %>%
  ggplot(aes(x = "", y = n, fill = drv)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Proporción de coches por tipo de tracción", fill = "Tracción") +
  theme_void()

## Treemap - número de modelos por fabricante ##
cars %>%
  count(manufacturer) %>%
  ggplot(aes(area = n, fill = manufacturer, label = manufacturer)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
  labs(title = "Modelos por fabricante (Treemap)") +
  theme(legend.position = "none")

## Stacked bars 100% - proporción de tracción por clase ##
ggplot(cars, aes(x = class, fill = drv)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proporción de tracción por clase de coche",
    x = "Clase",
    y = "Proporción",
    fill = "Tracción"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Donut - proporción de coches por tracción ##
cars %>%
  count(drv) %>%
  mutate(drv = recode(drv,
                      "4" = "4 ruedas",
                      "f" = "Delantera",
                      "r" = "Trasera")) %>%
  ggplot(aes(x = 2, y = n, fill = drv)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  labs(
    title = "Proporción de coches por tipo de tracción",
    fill = "Tracción"
  ) +
  theme_void()

## Venn - coches eficientes en ciudad, carretera y motor pequeño ##
# install.packages("ggVennDiagram")
library(ggVennDiagram)
venn_data <- list(
  "Ciudad eficiente" = cars %>%
    filter(cty > mean(cty)) %>%
    pull(model),
  
  "Carretera eficiente" = cars %>%
    filter(hwy > mean(hwy)) %>%
    pull(model),
  
  "Motor pequeño" = cars %>%
    filter(displ < mean(displ)) %>%
    pull(model)
)

ggVennDiagram(venn_data, label_alpha = 0) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Relación entre eficiencia en ciudad, carretera y motor pequeño"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  )

###########
# SPATIAL #
###########
 

########
# FLOW #
########

###########
# RANKING #
###########

## Ordered bar - consumo medio en carretera por fabricante ##
cars %>%
  group_by(manufacturer) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  ggplot(aes(x = mean_hwy, y = reorder(manufacturer, mean_hwy))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Consumo medio en carretera por fabricante (ordenado)",
    x = "Millas por galón (hwy)",
    y = "Fabricante"
  ) +
  theme_minimal()

## Slope chart - esperanza de vida 1952 vs 2007 por continente ##
gap %>%
  filter(year %in% c(1952, 2007)) %>%
  group_by(continent, year) %>%
  summarise(mean_life = mean(lifeExp), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = mean_life, group = continent, color = continent)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Esperanza de vida por continente: 1952 vs 2007",
    x = "Año",
    y = "Esperanza de vida media (años)",
    color = "Continente"
  ) +
  theme_minimal()

###############
# CORRELATION #
###############

## Scatterplot - PIB per cápita vs esperanza de vida ##
gap %>% filter(year == 2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  labs(
    title = "PIB per cápita vs Esperanza de vida (2007)",
    x = "PIB per cápita (escala log)",
    y = "Esperanza de vida (años)"
  ) +
  theme_minimal()

## Bubble chart - PIB vs esperanza de vida con tamaño = población ##
gap %>% filter(year == 2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  scale_size(range = c(2, 15)) +
  labs(
    title = "PIB vs Esperanza de vida (tamaño = población, 2007)",
    x = "PIB per cápita (escala log)",
    y = "Esperanza de vida (años)",
    size = "Población",
    color = "Continente"
  ) +
  theme_minimal()

## Heatmap - consumo medio en carretera por fabricante y tracción ##
cars %>%
  group_by(manufacturer, drv) %>%
  summarise(mean_hwy = mean(hwy), .groups = "drop") %>%
  ggplot(aes(x = drv, y = manufacturer, fill = mean_hwy)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Consumo medio en carretera por fabricante y tracción",
    x = "Tracción",
    y = "Fabricante",
    fill = "hwy (mpg)"
  ) +
  theme_minimal()