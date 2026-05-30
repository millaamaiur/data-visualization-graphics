#Data Visualization Graphics Repository

#Created by Julen Corera, Airam Fernandez, Amaiur Milla

library(tidyverse)

data <- read.csv("vgsales.csv")


####################
# COUNTING AMOUNTS #
####################

## Bar ##
ggplot(data, aes(x = variable_cat)) +
  geom_bar() +
  labs(
    title = "Número de observaciones por categoría",
    x = "Categoría",
    y = "Cantidad"
  ) +
  theme_minimal()

## Dots ##
data %>%
  count(variable_cat) %>%
  ggplot(aes(x = n, y = reorder(variable_cat, n))) +
  geom_point(size = 4) +
  labs(
    title = "Cantidad por categoría",
    x = "Cantidad",
    y = "Categoría"
  ) +
  theme_minimal()

## Grouped bars ##
ggplot(data, aes(x = variable_cat, fill = grupo)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Barras agrupadas por grupo",
    x = "Categoría",
    y = "Cantidad",
    fill = "Grupo"
  ) +
  theme_minimal()

## Stacked bars ##
ggplot(data, aes(x = variable_cat, fill = grupo)) +
  geom_bar(position = "stack") +
  labs(
    title = "Barras apiladas",
    x = "Categoría",
    y = "Cantidad",
    fill = "Grupo"
  ) +
  theme_minimal()

################
# DISTRIBUTION #
################

## Historgram ##
ggplot(data, aes(x = variable_num)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribución de la variable numérica",
    x = "Variable numérica",
    y = "Frecuencia"
  ) +
  theme_minimal()

## Density plot ##
ggplot(data, aes(x = variable_num)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(
    title = "Densidad de la variable numérica",
    x = "Variable numérica",
    y = "Densidad"
  ) +
  theme_minimal()

## Boxplot ##
ggplot(data, aes(x = variable_cat, y = variable_num)) +
  geom_boxplot() +
  labs(
    title = "Distribución por categoría",
    x = "Categoría",
    y = "Variable numérica"
  ) +
  theme_minimal()

## Violin plot ##
ggplot(data, aes(x = variable_cat, y = variable_num)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.shape = NA) +
  labs(
    title = "Distribución por categoría",
    x = "Categoría",
    y = "Variable numérica"
  ) +
  theme_minimal()
## Ridgeline plot ##

###############
# TIME SERIES #
###############


##############
# MAGNITUDE #
##############



###############
# PROPORTIONS #
###############


###########
# SPATIAL #
###########


########
# FLOW #
########



###########
# Ranking #
###########



###############
# Correlation #
###############


#############
# Deviation #
#############
