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

##################
# Procesing data #
##################

#df <- read.csv("dataset.csv")

## Primer vistazo ##
#head(df)
#dim(df)
#colnames(df)


## Estructura ##
#str(df)


## Resumen estadístico ##
#summary(df)


## Valores perdidos ##
#colSums(is.na(df))
#sum(is.na(df))
#dim(df)
#dim(na.omit(df))


## Duplicados ##
#sum(duplicated(df))


## Tablas para categóricas ##
#table(df$variable_categorica)


## Resumen de numéricas ##
#summary(df$variable_numerica)

###############################
# Choosing graphs and questions
###############################

## 1. Distribución de variables numéricas ##
# Pregunta: ¿Cómo se distribuye esta variable?
# Gráficos útiles: histograma, boxplot, density plot

# ggplot(df, aes(x = variable_numerica)) +
#   geom_histogram()

# ggplot(df, aes(y = variable_numerica)) +
#   geom_boxplot()


## 2. Frecuencia de variables categóricas ##
# Pregunta: ¿Qué categorías aparecen más o menos?
# Gráfico útil: barplot

# ggplot(df, aes(x = variable_categorica)) +
#   geom_bar()


## 3. Relación entre dos variables numéricas ##
# Pregunta: ¿Existe relación entre ambas variables?
# Gráfico útil: scatter plot

# ggplot(df, aes(x = variable_numerica_1, y = variable_numerica_2)) +
#   geom_point()


## 4. Comparación de una numérica entre categorías ##
# Pregunta: ¿Una variable numérica cambia según el grupo?
# Gráficos útiles: boxplot, violin plot, barplot con medias

# ggplot(df, aes(x = variable_categorica, y = variable_numerica)) +
#   geom_boxplot()


## 5. Relación entre dos variables categóricas ##
# Pregunta: ¿Hay asociación entre dos variables categóricas?
# Gráficos útiles: barras agrupadas, barras apiladas

# ggplot(df, aes(x = variable_categorica_1, fill = variable_categorica_2)) +
#   geom_bar(position = "dodge")


## 6. Evolución temporal ##
# Pregunta: ¿Cómo cambia una variable a lo largo del tiempo?
# Gráfico útil: line plot

# ggplot(df, aes(x = variable_fecha, y = variable_numerica)) +
#   geom_line()

##################
# Interpretation #
##################

#1. Qué gráfico he hecho.
#2. Qué variable(s) analiza.
#3. Qué patrón se observa.
#4. Qué interpretación tiene.
#5. Si hay outliers, sesgos, grupos destacados o limitaciones.



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

colors_border <- c("#DF536B", "#61D04F", "#2297E6", "#28E1E5", "#E6C122", "#B385FF", "#F59B33")
colors_in <- scales::alpha(colors_border, 0.2)

radarchart(
  radar_plot_data,
  axistype = 1,
  pcol = colors_border,
  pfcol = colors_in,
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey40",
  title = "Perfil medio de las clases de coche"
)

legend(
  x = "topright",                                  # Posición de la caja
  legend = rownames(radar_plot_data)[-c(1,2)],     # Quita 'max' y 'min' de los nombres
  bty = "n",                                       # Sin borde en la caja de la leyenda
  pch = 20,                                        # Icono de punto sólido
  col = colors_border,                             # Colores asignados
  text.col = "grey20", 
  pt.cex = 1.5, 
  cex = 0.8
)

## Si usas la media hay que mostrar la distribucion
cars_escalado <- cars %>%
  mutate(
    Ciudad    = rescale(cty),   # Usa 'cty' de tu tibble
    Carretera = rescale(hwy),   # Usa 'hwy' de tu tibble
    Cilindros = rescale(cyl),   # Usa 'cyl' de tu tibble
    Motor     = rescale(displ)  # Usa 'displ' de tu tibble
  ) %>%
  select(class, Ciudad, Carretera, Cilindros, Motor)

# 2. Pasamos a formato largo para poder facetar
df_densidades_long <- cars_escalado %>%
  pivot_longer(cols = c(Ciudad, Carretera, Cilindros, Motor), 
               names_to = "Pregunta", 
               values_to = "Rating")

# 3. Lista de nombres para las etiquetas flotantes
preguntas_radar <- c("Ciudad", "Carretera", "Cilindros", "Motor")

# 4. Tu gráfico clonado
ggplot(df_densidades_long, aes(x = Rating, fill = class, color = class)) +
  geom_density(alpha = 0.4) +
  
  # Aplica los mismos colores que definiste para el spider plot
  scale_fill_manual(values = colors_border) +
  scale_color_manual(values = colors_border) +
  
  # Facetado en una sola columna vertical
  facet_wrap(~ Pregunta, ncol = 1, scales = "free_y") +
  
  # El truco del geom_text para poner los títulos arriba a la izquierda (x = 0)
  geom_text(
    data = data.frame(Pregunta = preguntas_radar),
    aes(x = 0, y = Inf, label = Pregunta),
    inherit.aes = FALSE, vjust = 1.5, hjust = 0,
    fontface = "bold", alpha = 0.7
  ) +
  
  labs(
    title = "Score Distributions by Car Class",
    subtitle = "Comparing density of answers for Radar Chart variables",
    x = "Rating (0-1 Scaled)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank(),       
    panel.spacing = unit(1, "lines")    
  )


## Bullet chart

###############
# PROPORTIONS #
###############

## Stacked bars - cantidad de coches por clase y tracción ##
ggplot(cars, aes(x = class, fill = drv)) +
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Pastel2") +
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


###############
# HIGH DIMENSIONALITY #
###############

## Alluvial plot - flujo de movimientos o tambien para hierarchical data

library(easyalluvial)
library(parcats)
library(dplyr)

# 2. Preparamos los datos de 'cars' seleccionando variables categóricas/discretas
df_alluvial_cars <- cars %>%
  select(
    `Fabricante` = manufacturer,
    `Cilindros`  = cyl,
    `Traccion`   = drv,
    `Clase`      = class
  ) %>%
  # Convertimos todo a caracteres/factores (obligatorio para flujos categóricos)
  mutate(across(everything(), as.character))

# 3. Creamos la estructura del alluvial (Formato ancho / Wide)
p_alluvial_cars <- alluvial_wide(
  data = df_alluvial_cars, 
  fill_by = 'first_variable' # El color del flujo dependerá del Fabricante
)

# 4. Lo convertimos en gráfico interactivo
parcats(
  p_alluvial_cars, 
  data_input = df_alluvial_cars,
  marginal_histograms = TRUE, 
  hoverinfo = "count+probability",
  width = 1100, 
  height = 700
)

## Parcoords - se puede con el mismo dataframe, el formato es el mismo
install.packages("remotes")
remotes::install_github("timelyportfolio/parcoords")
install.packages("d3r")
library(parcoords)
library(dplyr)

# 2. Definimos tus 7 colores (uno para cada una de las 7 clases de coche)
all_colors <- c("#DF536B", "#61D04F", "#2297E6", "#28E1E5", "#E6C122", "#B385FF", "#F59B33")

# 3. Lanzamos el gráfico calcado a tus apuntes
parcoords(
  cars %>% select("cty", "hwy", "cyl", "displ", "year", "class"),
  reorderable = TRUE,
  brushMode = "1d-axes",
  rownames = FALSE,
  color = list(
    colorBy = "class",           # Agrupa y colorea por tipo de coche (SUV, Compact...)
    colorScale = "scaleOrdinal", # Al ser texto (categorías), usamos escala ordinal
    colorScheme = all_colors
  ),
  alpha = 0.4,                   # Te sugiero subir el alpha a 0.4; 0.02 es demasiado invisible para 234 filas
  withD3 = TRUE
)
