## ----------------------------------------------------------------
# install.packages(c("tidyverse",
#                    "ggplot2"))

## ---------------------------------------------------------

## Cargar paquetes
library(tidyverse) 
library(ggplot2)

## ---------------------------------------------------------

trees <- read_csv("files/trees.csv")

# graficar height ~ dbh
ggplot(trees, aes(x = dbh, y = height)) + 
  geom_point()

# anadir color en base al sexo
ggplot(trees, aes(x = dbh, y = height,
                  color = sex)) + 
  geom_point()

# cambiar tamaño y transparecia puntos
ggplot(trees, aes(x = dbh, y = height,
                  color = sex)) + 
  geom_point(size = 4, alpha = 0.5)

# cambiar color puntos de forma manual
ggplot(trees, aes(x = dbh, y = height,
                  color = sex)) + 
  geom_point(size = 4, alpha = 0.5) + 
  scale_color_manual(values = c("yellow", "purple"))

# usar paleta de color (viridis)
ggplot(trees, aes(x = dbh, y = height,
                  color = sex)) + 
  geom_point(size = 4, alpha = 0.5) + 
  scale_color_viridis_d()

# cambiar color en base a variable continua (height)
ggplot(trees, aes(x = dbh, y = height,
                  color = height)) + 
  geom_point(size = 4, alpha = 0.5) + 
  scale_color_viridis_c()

# usar paleta de color (magma)
ggplot(trees, aes(x = dbh, y = height,
                  color = height)) + 
  geom_point(size = 4, alpha = 0.5) + 
  scale_color_viridis_c(option = "magma")

# añadir linea de tendencia
ggplot(trees, aes(x = dbh, y = height)) + 
  geom_point() + 
  geom_smooth(method = "lm")

## ---------------------------------------------------------

# añadir linea de tendencia por sitio
ggplot(trees, aes(x = dbh, y = height,
                  color = as.factor(site))) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm")

## ---------------------------------------------------------

# separar graficas por sitio
ggplot(trees, aes(x = dbh, y = height,
                  color = as.factor(site))) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") +
  facet_wrap(~site)

# separar graficas por sitio y sexo
ggplot(trees, aes(x = dbh, y = height,
                  color = as.factor(site))) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") +
  facet_grid(sex~site)

## ---------------------------------------------------------

# histograma
ggplot(trees, aes(x = height)) +
  geom_histogram()

# densidad
ggplot(trees, aes(x = height)) +
  geom_density()

# densidad por sexo
ggplot(trees, aes(x = height,
                  fill = sex)) +
  geom_density(alpha = 0.5)

## ---------------------------------------------------------

# boxplot
ggplot(trees, aes(y = height, x = as.factor(site))) +
  geom_boxplot()

# boxplot
ggplot(trees, aes(y = height, x = as.factor(site))) +
  geom_boxplot() + 
  geom_jitter()

## ---------------------------------------------------------

# violin-plot
ggplot(trees, aes(y = height, x = as.factor(site))) +
  geom_violin()

## ---------------------------------------------------------

#temas
ggplot(trees, aes(y = height, x = as.factor(site))) +
  geom_violin() + 
  theme_bw()

#temas
ggplot(trees, aes(y = height, x = as.factor(site))) +
  geom_violin() + 
  theme_dark()
  
## ---------------------------------------------------------

#nombre ejes y titulo
ggplot(trees, aes(y = height, x = as.factor(site))) +
  geom_violin() + 
  labs(x = "Sitio", y = "Altura",
       title = "Altura arboles en distintos sitios")

## ---------------------------------------------------------

#Detalles de los ejes
ggplot(trees, aes(y = height, x = as.factor(site))) +
  geom_violin() + 
  labs(x = "Sitio", y = "Altura",
       title = "Altura arboles en distintos sitios") + 
  theme(axis.text.y = element_text(size = 15, angle = 45))


## ---------------------------------------------------------

# box plot por sitio y sexo
ggplot(trees, aes(y = height, x = as.factor(site), fill = sex)) +
  geom_boxplot() + 
  labs(x = "Sitio", y = "Altura",
       title = "Altura arboles en distintos sitios") 

# cambiar posicion leyenda
ggplot(trees, aes(y = height, x = as.factor(site), fill = sex)) +
  geom_boxplot() + 
  labs(x = "Sitio", y = "Altura",
       title = "Altura arboles en distintos sitios") +
  theme(legend.position = "bottom")
  
## ---------------------------------------------------------

#cambiar escala eje-y
ggplot(trees, aes(y = height, x = as.factor(site), fill = sex)) +
  geom_boxplot() + 
  labs(x = "Sitio", y = "LOG de Altura",
       title = "Altura arboles en distintos sitios") +
  theme(legend.position = "bottom") + 
  scale_y_log10()


## ---------------------------------------------------------

# juntar varias graficas

# install.packages("patchwork")
library(patchwork)

p1 <- ggplot(trees, aes(x = height, fill = as.factor(dead))) + 
  geom_density(alpha = 0.5)

p2 <- ggplot(trees, aes(x = dbh, fill = as.factor(dead))) + 
  geom_density(alpha = 0.5)

p1 / p2

p1 + p2

p1 + p2 + plot_layout(guides = "collect")

## ---------------------------------------------------------

## ---------------------------------------------------------

## ---------------------------------------------------------

## ---------------------------------------------------------

## ---------------------------------------------------------
