## ----------------------------------------------------------------
# install.packages(c("tidyverse",
#                    "tidylog"))

## ---------------------------------------------------------

## Cargar paquetes
library(tidyverse) #paquete para manejo de datos
library(tidylog) #informacion de las operaciones de las funciones tidyverse

## ---------------------------------------------------------

## Leer y guardar datos

# read_csv() - Leer datos
# write_csv() - Guardar datos

## ---------------------------------------------------------

## Principales funciones de dply ##

# arrange() - Ordenar variable por casos 
# rename() - Renombrar variables 
# relocate() Reordenar variables
# select() - Extraer variables
# filter() - Filtrar datos por casos 
# distinct() - Extraer valores únicos
# mutate() - Crear nuevas variables 
### if_else() - Operar en base a condiciones (si condicion se cumple, A, sino B)
# group_by() - Agrupar datos por casos
# summarise() - Resumir datos por casos


## ---------------------------------------------------------

## Leer datos
dt_raw <- read_csv("files/seeds.csv")

## ---------------------------------------------------------

## Previsualizar datos
glimpse(dt_raw)

## ---------------------------------------------------------

## Seleccionar datos de interes
dt <- dt_raw |> 
  select(site_name,
         year,
         count,
         stem_diameter_cm,
         trap_area_m2,
         general_method)

## ---------------------------------------------------------

## Filtrar datos - funcion filter()
dt |> 
  # filtrar varios sitios
  filter(site_name %in% c("AEC", "AND", "BNZ")) 

dt |>
  #quitar observaciones con 0 numero de frutos o sin datos
  filter(count != 0)

dt |>
  #quitar observaciones sin datos para numero de frutos y diametro
  filter(!is.na(count))




## ---------------------------------------------------------
# CREAR NUEVAS VARIABLES
## ---------------------------------------------------------

## Crear nueva variable - funcion mutate()
dt |> 
  # convertir unidades (de cm a m)
  mutate(stem_m = stem_diameter_cm/100) |> 
  arrange(desc(stem_m))

## Crear nueva variable - funcion mutate()
dt |> 
  # numero de frutos por m2
  mutate(fruits_m2 = count/trap_area_m2) |> 
  ## Ordenar datos
  arrange(desc(fruits_m2))

## ---------------------------------------------------------

## Arreglar casos o categorizar datos - funcion if_else() 
dt |>
  mutate(fruits_m2 = count/trap_area_m2) |> 
  arrange(desc(fruits_m2)) |> 
  # quitar un valor equivocado 
  mutate(fruits_m2 = if_else(fruits_m2 >= 300000, NA, fruits_m2)) 

## ---------------------------------------------------------

## Extraer valores unicos para vriables categoricas
dt |> distinct(general_method)

dt |> distinct(site_name, general_method)

## Funcion if_else para categorizar 
dt |> 
  # calcular número de frutos por m2 
  mutate(fruits_m2 = count/trap_area_m2) |> 
  # crear variable con la cantidad de frutos de count o corregida 
  mutate(fruits_fix = if_else(general_method == "TRAP", fruits_m2, count))





## ---------------------------------------------------------
# GUARDAR BASES DE DATOS
## ---------------------------------------------------------

dt_clean <- dt |> 
  # calcular número de frutos por m2 
  mutate(fruits_m2 = count/trap_area_m2) |> 
  # crear variable con la cantidad de frutos de count o corregida 
  mutate(fruits_fix = if_else(general_method == "TRAP", fruits_m2, count))

write.csv(dt_clean, "files/data_clean.csv")






## ---------------------------------------------------------
# RESUMIR BASES DE DATOS
## ---------------------------------------------------------

## Agrupar datos y resumir - funciones group_by() + summarise()
dt |>
  #1. agrupar por sitio
  group_by(site_name) |> 
  #2. sumar numero de frutos por sitio
  summarise(fruits = sum(count))

dt |>
  #1. agrupar por sitio
  group_by(site_name) |> 
  #2. sumar numero de frutos por sitio
  #añadir na.rm = TRUE para obviar NAs
  summarise(fruits = sum(count, na.rm = TRUE))

#usando datos limpios sin NAs
dt |>
  filter(count != 0) |> 
  #1. agrupar por sitio
  group_by(site_name) |>
  #2. numero maximo y medio de frutos por sitio
  summarise(mean_fruit = mean(count),
            max_fruit = max(count))

## ---------------------------------------------------------

# Combinar todo lo anterior para crear nuevo dataset resumido

#Resumir frutos y diametro por sitio
dt_sum <- dt |> 
  group_by(site_name) |> 
  summarise(mean_fruits = mean(count, na.rm = TRUE),
            mean_diam = mean(stem_diameter_cm, na.rm = TRUE))

dt_sum




## ---------------------------------------------------------
# COMBINAR BASES DE DATOS
## ---------------------------------------------------------

# Cargar nuevo dataset con info para especies
sp_info <- read_csv("files/species_attributes.csv")

glimpse(sp_info)

## ---------------------------------------------------------

# Contar casos
sp_info |> count(pollinator_code)

sp_info |> count(shade_tolerance)

## ---------------------------------------------------------

#crear dataset combinado
dt_comb <- dt_raw |> 
  left_join(sp_info, by = c("species_name"))





## ---------------------------------------------------------
# REORGANIZAR BASES DE DATOS
## ---------------------------------------------------------

dt |> 
  group_by(site_name, year) |> 
  summarise(fruits = max(count)) |> 
  pivot_wider(names_from = site_name,
              values_from = fruits)
