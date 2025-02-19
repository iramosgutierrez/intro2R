library(here)
library(tidyverse)


# Load data ####
dt_raw <- read_csv("individual_seed_production.csv")

# Preview data####
# see dimensions
dim(dt_raw)

#See data structure
glimpse(dt_raw)

#See first rows
head(dt_raw)


# Reorder data####
# rows, by 'count' (ascending)
dt_raw |>
  arrange(count)


#rows, by 'count' (descending)
dt_raw |>
  arrange(desc(count))


#rows, by several factors (order matters!)
dt_raw |>
  arrange(site_name, species_name, desc(count))


#columns, .before = "column" / .after = "column"
dt_raw |>
  relocate(year, .after = megaplot)


# Custom columns ####
#Rename columns
dt_raw |>
  rename(site = site_name)



#Select some columns
dt_raw |>
  select(site_name, year, species_name, count)


#Select all columns except some
dt_raw |>
  select(-c(megaplot, plot, trap))


# `select()` also works as rename and relocate
dt <- dt_raw |>
  select(site = site_name,
         year,
         species_name,
         plant_ID,
         count,
         method = general_method,
         stem_cm = stem_diameter_cm,
         trap_area_m2)


head(dt)

# Custom rows ####
# select unique values (works as select, too!)
  # 1 column...
dt |>
  distinct(site)
  # ... or more!
dt |>
  distinct(site, method)


# select specified values
dt |>
  filter(site == "BNZ")

dt |>
  filter(site %in% c("AEC", "AND", "BNZ")) |>
  filter(count >= 10)


# Modify values ####

# a unique value
dt |>
  mutate(dataset = "Nigro et al., 2025")

# value related to other columns
dt |>
  mutate(fruits_per_m2 = count/trap_area_m2) |> 
  View()

# conditional values
  # one or other

 dt |>
  # quitar un valor equivocado
  mutate(count = if_else(count > 200000, NA, count))

dt |>
  # agrupar 2 clases
  mutate(time = if_else(year < 1980, "old", "new"))


  #several options (warning! order matters, and NAs are given for no matching queries)
dt |>
  mutate(nivel_frutos = case_when(
    count <= 100 ~ "bajo",
    count > 100 & count <= 1000 ~ "medio",
    count > 1000 ~ "alto"))



