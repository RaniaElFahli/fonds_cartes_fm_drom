library(dplyr) ; library(tidyverse)
library(sf); library(ggplot2)
library("qgisprocess")

source("C:/Users/Rania El Fahli/Documents/MIGCOM_repo/Migrations_residentielles_communes.Insee/fonctions/carte_bv_continue_dep.R")
bv22 <- st_read("C:/Users/Rania El Fahli/Documents/Atlas/Fonds de carte/BV2022/bv2022_2023.shp", quiet = T)

mayotte <- st_read("C://Users//Rania El Fahli//Documents//mayotte.shp")
bv22 <- sf::st_transform(bv22, crs = st_crs("EPSG:3949"))
mayotte <- sf::st_transform(mayotte, crs = st_crs("EPSG:3949"))

# récupérer codes géo 22 des EPCI de chaque DROM

# séparer les fonds de cartes
fonds_separes <- function(table_sf, code_geo, code_filtre) {
  
  france_m <- table_sf %>%
    dplyr::filter(!{{code_geo}} %in% get(paste0(code_filtre, "_dom")) &
                    !grepl("^976", {{code_geo}}))|>
    mutate(drom_fm = "france_m")
  print(paste("Exclure:", paste0(code_filtre, "_dom")))
  print(head(france_m))
  
  guadeloupe <- table_sf %>%
    dplyr::filter({{code_geo}} %in% get(paste0(code_filtre, "_guad"))) |>
    mutate(drom_fm = "guadeloupe")
  print(paste("Inclure Guadeloupe:", paste0(code_filtre, "_guad")))
  print(head(guadeloupe))
  
  reunion <- table_sf %>%
    dplyr::filter({{code_geo}} %in% get(paste0(code_filtre, "_reun"))) |>
    mutate(drom_fm = "reunion")
  print(paste("Inclure Reunion:", paste0(code_filtre, "_reun")))
  print(head(reunion))
  
  martinique <- table_sf %>%
    dplyr::filter({{code_geo}} %in% get(paste0(code_filtre, "_mart"))) |>
    mutate(drom_fm = "martinique")
  print(paste("Inclure Martinique:", paste0(code_filtre, "_mart")))
  print(head(martinique))
  
  guyane <- table_sf %>%
    dplyr::filter({{code_geo}} %in% get(paste0(code_filtre, "_guy"))) |>
    mutate(drom_fm = "guyane")
  print(paste("Inclure Guyane:", paste0(code_filtre, "_guy")))
  print(head(guyane))
  
  return(list(france_m = france_m, guadeloupe = guadeloupe, reunion = reunion, martinique = martinique, guyane = guyane))
}

fonds_separes_bv <- fonds_separes(
  table_sf = bv22, 
  code_geo = bv2022, 
  code_filtre = "bv"
)

# calcul distances bbox ---------------------------------------------------



bbox_drom_fm <- lapply(fonds_separes_bv, function(x) {
  assign(paste0(names(x)), x |> sf::st_bbox(), envir = .GlobalEnv)
})

compo_bv22 <- readxl::read_excel("C:/Users/Rania El Fahli/Documents/Atlas/Données brutes/Tables_géo_zonages/BV2022_au_01-01-2022.xlsx", sheet = 2, skip = 5)

bv_corse <- compo_bv22 |>
  dplyr::filter(REG == "94") |>
  select(c(BV2022)) |>
  distinct(BV2022)
bv_corse <- bv_corse$BV2022

bv_fm_sc <- fonds_separes_bv[["france_m"]] |>
  dplyr::filter(!bv2022 %in% bv_corse)

bbox_fm_sc <- sf::st_bbox(bv_fm_sc)


calcul_distances <- function(x) {
  
  dx <- bbox_fm_sc$xmin - x$xmax 
  dy <- bbox_fm_sc$ymin - x$ymax
  
  return(list(dx, dy))
  
}


distances <- bbox_drom_fm |>
  lapply(calcul_distances)

# récupérer algorithme qgis
alg = "native:affinetransform"

# check algorithm arguments
# affine_arguments = qgis_get_argument_specs(alg)
# affine_arguments 

mayotte_bbox <- sf::st_bbox(mayotte)
dx <- bbox_fm_sc$xmin - mayotte_bbox$xmax 
dy <- bbox_fm_sc$ymin - mayotte_bbox$ymax

# guadeloupe --------------------------------------------------------------


test_guadeloupe_wgs84 <- qgis_run_algorithm(
  alg, INPUT = fonds_separes_bv[["guadeloupe"]], DELTA_Y = distances[["guadeloupe"]][[2]] + 480900 , DELTA_X = distances[["guadeloupe"]][[1]] +90900
)

test_2 = sf::st_as_sf(test_guadeloupe_wgs84)
test_2 <- test_2 |>
  dplyr::rename("geometry" = geom)
# test_2 = rbind(bv_fm_sc, test_2)
mapview::mapview(test_2)

test_2 = rbind(bv_fm_sc, test_2)


# martinique --------------------------------------------------------------

# valeur_z_martinique <- qgis_run_algorithm(
#   "native:extractzvalues", INPUT = fonds_separes_bv[["martinique"]],
#   SUMMARIES = 0
# )
# hh <- valeur_z_martinique |>
#   sf::st_as_sf()

test_martinique_wgs84 <- qgis_run_algorithm(
  alg, INPUT = fonds_separes_bv[["martinique"]], DELTA_Y = distances[["martinique"]][[2]] + 340900, DELTA_X = distances[["martinique"]][[1]] +  80600)

test_martinique = sf::st_as_sf(test_martinique_wgs84)
test_martinique <- test_martinique |>
  dplyr::rename("geometry" = geom)
mapview::mapview(test_martinique)

total_1 = rbind(bv_fm_sc, test_2, test_martinique)
mapview::mapview(total_1)

# réunion -----------------------------------------------------------------


test_reunion_wgs84 <- qgis_run_algorithm(
  alg, INPUT = fonds_separes_bv[["reunion"]], DELTA_Y = distances[["reunion"]][[2]] +  690900, DELTA_X = distances[["reunion"]][[1]] + 2046800, SCALE_X = 0.82, SCALE_Y = 0.82)

test_reunion = sf::st_as_sf(test_reunion_wgs84)
test_reunion <- test_reunion |>
  dplyr::rename("geometry" = geom)
mapview::mapview(test_reunion)

 total_3 = rbind(bv_fm_sc, test_2, test_martinique, test_reunion)
 mapview::mapview(total_3)
 


# Guyane ------------------------------------------------------------------

 test_guyane_wgs84 <- qgis_run_algorithm(
   alg, INPUT = fonds_separes_bv[["guyane"]], DELTA_Y = distances[["guyane"]][[2]] + 840900, DELTA_X = distances[["guyane"]][[1]] +  5600)
 
 test_guyane = sf::st_as_sf(test_guyane_wgs84)
 test_guyane <- test_guyane |>
   dplyr::rename("geometry" = geom)
 mapview::mapview(test_guyane)
 
# prépa couche vectoriel pour test déplacement polygones sur QGIS -------------------------------------------------------------------

#  source("C:/Users/Rania El Fahli/Documents/MIGCOM_repo/Migrations_residentielles_communes.Insee/fonctions/carte_bv_continue_dep.R")
#  bv22 <- st_read("C:/Users/Rania El Fahli/Documents/Atlas/Fonds de carte/BV2022/bv2022_2023.shp", quiet = T) 
# 
# bv22 <- bv22 |>
#    mutate(
#      region = case_when(
#        bv2022 %in% bv_guad ~ "Guadeloupe", 
#        bv2022 %in% bv_guy ~ "Guyane", 
#        bv2022 %in% bv_reun ~ "Réunion", 
#        bv2022 %in% bv_mart ~ "Martinique", 
#       !bv2022 %in% bv_dom & !grepl("^976", bv2022) ~ "fm", 
#       grepl("^976", bv2022) ~ "Mayotte"
#      )
#    )
# 
# sf::st_write(bv22, "C:/Users/Rania El Fahli/Documents/Atlas/Fonds de carte/BV2022/bv2022_2023_var_region_t2.shp", quiet = T)
#  
# 
# test_bv <- sf::st_read("C:/Users/Rania El Fahli/Documents/Atlas/Fonds de carte/BV2022/bv2022_2023_var_region.shp", quiet = T)
# 
# 
# test_bv |>
#   ggplot() +
#   geom_sf(fill ="pink")
# 
# 
# test_bv2 <- sf::st_read("C:/Users/Rania El Fahli/Documents/Atlas/Fonds de carte/BV2022/bv2022_2023_var_region_t2.shp", quiet = T)
# 
# test_bv2 |>
#   ggplot() +
#   geom_sf(fill = alpha("#FEE4D8", alpha  = 0.3), colour = "grey77",  linewidth = 0.6) +
#   coord_sf(crs = "EPSG:3949") +
#   theme(panel.background = element_blank(), 
#         panel.grid = element_blank(), 
#         axis.line = element_blank(), 
#         axis.text.x = element_blank(), 
#         axis.text.y = element_blank(), 
#         axis.ticks.x = element_blank(), 
#         axis.ticks.y = element_blank(), 
#         legend.position = "none", 
#         legend.title = element_text(size = 18),
#         legend.text = element_text(size = 16), 
#         text = element_text(family = "Chivo")
#   )
#  rendu pas terrible sur QGIS, déplacement à la main des polygones suffit pas, marche pas aussi bien que 
#  transformation affine (les polygones sont un peu écrasés & le changement de projection CC49 -> WGS84 pas fou)

 europe_eurostat <- sf::st_read("C:/Users/Rania El Fahli/Downloads/NUTS_RG_03M_2021_4326.shp/NUTS_RG_03M_2021_4326.shp")
 
 europe_hab <- europe_eurostat |>
   dplyr::group_by(CNTR_CODE) |>
   dplyr::summarise(
     geometry = sf::st_union(geometry)
   ) |>
   ungroup()
 
 europe_hab <- st_transform(europe_hab, crs = st_crs("EPSG:3949")) 
 france <- europe_hab |>
   dplyr::filter(CNTR_CODE == "FR")
 
test_bv3 <- sf::st_read("C:/Users/Rania El Fahli/Downloads/bv2022_2023_drom_rap/bv2022_2023_drom_rap.shp")
 test_bv3|>
   ggplot() +
   geom_sf(fill = alpha("#FEE4D8", alpha  = 0.3), colour = "grey77",  linewidth = 0.6) +
   geom_sf(data = subset(europe_hab, CNTR_CODE != "FR"),
           fill = "grey87", colour = "white", linewidth = 0.7) +
   geom_sf(data = france, fill = "transparent",, colour = "grey3", linewidth = 0.7) +
   theme(panel.background = element_rect(fill = "aliceblue"))+
   coord_sf(crs = "EPSG:3949") + 
   coord_sf(xlim = c(2230894, 1091069), ylim = c(7347866, 8481077))
 