library(dplyr) ; library(tidyverse)
library(sf); library(ggplot2)

epci_fonds <- st_read("C:/Users/Rania El Fahli/Downloads/ADMIN-EXPRESS-COG_3-1__SHP_WGS84G_FRA_2022-04-15/ADMIN-EXPRESS-COG_3-1__SHP_WGS84G_FRA_2022-04-15/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2022-04-15/ADECOG_3-1_SHP_WGS84G_FRA/EPCI.shp")

mayotte <- st_read("C://Users//Rania El Fahli//Documents//mayotte.shp")
epci_fonds <- sf::st_transform(epci_fonds, crs = st_crs("EPSG:3949"))
mayotte <- sf::st_transform(mayotte, crs = st_crs("EPSG:3949"))

# récupérer codes géo 22 des EPCI de chaque DROM
source("C:/Users/Rania El Fahli/Documents/Atlas/Cartes/Scripts/codes_epci_dom.R")

# séparer les fonds de cartes
fonds_separes <- function(table_sf, code_geo, code_filtre) {
  
  france_m <- table_sf %>%
    dplyr::filter(!{{code_geo}} %in% get(paste0(code_filtre, "_dom")))|>
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

fonds_separes_epci <- fonds_separes(
  table_sf = epci_fonds, 
  code_geo = CODE_SIREN, 
  code_filtre = "epci"
)

bbox_drom_fm <- lapply(fonds_separes_epci, function(x) {
  assign(paste0(names(x)), x |> sf::st_bbox(), envir = .GlobalEnv)
})

tab_22 <- COGugaison::table_supracom_2022

epci_corse <- tab_22 |>
  dplyr::filter(REG == "94") |>
  select(c(EPCI)) |>
  distinct(EPCI)
epci_corse <- epci_corse$EPCI

epci_fm_sc <- fonds_separes_epci[["france_m"]] |>
  dplyr::filter(!CODE_SIREN %in% epci_corse)

bbox_fm_sc <- sf::st_bbox(epci_fm_sc)


calcul_distances <- function(x) {
  
  dx <- bbox_fm_sc$xmin - x$xmax 
  dy <- bbox_fm_sc$ymin - x$ymax
  
  return(list(dx, dy))
  
}
library("qgisprocess")

distances <- bbox_drom_fm |>
  lapply(calcul_distances)

alg = "native:affinetransform"
union_arguments = qgis_get_argument_specs(alg)
union_arguments


# guadeloupe --------------------------------------------------------------


test_guadeloupe_wgs84 <- qgis_run_algorithm(
  alg, INPUT = fonds_separes_epci[["guadeloupe"]], DELTA_Y = distances[["guadeloupe"]][[2]] + 480900 , DELTA_X = distances[["guadeloupe"]][[1]] +90900
)

test_2 = sf::st_as_sf(test_guadeloupe_wgs84)
test_2 <- test_2 |>
  dplyr::rename("geometry" = geom)
# test_2 = rbind(epci_fm_sc, test_2)
mapview::mapview(test_2)

test_2 = rbind(epci_fm_sc, test_2)


# martinique --------------------------------------------------------------


test_martinique_wgs84 <- qgis_run_algorithm(
  alg, INPUT = fonds_separes_epci[["martinique"]], DELTA_Y = distances[["martinique"]][[2]] + 580900, DELTA_X = distances[["martinique"]][[1]] +  90800 
)

test_martinique = sf::st_as_sf(test_martinique_wgs84)
test_martinique <- test_martinique |>
  dplyr::rename("geometry" = geom)
mapview::mapview(test_martinique)

total_1 = rbind(epci_fm_sc, test_2, test_martinique)
mapview::mapview(total_1)


# réunion -----------------------------------------------------------------


test_reunion_wgs84 <- qgis_run_algorithm(
  alg, INPUT = fonds_separes_epci[["reunion"]], DELTA_Y = distances[["reunion"]][[2]] + 380900, DELTA_X = distances[["reunion"]][[1]] +  90800
)

test_reunion = sf::st_as_sf(test_reunion_wgs84)
test_reunion <- test_reunion |>
  dplyr::rename("geometry" = geom)
mapview::mapview(test_reunion)

total_1 = rbind(epci_fm_sc, test_2, test_martinique)
mapview::mapview(total_1)