library(dplyr) ; library(tidyverse)
library(sf); library(ggplot2)


# charger les shapefiles & mettre projection Lambert 93
epci_fonds <- st_read("C:/Users/Rania El Fahli/Downloads/ADMIN-EXPRESS-COG_3-1__SHP_WGS84G_FRA_2022-04-15/ADMIN-EXPRESS-COG_3-1__SHP_WGS84G_FRA_2022-04-15/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2022-04-15/ADECOG_3-1_SHP_WGS84G_FRA/EPCI.shp")

# mayotte <- st_read("C://Users//Rania El Fahli//Documents//mayotte.shp")
# epci_fonds <- sf::st_transform(epci_fonds, crs = st_crs("EPSG:3949"))
# mayotte <- sf::st_transform(mayotte, crs = st_crs("EPSG:3949"))

# récupérer codes géo 22 des EPCI de chaque DROM
source("C:/Users/Rania El Fahli/Documents/Atlas/Cartes/Scripts/codes_epci_dom.R")

# séparer les fonds de cartes
fonds_separes <- function(table_sf, code_geo, code_filtre) {
  
  france_m <- table_sf %>%
    dplyr::filter(!{{code_geo}} %in% get(paste0(code_filtre, "_dom")))
  print(paste("Exclure:", paste0(code_filtre, "_dom")))
  print(head(france_m))
  
  guadeloupe <- table_sf %>%
    dplyr::filter({{code_geo}} %in% get(paste0(code_filtre, "_guad")))
  print(paste("Inclure Guadeloupe:", paste0(code_filtre, "_guad")))
  print(head(guadeloupe))
  
  reunion <- table_sf %>%
    dplyr::filter({{code_geo}} %in% get(paste0(code_filtre, "_reun")))
  print(paste("Inclure Reunion:", paste0(code_filtre, "_reun")))
  print(head(reunion))
  
  martinique <- table_sf %>%
    dplyr::filter({{code_geo}} %in% get(paste0(code_filtre, "_mart")))
  print(paste("Inclure Martinique:", paste0(code_filtre, "_mart")))
  print(head(martinique))
  
  guyane <- table_sf %>%
    dplyr::filter({{code_geo}} %in% get(paste0(code_filtre, "_guy")))
  print(paste("Inclure Guyane:", paste0(code_filtre, "_guy")))
  print(head(guyane))
  
  return(list(france_m = france_m, guadeloupe = guadeloupe, reunion = reunion, martinique = martinique, guyane = guyane))
}

fonds_separes_epci <- fonds_separes(
  table_sf = epci_fonds, 
  code_geo = CODE_SIREN, 
  code_filtre = "epci"
)


# try2 --------------------------------------------------------------------

mapview::mapview(fonds_separes_epci[["france_m"]])

# calcul des bbox : 

bbox_drom_fm <- lapply(fonds_separes_epci, function(x) {
  assign(paste0(names(x)), x |> sf::st_bbox(), envir = .GlobalEnv)
})

# Calcul des paramètres dx et dy de translation :
dx_guadeloupe <- bbox_drom_fm[["france_m"]]$xmin - bbox_drom_fm[["guadeloupe"]]$xmax
dy_guadeloupe <- bbox_drom_fm[["france_m"]]$ymin -  bbox_drom_fm[["guadeloupe"]]$ymax


g <- fonds_separes_epci[["guadeloupe"]]
test = st_geometry(g) + c(dx_guadeloupe , dy_guadeloupe)
test_2 = sf::st_set_geometry(g, test)
test_2 <- st_set_crs(test_2, "EPSG:3949")
mapview::mapview(test_2)


# sans corse  -------------------------------------------------------------

tab_22 <- COGugaison::table_supracom_2022

epci_corse <- tab_22 |>
  dplyr::filter(REG == "94") |>
  select(c(EPCI)) |>
  distinct(EPCI)
epci_corse <- epci_corse$EPCI

epci_fm_sc <- fonds_separes_epci[["france_m"]] |>
  dplyr::filter(!CODE_SIREN %in% epci_corse)

mapview::mapview(epci_fm_sc)
# le xmax et ymin de france_m est par rapport à la corse, on essaye sans la CORSE 

bbox_fm_sc <- sf::st_bbox(epci_fm_sc)

dx_guadeloupe <- (bbox_fm_sc$xmin- bbox_drom_fm[["guadeloupe"]]$xmin) - 95000
dy_guadeloupe <- (bbox_fm_sc$ymax- bbox_drom_fm[["guadeloupe"]]$ymax) - 500000
g <- fonds_separes_epci[["guadeloupe"]]qgis_plugins()
test = st_geometry(g) + c(dx_guadeloupe , dy_guadeloupe)
test_2 = sf::st_set_geometry(g, test)
test_2 <- st_set_crs(test_2, 2154)
mapview::mapview(test_2)

test_fm_guad <- rbind(epci_fm_sc, test_2)
plot(test_fm_guad)


# qgis --------------------------------------------------------------------

# distance entre les 2 

regions_france_22 <- sf::st_read("C:/Users/Rania El Fahli/Downloads/ADMIN-EXPRESS-COG_3-1__SHP_WGS84G_FRA_2022-04-15(1)/ADMIN-EXPRESS-COG_3-1__SHP_WGS84G_FRA_2022-04-15/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2022-04-15/ADECOG_3-1_SHP_WGS84G_FRA/REGION.shp", quiet = T)

france_m_sc <- regions_france_22 |>
  dplyr::filter(!INSEE_REG %in% c("03", "04", "06", "01", "02", "94")) |>
  sf::st_union()

sf::st_centroid(france_m_sc)
sf::st_centroid(subset(regions_france_22, INSEE_REG == "01"))

# centroide région Guadeloupe : -61.53983 16.19761
# centroide france métro (hors corse) : 2.459899 46.57676 

france_bbox_sc <- sf::st_bbox(france_m_sc)
sf::st_bbox(subset(regions_france_22, INSEE_REG == "01"))


dx_guadeloupe <-france_bbox_sc$xmin - bbox_drom_fm[["guadeloupe"]]$xmax +2
dy_guadeloupe <- france_bbox_sc$ymin -  bbox_drom_fm[["guadeloupe"]]$ymax +2


g <- fonds_separes_epci[["guadeloupe"]]
test = st_geometry(g) + c(dx_guadeloupe , dy_guadeloupe)
test_2 = sf::st_set_geometry(g, test)
test_2 <- st_set_crs(test_2, "EPSG:4326")
mapview::mapview(test_2)
mapview::mapview(g)

library(qgisprocess)
qgis_enable_plugins()
qgis_search_algorithms("translate")
alg = "native:translategeometry"
union_arguments = qgis_get_argument_specs(alg)
union_arguments

mapview::mapview(fonds_separes_epci[["martinique"]])

test_martinique_wgs84 <- qgis_run_algorithm(
  alg, INPUT = fonds_separes_epci[["martinique"]], DELTA_X = , DELTA_Y = 
)

test_2 = sf::st_as_sf(test_martinique_wgs84)

mapview::mapview(test_2)


# distance x2-x1, y2-y1 -------------------------------------------

calcul_distances <- function(x, adjust_value) {
  
  dx <- france_bbox_sc$xmin - x$xmax + adjust_value
  dy <- france_bbox_sc$ymin - x$ymax + adjust_value
  
  return(list(dx, dy))
  
}


distances <- bbox_drom_fm |>
  lapply(calcul_distances, adjust_value = c(2, 2.2,2.5,2.7,2.9))

test_martinique_wgs84 <- qgis_run_algorithm(
  alg, INPUT = fonds_separes_epci[["martinique"]], DELTA_X = , DELTA_Y = 
)




