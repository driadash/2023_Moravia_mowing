library(sf)
library(tidyverse)

read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data/headers.xlsx)', 1) |>
  group_by(site_ID) |> summarise(across(c('lat', 'long'), mean)) |>
  st_as_sf(coords = c('long', 'lat'), crs = 4326) |>
  write_sf('data/mapa.gpkg')

read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data/headers.xlsx)', 1)  |>
  distinct(site_ID, site_name_interpunction) |>
  mutate(x = paste0(site_ID, ': ', site_name_interpunction)) |>
  pull(x) |> paste0(collapse = ', ') |>
  writeClipboard()

