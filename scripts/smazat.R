library(tidyverse)
library(sf)
library(writexl)

read_sf(r'(C:\Users\krystof\Desktop\Trtina_2023\Bila\data.gpkg)',
          layer = 'Verca_2021_63639e72_050b_4a26_b17b_10ad01a73a1f') |> 
  write_xlsx(r'(C:\Users\krystof\Desktop\Trtina_2023\Bila2023_VK.xlsx)')

