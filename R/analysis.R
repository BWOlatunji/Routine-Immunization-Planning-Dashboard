library(tidyverse)
library(lorem)
library(bsicons)
library(leaflet)
library(mapview)
library(osmdata)    # Open Street Map Overpass API
library(osrm)       # Open Street Map Routing API

library(sf)         # Simple Features
library(nngeo)      # Nearest Neighbors
library(geojsonsf)

health_care_facilities_geo <- geojson_sf("data/health-care-facilities-primary-secondary-and-tertiary.geojson")|>
  mutate(ri_service_status = replace_na(ri_service_status,"0"),
         cce_quantity      = replace_na(cce_quantity, "0"),
         cce_available     = replace_na(cce_available,"0"),
         functional_status = replace_na(functional_status, "Unknown"),
         accessibility = replace_na(accessibility, "Inaccessible")) |> 
  mutate(accessibility = ifelse(accessibility == "Inaccessible", 0, 1)) |> 
  select(-c(alternate_name, global_id,contact_name,contact_phone,
            cce_lastupdated,ward_code,ward_name,source,lga_code,state_code))

write_rds(health_care_facilities_geo, "data/health_care_facilities_geo.rds")

health_care_facilities_geo |> 
  filter(cce_quantity>0)

# total number of health facilities with CCE = 3629 rows

health_care_facilities_geo |> 
  filter(cce_quantity>0 & ri_service_status == 1) 

# total number of health facilities with CCE and offer RI services = 1801 rows


health_care_facilities_geo |> 
  filter(cce_quantity>0 & ri_service_status == 0) 

# # total number of health facilities with CCE but does not offer RI services = 1828 rows


# |>
# mutate(cc_store_type = case_when(
#   cce_quantity == 0 ~ "CCE Unequipped",
#   cce_quantity == 1 ~ "CCE Equipped",
#   cce_quantity %in% c(2, 3) ~ "LGA Cold Store",
#   cce_quantity >= 4 ~ "Zonal(State) Cold Store"
# )) |>
#   relocate(cc_store_type, .after = cce_quantity)



install.packages("ndjson")
library(ndjson)
yelp_business <- ndjson::stream_in("data/yelp_dataset/yelp_academic_dataset_business.json")

