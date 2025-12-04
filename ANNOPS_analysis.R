## Importation, screening and cleaning ----
install.packages(c("MASS", "janitor", 
                   "patchwork", "scales", "leaflet"))
library("visdat")
library("MASS")
library("janitor")
library("patchwork")
library("tidyverse")
library("leaflet")
sen0 <- read_csv("NIEL-SN-2304_anospp_dedup-2.csv")
sen <- sen0[c(1:3, 6, 8:10, 12, 19, 23, 36:37, 45, 46, 51:53, 56, 57)]
sen <- relocate(sen, c(nnovae_mosquito_species, plasmodium_species, blood_meal, date_of_collection), .after = series)
head(sen)
summary(sen)
colnames(sen)
sen %>% group_by(collection_location, nnovae_mosquito_species) %>% count() 
unique(sen$nnovae_mosquito_species) %>% length()
unique(sen$collection_location)

# Adding columns for geographic subdivisions
install.packages("sf")
install.packages("geodata")
library(sf)
library(geodata)
regions <- st_read("gadm41_SEN_1.shp")
districts <- st_read("gadm41_SEN_4.shp")
View(sen %>% filter(is.na(decimal_longitude) | is.na(decimal_latitude)))
sen <- sen %>%
  filter(!is.na(decimal_longitude) | !is.na(decimal_latitude)) # Removing the NA's

sen <- st_as_sf(   # creating an spatial object for reading the coordinates to the appropriate departments
  x = sen,
  coords = c("decimal_longitude", "decimal_latitude"), 
  crs = 4326               
)

sen <- st_join(sen, districts, join = st_within) # Adding the districts Names
sen %>% filter(is.na(COUNTRY) | is.na(regions) | is.na(departements)) %>% 
  distinct(collection_location) # Mbine Coly and Paka Thiar Ndiayene coordinates are not accurate

sites_2025 <- read_csv("GPS_coordonnates_2025.csv") # Importing @025 sites coordinates
!is.na(sites_2025) %>% length() # NAs checking
sites_2025 <- sites_2025 %>% rename(longitude = `Vil-Longitude`,
                                    latitude = `Vil-Lattitude`) %>% 
              drop_na(longitude, latitude) %>% # Removes rows where EITHER is NA
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )
sites_2025 <- st_join(sites_2025, districts, join = st_within) # Districts names added

sites_2025 <-  sites_2025 %>% select(-GID_0, -GID_1,-GID_2,
                      -GID_3, -GID_4, -VARNAME_4:-CC_4) %>% # Cleaning
  rename(regions = NAME_1, 
                      departements = NAME_2,
                      arrondissements = NAME_3,
                      commune = NAME_4)  # and renaming 2025 sites coordinates

sen <- sen %>% select(-GID_0, -GID_1,-GID_2,
       -GID_3, -GID_4, -VARNAME_4:-CC_4) %>% # Cleaning
  rename(regions = NAME_1, 
         departements = NAME_2,
         arrondissements = NAME_3,
         commune = NAME_4) # and renaming
# Creating a data base that contains communes from both campains ----
# Checking NAs: 
sum(is.na(sen$commune))
# Only Mbine Coly and Paka Thiar Ndiayene did not have because of their coodinates
sites_2025 %>%  filter(is.na(commune)) 
# No missing data

sites_2025_without_geom <- sites_2025 %>% st_drop_geometry() # Droping geom data to join after
same_area_sites <-  semi_join(sen, sites_2025_without_geom, by = "commune")

# Visualising the area overlapping ----

summary(same_area_sites)
plot(same_area_sites$geometry)

plot(same_area_sites["regions"])
install.packages("leaflet")
library(leaflet)

leaflet(data = same_area_sites) %>%
  addTiles() %>%  # Adds the default OpenStreetMap basemap
  addCircleMarkers(
    radius = 3,       # Control the size of the points
    color = "blue",   # Control the color
    popup = ~commune  # Display the commune name when a point is clicked
  )

# Trying to reflect the sample sizes 
###select(cummune, geometry) %>% 

area_summary <- same_area_sites %>%
  st_drop_geometry() %>% # Temporarily drop geometry to make counting easy
  count(commune, name = "sample_count") %>% # Count samples per commune
  right_join(same_area_sites %>% unique(), by = "commune") %>% # Re-attach one unique geometry for each commune
  st_as_sf() # Convert back to spatial object

# Now ploting
library(leaflet)
library(dplyr)
library(sf)

# 2. Create a color palette function
# 'Set1' is a common palette for distinct categories
color_palette <- colorFactor(palette = "Paired", domain = same_area_sites$departements)

# 3. Use the palette in your map
leaflet(data = area_summary) %>%
  addTiles() %>%
  addCircleMarkers(
    # Scale the radius using the count data (adjust the '0.5' multiplier as needed)
    radius = ~sqrt(sample_count) * 0.5, 
    color = ~color_palette(departements),
    stroke = FALSE, 
    fillOpacity = 0.7,
    # Add informative popups and labels
    popup = ~paste(
      "Commune: ", commune, "<br>", 
      "Samples: ", sample_count
    ),
    label = ~as.character(sample_count)
  ) %>%
  # 4. Add a legend to explain the colors
  addLegend("bottomright", pal = color_palette, values = ~departements,
            title = "District")

# Trying to highlight Kedougou, Tamba and Kolda
area_summary <- area_summary %>% 
  mutate(is_highlight = regions %in% c("Kédougou", "Kolda", "Tambacounda"))

leaflet(data = area_summary) %>% 
  addTiles() %>% 
  addCircleMarkers(
    radius = ~sqrt(sample_count) * 0.5,
    color = ~ifelse(is_highlight, "red", color_palette(departements)),
    stroke = TRUE,
    weight = ~ifelse(is_highlight, 3, 1),
    fillOpacity = 0.7,
    popup = ~paste(
      "Commune: ", commune, "<br>",
      "Samples: ", sample_count
    ),
    label = ~as.character(sample_count)
  ) %>% 
  addLegend(
    "bottomright",
    pal = color_palette,
    values = ~departements,
    title = "District"
  )


library(sf)
library(dplyr)
library(tidyr) # Required for unnest() if coordinates are in a matrix format

# Exporting the overlapping communes
# Extract coordinates into new columns
overlapping_data <- same_area_sites %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(longitude = X, latitude = Y) %>%
  bind_cols(same_area_sites, .) %>%
  st_drop_geometry()
getwd()

# Export to a CSV file
write.csv(overlapping_data, "same_area_sites_with_coords.csv", row.names = FALSE)
363	+ 868	+ 36
# Install the package if you haven't already:
# install.packages("dplyr")

# Load the package
library(dplyr)


# 1. Filter the data frame to exclude the specified conditions:
#    - Exclude rows where "nnovae_mosquito_species" is "Too_few_targets"
#    - Exclude rows where "bloddmeal" is "N"

filtered_sites <- overlapping_data %>%
  filter(!(nnovae_mosquito_species == "TOO_FEW_TARGETS" | blood_meal == "N")) 

# 2. Perform stratified sampling to get a total of 1000 rows, 
#    reflecting the proportion of each location size.

# Calculate the total number of rows in the filtered data
total_filtered_rows <- nrow(filtered_sites)
target_sample_size <- 1000

# Determine the fraction needed from each group
# Since we want the result to be proportional, we aim for a consistent fraction overall.
# We want a total of 1000 samples, the fraction is 1000 / total_filtered_rows.

# Check if there are enough rows to sample 1000 in total
if (total_filtered_rows >= target_sample_size) {
  set.seed(42) # For reproducibility
  
  # Group by the "departements" and sample a fraction (prop) from each group
  sampled_sites_stratified <- filtered_sites %>%
    group_by(departements) %>%
    slice_sample(prop = target_sample_size / total_filtered_rows) %>%
    ungroup() # Ungroup the data frame after sampling
  
} else {
  print(paste("Warning: Only", total_filtered_rows, "rows available after filtering. Stratified sampling not possible for target size 1000."))
  sampled_sites_stratified <- filtered_sites # Return all available rows if fewer than 1000
}

# View the counts per location size in the final sample to verify proportions
if (nrow(sampled_sites_stratified) > 0) {
  print("Counts per location size in the final sample:")
  print(table(sampled_sites_stratified$departements))
}


write.csv(sampled_sites_stratified, "samples_to_run.csv", row.names = FALSE)




# Trying to have at least one sample in each departements
set.seed(42)

total_filtered_rows <- nrow(filtered_sites)

  
  # ---- 1. Sample at least 1 row per departement

base_sample <- filtered_sites %>%
    group_by(departements) %>%
    slice_sample(n = 1, replace = FALSE) %>%
    ungroup()
  
  base_n <- nrow(base_sample)
  remaining_needed <- target_sample_size - base_n
  total_remaining <- nrow(remaining_pool)
    
    # Remove rows already used
    remaining_pool <- filtered_sites %>%
      anti_join(base_sample, by = "departements") 
    
    total_remaining <- nrow(remaining_pool)
    
    # Protect against impossible sampling

      
      # Compute sampling proportion safely
safe_prop <- min(1, remaining_needed / total_remaining)
      
stratified_extra <- remaining_pool %>%
        group_by(departements) %>%
        slice_sample(prop = safe_prop, replace = FALSE) %>%
        ungroup()
      
      # Bind guaranteed sample + stratified sample
      combined <- bind_rows(base_sample, stratified_extra)
      
      # Trim to the exact target size
      sampled_sites_stratified <- combined %>%
        slice_sample(n = target_sample_size)
      
      
      # No rows remaining after base sample — use only base
      sampled_sites_stratified <- base_sample

    

    
    # More departements than target size
    sampled_sites_stratified <- base_sample %>%
      slice_sample(n = target_sample_size)

