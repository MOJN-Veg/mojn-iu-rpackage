#' @importFrom magrittr %>% %<>%

pkg_globals <- new.env(parent = emptyenv())

#' Fetch IU data from AGOL and do preliminary data wrangling
#'
#' @param IU_urls list of IU database URLs
#' @param agol_username Authentication token (not needed for public layers)
#'
#' @return A list of data frames and metadata
#' @export

# TODO: add variables that were joined to data tables to each metadata table
loadAndWrangleIU <- function(iu_urls = c("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_IU_Database/FeatureServer",
                                           "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_IU_Site/FeatureServer",
                                           "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_IU_UnknownPlant/FeatureServer",
                                           "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_IU_RepeatPhotos/FeatureServer"),
                                     agol_username = "mojn_data", tables_to_remove = c("Site - DEPRACATED"), cols_to_remove = c("objectid", "InstanceName", "^app_.*", "GapsKey", "^Shrub.*")) {
  flattened_data <- list(data = list(),
                         metadata = list())

  # Import aspen databases
  raw_data <- lapply(iu_urls, function(url){
    fetchagol::fetchRawData(url, agol_username)})

  # TODO: Change this to apply if possible
  # Add the tables in all the IU databases to one dataframe
  for(i in 1:length(raw_data)) {
    for(j in 1:length(raw_data[[i]])) {
      for(k in 1:length(raw_data[[i]][[j]])) {
        #if(names(raw_data[[i]][[j]])[[k]] != 'Site - DEPRACATED' & (!names(raw_data[[i]][[j]])[[k]] %in% cols_to_remove))
          if(!names(raw_data[[i]][[j]])[[k]] %in% tables_to_remove)
          # Add each table in data and metadata to new data frame
    flattened_data[[j]][[names(raw_data[[i]][[j]])[[k]]]] <- raw_data[[i]][[j]][[k]]
      }
    }
  }

  flattened_data <- fetchagol::cleanData(flattened_data)

  # Rename unknown plant photo table
  names(flattened_data$data)[names(flattened_data$data) == "PhotosRepeat"] <- "unknownPlant_Photo"
  names(flattened_data$metadata)[names(flattened_data$metadata) == "PhotosRepeat"] <- "unknownPlant_Photo"
  flattened_data$metadata$unknownPlant_Photo$table_name <- "unknownPlant_Photo"


  # Create new tables
  flattened_data$data$PointIntercept_Surface <- dplyr::left_join(flattened_data$data$PointIntercept,
                                                                 # dplyr::left_join(dplyr::select(flattened_data$data$PointIntercept, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor"))),
                                                                 dplyr::select(flattened_data$data$PointIntercept_LPI, dplyr::any_of(dplyr::starts_with(c("Meter", "SoilSurface", "PlantBase", "globalid", "parentglobalid")))),
                                                                 by = c("globalid" = "parentglobalid"))
  flattened_data$data$PointIntercept_WoodyVegHeight <- dplyr::left_join(flattened_data$data$PointIntercept,
                                                                        # dplyr::left_join(dplyr::select(flattened_data$data$PointIntercept, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor"))),
                                                                        dplyr::select(flattened_data$data$PointIntercept_LPI, -dplyr::any_of(dplyr::starts_with(c("Overstory", "Canopy", "Soil", "Disturbance", "Shrub", "PlantBase", "CreationDate", "Creator", "EditDate", "Editor")))),
                                                                        by = c("globalid" = "parentglobalid")) %>%
    dplyr::filter(Meter %% 5 == 0)

  lpi_overstory <- dplyr::left_join(flattened_data$data$PointIntercept,
                                    # dplyr::left_join(dplyr::select(flattened_data$data$PointIntercept, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor"))),
                                    dplyr::select(flattened_data$data$PointIntercept_LPI, dplyr::any_of(dplyr::starts_with(c("Meter", "Overstory", "globalid", "parentglobalid")))),
                                    by = c("globalid" = "parentglobalid")) %>%
    dplyr::select(-dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor", "globalid"))) %>%
    dplyr::inner_join(flattened_data$data$PointIntercept_Overstory, by = c("globalid.y" = "parentglobalid")) %>%
    dplyr::rename(Species = Overstory) %>%
    dplyr::mutate(Layer = "overstory")
  names(lpi_overstory) <- stringr::str_remove(names(lpi_overstory), "^Overstory")
  lpi_canopy <- dplyr::left_join(flattened_data$data$PointIntercept,
                                 # dplyr::left_join(dplyr::select(flattened_data$data$PointIntercept, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor"))),
                                 dplyr::select(flattened_data$data$PointIntercept_LPI, dplyr::any_of(dplyr::starts_with(c("Meter", "Canopy", "globalid", "parentglobalid")))),
                                 by = c("globalid" = "parentglobalid")) %>%
    dplyr::select(-dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor", "globalid"))) %>%
    dplyr::inner_join(flattened_data$data$PointIntercept_Canopy, by = c("globalid.y" = "parentglobalid")) %>%
    dplyr::rename(Species = Canopy) %>%
    dplyr::mutate(Layer = "canopy")
  names(lpi_canopy) <- stringr::str_remove(names(lpi_canopy), "^Canopy")

  flattened_data$data$PointIntercept_VegSpecies <- rbind(dplyr::select(lpi_overstory, intersect(names(lpi_canopy), names(lpi_overstory))),
                                                         dplyr::select(lpi_canopy, intersect(names(lpi_canopy), names(lpi_overstory))))

  # Join some general/site data to tables
  flattened_data$data$Gaps_Canopy <- dplyr::left_join(dplyr::select(flattened_data$data$Gaps, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor"))),
                                                      flattened_data$data$Gaps_Canopy,
                                                      by = c("globalid" = "parentglobalid"))
  flattened_data$data$Gaps_Basal <- dplyr::left_join(dplyr::select(flattened_data$data$Gaps, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor"))),
                                                     flattened_data$data$Gaps_Basal,
                                                     by = c("globalid" = "parentglobalid"))
  flattened_data$data$Inventory <- dplyr::left_join(dplyr::select(flattened_data$data$Inventory, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor"))),
                                                    flattened_data$data$Inventory_Species,
                                                    by = c("globalid" = "parentglobalid"))
  flattened_data$data$Frequency_Crust <- dplyr::left_join(dplyr::select(flattened_data$data$Frequency, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor"))),
                                                          dplyr::select(flattened_data$data$Frequency_Quadrats,  -dplyr::any_of(c("globalid"))),
                                                          by = c("globalid" = "parentglobalid"))
  flattened_data$data$Frequency_Species <- dplyr::left_join(dplyr::select(flattened_data$data$Frequency_Crust, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor", "Crust", "CrustPhotoName"))),
                                                            flattened_data$data$Frequency_Species,
                                                            by = c("globalid" = "parentglobalid"))
  flattened_data$data$Density <- dplyr::left_join(dplyr::select(flattened_data$data$Density, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor"))),
                                                  flattened_data$data$Density_Species,
                                                  by = c("globalid" = "parentglobalid"))
  flattened_data$data$SoilStability <- dplyr::left_join(dplyr::select(flattened_data$data$SoilStability, -dplyr::any_of(c("CreationDate", "Creator", "EditDate", "Editor"))),
                                                        flattened_data$data$SoilStability_Measurements,
                                                        by = c("globalid" = "parentglobalid"))



  # Create metadata for new tables that were created
  flattened_data$metadata$PointIntercept_Surface <- list(table_name = "PointIntercept_Surface",
                                                         table_description = "Point Intercept Surface Table",
                                                         fields = flattened_data$metadata$PointIntercept$fields,
                                                         table_id = 1)
  flattened_data$metadata$PointIntercept_Surface$fields <- append(flattened_data$metadata$PointIntercept_Surface$fields, flattened_data$metadata$PointIntercept_LPI$fields[c("Meter", "MeterNote", "SoilSurface", "SoilSurfaceShrub", "SoilSurfacePlantBase", "PlantBaseDead", "PlantBaseSpeciesOther", "PlantBaseUnkNumber", "parentglobalid")])


  flattened_data$metadata$PointIntercept_WoodyVegHeight <- list(table_name = "PointIntercept_WoodyVegHeight",
                                                         table_description = "Point Intercept Woody Vegetation Height Table",
                                                         fields = flattened_data$metadata$PointIntercept$fields,
                                                         table_id = 1)
  flattened_data$metadata$PointIntercept_WoodyVegHeight$fields <- append(flattened_data$metadata$PointIntercept_WoodyVegHeight$fields, flattened_data$metadata$PointIntercept_LPI$fields[c("Meter", "MeterNote", "LPInotes", "EndMeter", "ReviewTableRow", "parentglobalid", "WoodyVegSpecies", "WoodyVegDead","WoodyVegSpeciesOther", "WoodyVegUnkNumber", "WoodyVegHeight_m")])

  flattened_data$metadata$PointIntercept_VegSpecies <- list(table_name = "PointIntercept_VegSpecies",
                                                            table_description = "Point Intercept Vegetation Species Table",
                                                            fields = flattened_data$metadata$PointIntercept$fields,
                                                            table_id = 1)
  names(flattened_data$metadata$PointIntercept_VegSpecies$fields)[names(flattened_data$metadata$PointIntercept_VegSpecies$fields) == "globalid"] <- "globalid.y"
  flattened_data$metadata$PointIntercept_VegSpecies$fields <- append(flattened_data$metadata$PointIntercept_VegSpecies$fields, flattened_data$metadata$PointIntercept_LPI$fields[c("Meter", "MeterNote", "OverstoryJoin", "OverstoryCount", "globalid")])
  flattened_data$metadata$PointIntercept_VegSpecies$fields <- append(flattened_data$metadata$PointIntercept_VegSpecies$fields, flattened_data$metadata$PointIntercept_Overstory$fields[c("Overstory", "OverstoryOrder", "OverstoryDead", "OverstorySpeciesOther", "OverstoryUnkNumber", "OverstoryDeadLabel", "OverstoryConcat", "OverstoryDuplicates", "OverstoryMaterial")])
  names(flattened_data$metadata$PointIntercept_VegSpecies$fields)[names(flattened_data$metadata$PointIntercept_VegSpecies$fields) == "Overstory"] <- "Species"

  names(flattened_data$metadata$PointIntercept_VegSpecies$fields) <- stringr::str_remove(names(flattened_data$metadata$PointIntercept_VegSpecies$fields), "^Overstory")
  # Creating metadata for new variable
  flattened_data$metadata$PointIntercept_VegSpecies$fields <- append(flattened_data$metadata$PointIntercept_VegSpecies$fields, flattened_data$metadata$PointIntercept_VegSpecies$fields[2])
  names(flattened_data$metadata$PointIntercept_VegSpecies$fields)[length(flattened_data$metadata$PointIntercept_VegSpecies$fields)] <- "Layer"
  flattened_data$metadata$PointIntercept_VegSpecies$fields$Layer$description <- "Layer vegetation species came from"


  flattened_data$metadata$Frequency_Crust <- list(table_name = "Frequency_Crust",
                                                            table_description = "Frequency Crust Table",
                                                            fields = flattened_data$metadata$Frequency$fields,
                                                            table_id = 1)
  flattened_data$metadata$Frequency_Crust$fields <- append(flattened_data$metadata$Frequency_Crust$fields, flattened_data$metadata$Frequency_Quadrats$fields[c("Meter", "MeterNote", "InvCount", "InvasiveJoin", "SpeciesJoin", "Disturbance","DisturbancePhotoName", "Crust", "CrustPhotoName", "QuadratNotes")])

  invisible(flattened_data)
}

#' Write IU data to CSV
#'
#' @inheritParams fetchagol::writeToFiles
#'
#' @export
#'
writeIU <- function(all_data, data_dir = here::here("data", "final"), dictionary_dir = here::here("data", "dictionary"),
                       dictionary_filenames = c(tables = "data_dictionary_tables.txt",
                                                attributes = "data_dictionary_attributes.txt",
                                                categories = "data_dictionary_categories.txt"),
                       verbose = FALSE, removeColumns = TRUE, cols_to_remove = c("Editor", "Creator"), ...)
{
  fetchagol::writeToFiles(all_data = all_data, data_dir = data_dir, dictionary_dir = dictionary_dir, lookup_dir = NA, verbose = verbose, removeColumns = TRUE, cols_to_remove = c("Editor", "Creator"))
}
