# Test setup:
data.dir <- here::here("testdata")
refData <- here::here("testref/ATPU_GPS_Gull_referenceData_03Sep2024.csv")
out.dir <- here::here("testout")
spcd <- "ATPU"
site <- "GullIs"


# Function

formatPathtrack <- function(data.dir, refData, outliers00 = T, out.dir = NULL, spcd = NULL, site = NULL) {

  # Create a table of the '.pos' files and tag IDs

  files <- data.frame(filepath = list.files(path = data.dir, pattern = "*.pos", recursive = T)) |>
    dplyr::mutate(tag = stringr::str_extract(filepath, "(?<=Tag)[^/]+"))

  ### Import raw files, add column for tag name

  datalist <- list()

  for(i in files$filepath){

    tag <- dplyr::filter(files, filepath == i)
    tag.file <- read.csv(paste(data.dir, as.character(tag$filepath), sep = "/"),
                         header = F, skip = 5) |>
      dplyr::mutate(tag = tag$tag,
                    filename = basename(i))
    datalist[[i]] <- tag.file

    print(i)
  }

  # Combine

  pos <- do.call(rbind, datalist)

  # Remove rownames

  rownames(pos) <- NULL

  # Pathtrack standard column names (not needed, ease of use)

  colnames(pos) <- c("day", "month", "year", "hour", "minute", "second",
                    "secondOfDay", "satellites", "lat", "long", "altitude",
                    "clockOffset", "accuracy", "battery", "procParam1", "procParam2",
                    "tag", "filename")

  # Rename columns and format for Movebank

  pos <- pos |>
    dplyr::mutate(`sensor-type` = "GPS",
         timestamp = lubridate::dmy_hms(paste(paste(sprintf("%02d", day), sprintf("%02d", month), year, sep = "/"), paste(sprintf("%02d", hour), sprintf("%02d", minute), sprintf("%02d", second), sep = ":"), sep = " "), tz = 'UTC'),
         `gps-fix-type` = "3D",
         `gps:satellite-count` = satellites,
         `height-above-ellipsoid` = altitude, # meters
         `location-lat` = lat,
         `location-long` = long,
         `tag-voltage` = battery*1000, # mV not volts
         `tag-id` = tag,
         `filename-pos` = filename)


  # Join to Movebank reference data to get animal-id, animal-taxon, deployment-id
  # Unwise to join by tag-id - require filename in Reference data!

  # Read reference data and fix field names
  ref <- read.csv(refData)
  names(ref) <- gsub("\\.", "-", names(ref))

  ref <- dplyr::select(ref, c(`filename-pos`, `animal-id`, `deployment-id`))

  pos <- dplyr::left_join(pos, ref, by = "filename-pos") |>
    dplyr::select('sensor-type', 'tag-id', 'animal-id', 'deployment-id',
                  timestamp, 'location-long', 'location-lat',
                  'gps-fix-type', 'height-above-ellipsoid',
                  'gps:satellite-count', 'tag-voltage',
                  'filename-pos')

  # Label 0,0 coordinates as outliers, if indicated
  if(outliers00 == T){

    pos <- pos |>
      dplyr::mutate(`import-marked-outlier` = ifelse(`location-long` == 0 & `location-lat` == 0, TRUE, FALSE),
                    `import-marked-outlier-comment` = ifelse(`import-marked-outlier` == TRUE & `gps:satellite-count` == 0, "Pathtrack 0,0 coordinate, zero satellites",
                                                             ifelse(`import-marked-outlier` == TRUE & `gps:satellite-count` > 0, "Pathtrack 0,0 coordinate, non-zero satellites", NA)))

  }

  # save out if required info is provided

  if(!is.null(out.dir) & !is.null(spcd) & !is.null(site)) {

    write.csv(pos, paste0(out.dir, "/posData_", spcd, "_GPS_", site, ".csv"), row.names = F)

  }

  return(pos)

}

