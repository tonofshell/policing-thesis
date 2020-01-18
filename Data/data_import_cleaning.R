#### Importing and Cleaning Data ####

### Setup ###
library(tidyverse)
library(lubridate)
library(stringr)
library(here)
library(jsonlite)
library(ggmap)
library(RSocrata)

source(here("api_keys.R"))

### Functions ###

process_date_range = Vectorize(function(d_range, start = TRUE) {
  intervals = str_split(d_range, "to") %>% unlist() %>% str_trim()
  if (length(intervals) == 2) {
    beginning = intervals[1] %>% str_squish() %>% str_split(" ") %>% unlist()
    if (!start & !str_detect(intervals[2], "/")) {
      return(paste(beginning[1], intervals[2], sep = " "))
    } else if (!start) {
      return(intervals[2])
    }
  }
  if (length(intervals) == 3) {
    middle = intervals[2] %>% str_squish() %>% str_split(" ") %>% unlist()
    if (start) {
      return(paste(intervals[1], middle[2], middle[3], sep = " "))  
    }
    return(paste(middle[1], intervals[3], sep = " "))
  }
  if (start) {
    return(intervals[1])
  }
  return(NA)
})

extract_date = Vectorize(function(date_char) {
  attempt = mdy_hm(date_char)
  #message(attempt)
  if (is.na(attempt) & !is.na(date_char)) {
    if (str_detect(date_char, " ")) {
      date_char = date_char %>% str_split(" ") %>% unlist() %>% .[1]
    }
    #message(date_char)
    attempt = mdy(date_char) %>% as_datetime()
    #message(attempt)
  }
  return(attempt)
})

fix_nas = function(data_set, na_strs) {
  library(tidyverse)
  for (var in names(data_set)) {
    data_set[[var]][data_set[[var]] %in% na_strs] = NA
  }
  return(data_set)
}

# collpases n least common categories into an 'other' category
collapse_to_other = function(data_set, n_categories, ...) {
  library(tidyverse)
  variables = data_set %>% select(...) %>% names()
  for (variable in variables) {
    var_table = data_set[[variable]] %>% table() %>% as_tibble() %>% arrange(-n)
    other_cats = var_table$.[n_categories:length(var_table$.)]
    data_set[[variable]] = data_set[[variable]] %>% as.character()
    data_set[[variable]][data_set[[variable]] %in% other_cats] = "Other"
  }
  return(data_set)
} 

clean_disposition = function(disp_vector) {
  cpd_arrest = str_detect(disp_vector, "CPD") & str_detect(disp_vector, "Arrest") 
  cpd_alone = str_detect(disp_vector, "CPD") & !str_detect(disp_vector, "Arrest") 
  arrest_alone = str_detect(disp_vector, "Arrest") & !str_detect(disp_vector, "CPD") 
  disp_vector[cpd_arrest] = "CPD Arrest"
  disp_vector[cpd_alone] = "CPD Involved"
  disp_vector[arrest_alone] = "Arrest"
  closed = str_detect(disp_vector, "Closed") | str_detect(disp_vector, "closed") 
  disp_vector[closed] = "Closed"
  disp_vector[str_detect(disp_vector, "Clear")] = "Exceptionally Cleared"
  return(disp_vector)
}

get_location_type = function(x) {
  get_num_streets = Vectorize((function (v) v %>% str_split("&") %>% unlist() %>% str_split("between") %>% unlist() %>% str_split(" and") %>% unlist() %>% str_split(" at") %>% unlist() %>% str_split("/") %>% unlist() %>% str_split(" to") %>% unlist() %>% str_split(" near") %>% unlist() %>% str_squish() %>% length()))
  get_num_streets(x)
}

street_cleaner = Vectorize(function(x, return_poss = FALSE) {
  x = str_remove_all(x, "[:punct:]")
  possible_names = chicago_streets %>% filter(str_detect(full_street_name, x))
  if (nrow(possible_names) > 1 && "st" %in% possible_names$suffix) {
    possible_names = possible_names %>% filter(suffix == "st")
  }
  if (nrow(possible_names) > 1 &&  "e" %in% possible_names$direction) {
    possible_names = possible_names %>% filter(direction == "e")
  }
  if (nrow(possible_names) > 1 &&  "s" %in% possible_names$direction) {
    possible_names = possible_names %>% filter(direction == "s")
  }
  if (nrow(possible_names) > 1 &&  "ave" %in% possible_names$suffix) {
    possible_names = possible_names %>% filter(suffix == "ave")
  }
  if (nrow(possible_names) > 1 &&  " " %in% possible_names$suffix_direction) {
    possible_names = possible_names %>% filter(suffix_direction == " ")
  }
  if (nrow(possible_names) > 1) {
    warning("Two possible streets for ", x)
    if (return_poss) {
      print(possible_names)
    }
    return(NA)
  }
  if (nrow(possible_names) < 1) {
    warning("No matched streets for ", x)
    if (return_poss) {
      print(possible_names)
    }
    return(NA)
  }
})

address_cleaner = function(x) {
  house_num = str_extract(x, "^[:digit:]{2,4} ")
  if (is.na(house_num)) {
    return(NA)
  }
  street = str_remove(x, house_num) %>% street_cleaner()
  if (is.na(street)) {
    return(NA)
  }
  paste0(house_num, street)
}

get_coords = Vectorize(function(x) {
  format_geocode = function(x) {
    x %>% select(lat, lon) %>% unlist() %>% str_flatten(", ")
  }
  
  location = x %>% str_replace_all(" near", " and") %>% str_replace_all("&", " and") %>% str_replace_all(" at", " and") %>% str_replace_all("/", "between") %>% str_remove_all("[:punct:]") %>% str_split("between") %>% unlist() %>% str_squish() %>% .[. != ""]
  
  if (TRUE %in% str_detect(x, " to")) {
    location = location %>% str_split(" to") %>% unlist() %>% str_squish()
    
    if (length(location) == 4) {
      return(paste(location[1], "and", location[3], ", chicago il") %>% geocode() %>% bind_rows(paste(location[2], "and", location[4], ", chicago il") %>% geocode()) %>% summarise_all(mean) %>% format_geocode())
    }
    if (length(location) == 3) {
      return(paste(location[1], "and", location[2], ", chicago il") %>% geocode() %>% bind_rows(paste(location[1], "and", location[3], ", chicago il") %>% geocode()) %>% summarise_all(mean) %>% format_geocode())
    }
    if (TRUE %in% str_detect(location, " and")) {
      return(geocode(location[1]) %>% bind_rows(geocode(location[2])) %>% summarise_all(mean) %>% format_geocode())
    }
  } else {
    if (length(location) == 1) {
      return(location %>% paste0(", chicago il") %>% geocode() %>% format_geocode())
    }
    if (TRUE %in% str_detect(location, " and")) {
      cross_street = location[!str_detect(location, " and")]
      two_streets = location[str_detect(location, " and")] %>% str_split(" and") %>% unlist() %>% str_squish()
      
      return(paste(cross_street, "and", two_streets[1], ", chicago il") %>% geocode() %>% bind_rows(paste(cross_street, "and", two_streets[2], ", chicago il") %>% geocode()) %>% summarise_all(mean) %>% format_geocode())
    }
  }
  return(paste(location[1], ", chicago il") %>% geocode() %>% bind_rows(paste(location[2], ", chicago il") %>% geocode()) %>% summarise_all(mean) %>% format_geocode())
})

### Additional Data ###
chicago_streets = read.socrata("https://data.cityofchicago.org/resource/i6bp-fvbx.json", socrata_app_token) %>% as_tibble() %>% mutate_if(is.character, str_to_lower)

### UCPD Data ###
latest_release = read_json("https://api.github.com/repos/tonofshell/ucpd-incident-data/releases/latest")
list.files(here("Data"), pattern="ucpd_.*data_scraped_.*.csv") %>% {here("Data", .)} %>% file.remove()
for (asset in latest_release$assets) {
  download.file(asset$browser_download_url, here("Data", asset$name))
}



incident_data = list.files(here("Data"), pattern="ucpd_incident_data_scraped_*") %>% {here("Data", .)} %>% read_csv() %>% 
  fix_nas(c(":", "VOID", "Void", "void", "n/a", "N/A", "na", "NA", "No reports this date", "None")) %>% 
  mutate(Reported = Reported %>% mdy_hm(), 
         Start = Occurred %>% process_date_range() %>% extract_date() %>% as_datetime(), 
         End = Occurred %>% process_date_range(FALSE) %>% extract_date() %>% as_datetime(), 
         Disposition = Disposition %>% clean_disposition()) %>% 
  drop_na(Reported) %>% 
  collapse_to_other(10, Disposition) %>% 
  rename("Comments" = `Comments / Nature of Fire`) %>% 
  separate(Location, c("Location", "Building"), sep = "\\(") %>% 
  mutate(Location = str_to_lower(str_squish(Location)),
         Building = str_to_lower(str_remove_all(Building, "\\)")), 
         Incident = str_to_lower(Incident), 
         in_jurisdiction = str_detect(Location, "out of area", negate = TRUE) & str_detect(Location, "outside", negate = TRUE), 
         Location = str_remove_all(Location, "out of area") %>% str_remove_all("-") %>% str_squish()) %>% 
  mutate(information = str_detect(Incident, "information"), 
         assist_other_agency = str_detect(Incident, "assist other agency"),
         non_criminal = str_detect(Incident, "non-criminal") | str_detect(Incident, "non criminal") | str_detect(Incident, "non- criminal"),
         attempted = str_detect(Incident, "attempt") | str_detect(Incident, "att\\."),
         warrant = str_detect(Incident, "warrant"),
         armed = str_detect(Incident, "armed"),
         aggravated = str_detect(Incident, "aggravated") | str_detect(Incident, "agg\\."),
         coords = get_coords(Location),
         Incident = str_remove_all(Incident, "information") %>% 
           str_remove_all("assist other agency") %>% 
           str_remove_all("non-criminal") %>% str_remove_all("non criminal") %>% 
           str_remove_all("attempted") %>% 
           str_remove_all("attempt") %>% 
           str_remove_all("att\\.") %>% 
           str_remove_all("warrant") %>% 
           str_remove_all("armed") %>% 
           str_remove_all("aggravated") %>% 
           str_remove_all("agg\\.") %>% 
           str_remove_all("criminal") %>% 
           str_remove_all("arrest") %>% 
           {case_when((str_detect(., "theft") & str_detect(., "identity", negate = TRUE)) ~ "theft", 
                     str_detect(., "battery") ~ "battery", 
                     str_detect(., "assault") ~ "assault", 
                     str_detect(., "hit") & str_detect(., "run") ~ "hit and run", 
                     (str_detect(., "traffic") | str_detect(., "vehicle")) & (str_detect(., "crash") | str_detect(., "collision") | str_detect(., "accident")) ~ "traffic accident", 
                     str_detect(., "trespass") ~ "trespassing",
                     str_detect(., "robbery") ~ "robbery",
                     str_detect(., "deceptive") ~ "deceptive practice",
                     str_detect(., "stalk") ~ "stalking",
                     str_detect(., "indece") ~ "indecent exposure",
                     str_detect(., "harass") ~ "harassment",
                     str_detect(., "threat") ~ "threat",
                     str_detect(., "arson") ~ "arson",
                     str_detect(., "fire")  ~ "fire", 
                     str_detect(., "injur")  ~ "injured person", 
                     str_detect(., "home invasion")  ~ "home invasion",
                     str_detect(., "suspicious")  ~ "suspicious activity",
                     str_detect(., "dui")  ~ "driving under the influence",
                     str_detect(., "sex") & str_detect(., "assault", negate = TRUE) ~ "sex crime",
                     str_detect(., "liquor") | str_detect(., "consumption") ~ "liquor law violation", 
                     str_detect(., "damage") & str_detect(., "property") ~ "damage to property", 
                     str_detect(., "damage") & str_detect(., "vehicle") ~ "damage to vehicle", 
                     str_detect(., "traffic") & str_detect(., "accident", negate = TRUE) ~ "traffic violation", 
                     (str_detect(., "weapon") | str_detect(., "uuw") | str_detect(., "gun") | str_detect(., "ammunition")) & (str_detect(., "found", negate = TRUE) | str_detect(., "turnin", negate = TRUE)) ~ "weapon violation", 
                     TRUE ~ .)} %>%
           str_remove_all("/") %>% str_remove_all("-") %>% str_squish() %>% na_if("")
         ) %>% 
  separate(coords, c("lat", "lon"), sep = ",") %>% 
  mutate(lat = as.numeric(lat), lon = as.numeric(lon))

names(incident_data) = str_to_lower(names(incident_data))

write_csv(incident_data, here("Data", "cleaned_incident_data.csv"))
saveRDS(incident_data, here("Data", "cleaned_incident_data.rds"))

# CPD Data

# (41.845899, -87.608858)  (41.732987, -87.604720) (41.731920, -87.677545)
#approximate 4 mile radius from 57th and Ellis Ave
cpd_crimes_data = RSocrata::read.socrata("https://data.cityofchicago.org/resource/ijzp-q8t2.json?$where=year > 2009 AND latitude between '41.732987' and '41.845899' AND longitude > -87.677545", socrata_app_token) %>% select(-c(x_coordinate, y_coordinate, location.latitude, location.longitude, location.human_address)) %>% mutate(date = as_datetime(date), updated_on = as_datetime(updated_on), arrest = as.logical(arrest), domestic = as.logical(domestic), beat = factor(beat), district = factor(district), ward = factor(ward), community_area = factor(community_area), fbi_code = factor(fbi_code), year = as.numeric(year), latitude = as.numeric(latitude), longitude = as.numeric(longitude))

write_csv(cpd_crimes_data, here("Data", "cleaned_crime_data.csv"))
saveRDS(cpd_crimes_data, here("Data", "cleaned_crime_data.rds"))
