# This R script scrapes, cleans, and compiles parliamentary election data ready for further analysis. 
# Source: https://opendatalab.mn, covering the years 1992–2024.
#
# Definition:
    # The dataset includes electoral district names and numbers, registered voters,
    #   valid votes, turnout percentages, candidate names, party affiliations, and 
    #   vote counts recorded in Mongolia’s parliamentary elections between 1992 and 2024. 
    #   The final output merges all election years into a single, structured dataset 
    #   suitable for analysis and visualizations.

# About OpenDataLab.mn:
    # OPENDATA LAB MONGOLIA 2.0 is a public data platform established in 2019 to 
    #   promote citizens’ right to access information and oversee government activities, 
    #   ensuring transparency and accountability under Mongolia’s Constitution and 
    #   the 2022 Law on Public Information Transparency.

# Author: Itgelt Bat-Ochir, MPA 
# Affiliation: Central European University, Department of Public Policy 
# Created: 
    # February of 2024, this script was developed as part of the final assignment for the course 
    #   “Big Data for Public Policy,” led by Mihály Fazekas at the Central European University.

# The script consists of three parts:
  # Part 1 scrapes election data results of years between 1992 and 2016. 
  # Part 2 scrapes election data results of years 2020 and 2024 as the latest 
    # data has been entered in a different format than of previous years. Also an 
    # important note for researches who uses the dataset be cautious years 1992-2016 
    # only has data for those who were elected while 2020 and 2024 has all the candidate
    # data who participated in the elections.  
  # Part three combines the two dataset and saves the output as CSV. 

#rm(list=ls())

#   Load required libraries  
library(rvest)
library(dplyr)
library(xml2)
library(stringr)
library(tibble)
library(purrr)

options(scipen = 999) 

# Part 1 - Scrap years between 1992 and 2016 

# Function defined below is designed to extract data in desired format
extraction.function <- function(url) {
  content <- read_html(url)
  # first part 
  parl_election_div <- content %>% html_nodes(".parl-election")
  if (length(parl_election_div) == 0) {
    stop("No data found on the webpage!!!")
  }
  # Extract the rows (as district results) within the parl-election divs
  rows <- parl_election_div %>% html_nodes("div.row")
  # Extract data from each row and create a tibble for district results
  parl_data <- lapply(rows, function(row) {
    cols <- row %>% html_nodes("div") %>% html_text(trim = TRUE)
    return(cols)
  })
  # Creating a data frame using do.call, rbind consisting of the parl-election data
  parl_df <- as.data.frame(do.call(rbind, parl_data), stringsAsFactors = FALSE)
  # Extract column names from the first row and remove leading numbers, as well as the first row itself
  colnames(parl_df) <- gsub("^\\d+\\.", "", parl_df[1, ])
  parl_df <- parl_df[-1, ]
  # Initialize an empty list to store results
  results_list <- list()
  # Iterate over rows and extract data for each observation
  for (row in rows) {
    # Extract data from each row and create a tibble for each observation
    cols <- row %>% html_nodes("div") %>% html_text(trim = TRUE)
    
    # Additional code for extracting year and second element
    data <- content %>% html_nodes("h3, h5") %>% html_text()
    year <- str_extract(data[1], "\\d+")
    second_element <- data[2]
    
    # Splitting the second element using the comma as a separator
    districts_info <- strsplit(second_element, ": ")[[1]]
    # Removing unnecessary strings
    districts_info[2] <- gsub("Бүртгэлтэй", "", districts_info[2])
    districts_info[3] <- gsub(", Хүчинтэй", "", districts_info[3])
    # Splitting districts_info[3] between the number and percentage
    split_percentage <- strsplit(gsub("\\s", "", districts_info[4]), "\\(|\\)")
    # Extracting the values
    voted <- split_percentage[[1]][1]
    percentage <- split_percentage[[1]][2]
    
    # Creating a tibble with the desired columns
    result <- tibble(
      Он = year,
      Т.Нэр = districts_info[2],
      Т.Дугаар = districts_info[1],
      Бүртгэлтэй = as.numeric(districts_info[3]),
      Хүчинтэй = voted,
      `Хүчинтэй хувиар` = percentage,
      parl_df
    )
    
    # save it the result to the list
    results_list <- c(results_list, list(result))
  }
  return(results_list)
}

# Loop that enters each years then districts to extract the parl-election rows and needed h3 h5 content with the function defined above

# Base URL 
base_url <- "https://opendatalab.mn/election/"

# Initialize a list to store the data frames
all_data <- list()
url <- character()

# Loop from 1992 to 2016.
for (i in seq(1992, 2016, by = 4)) {
  
  # Construct the URL
  date_url <- paste(base_url, i, "/eparliament", sep = "")
  
  # Store the URL 
  url[i] <- date_url
  # Print the current URL
  print(paste("Processing:", url[i]))
  
  # Read the HTML content from the URL
  page <- read_html(url[i])
  # Extract district attributes, fortunately very consistent on the webpage through out the years
  district_links <- page %>% html_nodes("#page main div div div.row div a") %>%
    html_attr("href") %>% xml2::url_absolute("https://opendatalab.mn/")
  
  # Initialize a list to store data frames for each district
  district_data_list <- list()
  # Iterate over district links and apply the extraction function
  for (district_url in district_links) {
    tryCatch({
      data_df <- extraction.function(district_url)
      # Store the district data frame in the list
      district_data_list[[district_url]] <- data_df
      print(paste("Scraped:", district_url))
    }, error = function(e) {
      cat("Error extracting data from:", district_url, "\n")
    })
    Sys.sleep(1)
  }
  # Store the district data list in the overall list with the date_url as a key
  all_data[[date_url]] <- district_data_list
}

# Check the data  
View(all_data)

# Combining and creating the final data 
# An empty list to store data frames
combined_data_list <- list()

# Iterate over each element in all_data
for (date_url in names(all_data)) {
  # Extract the district data list for the current date_url
  district_data_list <- all_data[[date_url]]
  # Check if all data frames in the district_data_list have the same number of columns
  if (length(unique(sapply(district_data_list, ncol))) == 1) {
    # If the number of columns is the same, bind_rows
    combined_data_list[[date_url]] <- bind_rows(district_data_list)
  } else {
    # If the number of columns is not the same, print a warning or handle it as needed
    warning("Columns mismatch for:", date_url)
  }
}

# Combine all_data into a single data frame
combined_data <- bind_rows(combined_data_list)
view(combined_data)

# Removing the repeated entries 
final_data <- distinct(combined_data)
view(final_data)


# Part 2 - Scrap years 2020 and 2024
years <- c(2020, 2024)

# Initialize an empty list to store data frames for each district (both years)
district_data_frames <- list()

for (yr in years) {
  date_url <- paste0("https://opendatalab.mn/election/", yr, "/eparliament")
  print(paste("Processing:", date_url))
  
  ur <- read_html(date_url)
  Sys.sleep(0.5)  # pause between year page requests
  
  links <- ur %>%
    html_nodes("#page > main a") %>%
    html_attr("href")
  
  base_url <- "https://opendatalab.mn"
  full_links <- paste0(base_url, links)
  
  for (link in full_links) {
    district_url <- link
    tryCatch({
      # Read the HTML content from the current link
      content <- read_html(district_url)
      Sys.sleep(0.7)  # pause between district page requests
      
      # Extract general information like year, district number, etc.
      data <- content %>% html_nodes("h3, h5") %>% html_text()
      year <- str_extract(data[1], "\\d+")
      districtnum <- str_extract(data[2], "\\d+")
      districts_info <- strsplit(data[2], ": ")[[1]]
      districts_info[2] <- gsub("Бүртгэлтэй", "", districts_info[2])
      districts_info[3] <- gsub(", Хүчинтэй", "", districts_info[3])
      split_percentage <- strsplit(gsub("\\s", "", districts_info[4]), "\\(|\\)")
      voted <- split_percentage[[1]][1]
      percentage <- split_percentage[[1]][2]
      
      # Scrape specific data rows from the current page
      rows <- content %>% html_nodes(".card-body .row")
      
      # Extract texts from each row
      extracted_texts <- list()
      for (row in rows) {
        texts <- row %>% 
          html_nodes(".col-1, .col-3") %>% 
          head(4) %>% 
          html_text(trim = TRUE)
        extracted_texts <- c(extracted_texts, list(texts))
      }
      
      # Convert extracted texts into a data frame for the current district
      df <- do.call(rbind, lapply(extracted_texts, function(x) {
        data.frame(
          Он = year,
          Т.Нэр = districts_info[2], 
          Т.Дугаар = districtnum,
          Бүртгэлтэй = districts_info[3],
          Хүчинтэй = voted,
          `Хүчинтэй хувиар` = percentage,
          Дугаар = as.numeric(x[1]),
          `Нэр дэвшигч` = x[2],
          Нам = x[3],
          `Санал тоо` = as.numeric(x[4])
        )
      }))
      
      district_data_frames <- c(district_data_frames, list(df))
      print(paste("Scraped:", district_url))
      
    }, error = function(e) {
      cat("Error extracting data from:", district_url, "\n")
    })
  }
}

# Combine all district data frames into one (both 2020 and 2024)
final_data_2020_2024 <- do.call(rbind, district_data_frames)

final_data_2020_2024 <- final_data_2020_2024 %>%
  filter(!is.na(Дугаар)) %>% distinct()

view(final_data_2020_2024)

# Part 3 - Combine all years into one dataframe 
#  1) Fix 2020/2024 column names to match 1992–2016 
names(final_data_2020_2024)[names(final_data_2020_2024) == "Нэр.дэвшигч"]     <- "Нэр дэвшигч"
names(final_data_2020_2024)[names(final_data_2020_2024) == "Санал.тоо"]       <- "Санал тоо"
names(final_data_2020_2024)[names(final_data_2020_2024) == "Хүчинтэй.хувиар"] <- "Хүчинтэй хувиар"

# Column order
final_cols <- c("Он","Т.Нэр","Т.Дугаар","Бүртгэлтэй","Хүчинтэй","Хүчинтэй хувиар","Дугаар","Нэр дэвшигч","Нам","Санал тоо")

#  2) Coerce types for 1992–2016 
final_data_std <- final_data %>%
  mutate(
    Он               = as.character(Он),
    `Т.Нэр`          = as.character(`Т.Нэр`),
    `Т.Дугаар`       = as.character(`Т.Дугаар`),
    Бүртгэлтэй       = as.numeric(Бүртгэлтэй),
    Хүчинтэй         = as.character(Хүчинтэй),
    `Хүчинтэй хувиар`= as.character(`Хүчинтэй хувиар`),
    Дугаар           = as.numeric(Дугаар),
    `Нэр дэвшигч`    = as.character(`Нэр дэвшигч`),
    Нам              = as.character(Нам),
    `Санал тоо`      = as.numeric(`Санал тоо`)
  ) %>%
  select(all_of(final_cols))

#  3) Coerce types for 2020–2024 
final_data_2020_2024_std <- final_data_2020_2024 %>%
  mutate(
    Он               = as.character(Он),
    `Т.Нэр`          = as.character(`Т.Нэр`),
    `Т.Дугаар`       = as.character(`Т.Дугаар`),
    Бүртгэлтэй       = as.numeric(Бүртгэлтэй),
    Хүчинтэй         = as.character(Хүчинтэй),
    `Хүчинтэй хувиар`= as.character(`Хүчинтэй хувиар`),
    Дугаар           = as.numeric(Дугаар),
    `Нэр дэвшигч`    = as.character(`Нэр дэвшигч`),
    Нам              = as.character(Нам),
    `Санал тоо`      = as.numeric(`Санал тоо`)
  ) %>%
  select(all_of(final_cols))

# Combine datasets
final_data_1992_2024 <- rbind(final_data_std, final_data_2020_2024_std)
str(final_data_1992_2024)







