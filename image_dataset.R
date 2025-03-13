library(tidyverse)
library(data.table)
library(openxlsx)

# First fixing the datasets, want the text column from the original full data to the smaller dataset. 
# Also want the columns with image URLs

extra_columns <- c("text", "id", "url.x", "url.y", "url.x.x", "url.y.y")

covid_new <- covid |>
  left_join(select(covid_full, all_of(extra_columns)), by = "id")

# Code for combining images with the larger dataset
# Will first need to combine all the URL_columns into a single column
covid_long <- covid_new|>
  pivot_longer(
    cols = c(url.x, url.y, url.x.x, url.y.y),
    names_to = "url_column",
    values_to = "image_url"
  )

covid_long <- covid_long |>
  mutate(image_filename = basename(image_url))

# Path to the folder containing images
images_folder <- "D:/Images/Categorized_images/Total - Copy"

# Check if the files exist
covid_long <- covid_long |>
  mutate(image_exists = file.exists(file.path(images_folder, image_filename)))

covid_long |>
  ungroup() |>
  count(image_exists)

covid_images <- covid_long |>
  filter(image_exists == TRUE)

covid_images |>
  ungroup() |>
  count(id)

# Some tweets have multiple images, and some images are shared multiple times
# So there's 782 unique images, and 853 unique tweets

saveRDS(covid_images, "E:/Data/Datasets/image_dataset/covid_images.RDS")

###############################################################################
# Also extracting the tweets with mentions of the people of interest:

# "Anders tegnell", "Anthony Fauci", "Bent Høie", "Bill Gates", "Camilla Stoltenberg", 
# "Donald Trump", "Erna Solberg", "Espen Nakstad", "Ingvild Kjerkol", "Joe Biden",
# "Jonas Gahr Støre", 

# Twitter usernames:
# Tegnell not on Twitter
# Fauci: @Dr_AnthonyFauci
# Høie: @BentHHoyre 
# Gates: @BillGates
# Stoltenberg: @camisto
# Trump: @realDonaldTrump
# Solbrg: @erna_solberg
# Nakstad: @EspenNakstad
# Kjerkol: @ingvildkjerkol
# Biden: @JoeBiden 
# Støre: @jonasgahrstore
# Høie, Stotlenberg and Støre have usernames that wouldn't have been picked up on simply a string match based on names.

people <- list(
  "Tegnell" = "Tegnell",
  "Fauci" = c("Fauci", "@Dr_AnthonyFauci"),
  "Høie" = c("Høie", "@BentHHoyre"),
  "Gates" = c("Gates", "@BillGates"),
  "Stoltenberg" = c("Stoltenberg", "@camisto"),
  "Trump" = c("Trump", "@realDonaldTrump"),
  "Solberg" = c("Solberg", "@erna_solberg"),
  "Nakstad" = c("Nakstad", "@EspenNakstad"),
  "Kjerkol" = c("Kjerkol", "@ingvildkjerkol"),
  "Biden" = c("Biden", "@JoeBiden"),
  "Støre" = c("Støre", "@jonasgahrstore")
  )

people_1 <- c("Tegnell", "Fauci", "Høie", "Gates", "Stoltenberg", "Trump", 
            "Solberg", "Nakstad", "Kjerkol", "Biden", "Støre", "tegnell", 
            "fauci", "høie", "gates", "stoltenberg", "trump", 
            "solberg", "nakstad", "kjerkol", "biden", "støre", "@Dr_AnthonyFauci",
            "@BentHHoyre", "@BillGates", "@camisto", "@realDonaldTrump", 
            "@erna_solberg", "@EspenNakstad", "@ingvildkjerkol", "@JoeBiden", "@jonasgahrstore")

covid_person <- covid_new |>
  filter(
    str_detect(text, paste(people_1, collapse = "|"))
    )
# 20 761

# Function to count unique mentions per person
count_mentions <- function(texts, people_list) {
  mention_counts <- sapply(names(people_list), function(person) {
    pattern <- str_c("(?i)(?<!\\w)(", str_c(people_list[[person]], collapse = "|"), ")(?!\\w)")
    sum(str_detect(texts, pattern))  # Count texts that mention the person at least once
    })
  
  return(data.frame(Person = names(mention_counts), Count = mention_counts))
  }

# Apply function
mention_counts_df <- count_mentions(covid_new$text, people)

sum(mention_counts_df$Count)
# 22 407, so there's some tweets mentioning multiple people

# Lastly, I want to combine this dataset, with any tweet from "covid_images" that's not already in "covid_person"
covid_person_long <- covid_person|>
  pivot_longer(
    cols = c(url.x, url.y, url.x.x, url.y.y),
    names_to = "url_column",
    values_to = "image_url"
  ) |>
  select(-url_column)

covid_person_long_cleaned <- covid_person_long |>
  group_by(across(-image_url)) |>  
  mutate(has_unique_image = sum(!is.na(image_url)) > 0) |> 
  filter(ifelse(has_unique_image, !is.na(image_url), row_number() == 1)) |> 
  ungroup() |>
  select(-has_unique_image)
# Only tweets with textual references, on if tweet contains multiple images, that observation is duplicated with an unique image url

covid_references <- covid_person_long_cleaned |>
  mutate(image_filename = basename(image_url))

covid_references_agg <-  covid_references |>
  bind_rows(anti_join(covid_images, covid_references, by = "image_filename"))

covid_references_agg |>
  count(id)
# in total 21 318 unique tweets with a reference either in the image, or in the text. 
# Since some tweets contain multiple images, there's a total of 21 611 observations in the dataset

# Adding a column identifying if the image is of a politician or not
covid_references_agg <- covid_references_agg |>
  mutate(authority_image = ifelse(is.na(image_filename), NA, image_filename %in% covid_images$image_filename))

covid_references_agg |>
  count(authority_image)

# saving the dataset
saveRDS(covid_person, "E:/Data/Datasets/image_dataset/covid_references_text_and_image.RDS")


covid_image_person <- covid_person |>
  inner_join(covid_images, by = "id")

# 312 tweets referencing the people through both text AND image
saveRDS(covid_image_person, "E:/Data/Datasets/image_dataset/covid_image_person.RDS")

###############################################################################
# also checking what photos are not matched to the tweets

image_filenames_from_url <- basename(covid_images$image_url)

# Get all image filenames from the "Total" folder
image_files_in_total <- list.files("D:/Images/Categorized_images/Total", full.names = FALSE)

# Find images in "Total" that are NOT in the dataset
unmatched_images <- setdiff(image_files_in_total, image_filenames_from_url)

# Save the unmatched photos in separate folder
total_folder <- "D:/Images/Categorized_images/Total"
unmatched_folder <- "D:/Images/Categorized_images/Total/Unmatched"

file.copy(from = file.path(total_folder, unmatched_images), 
          to = file.path(unmatched_folder, unmatched_images), 
          overwrite = TRUE)

###############################################################################
# Also want to find how many of the tweets with only textual references have images embedded in the tweet?
# Meaning, how many non-politician images are there?

# remove first any tweet without an image, and then remove the images of identified politicians
covid_images_all <- covid_person_long |>
  drop_na(image_url)
# 1608

covid_images_all <- covid_images_all |>
  anti_join(covid_images, by = "image_url")
# 1296

covid_images_all |>
  ungroup() |>
  count(image_filename, sort = TRUE) 
# there's additionally 1271 unique images in 1067 unique tweets 


###############################################################################
# Now to extract a sample for manually classifying the data. 
# Since only a minority of the observations contain images, every tweet with an image is included
# for manual labeling. In addition, a random sample of text-only tweets are extracted. 

covid_references_sample_images <- covid_references_agg |>
  select(c(text, id, conversation_id, created_at, image_url, image_filename)) |>
  ungroup() |>
  drop_na(image_url)
# 2182

covid_references_sample_images <- covid_references_sample_images |>
  group_by(id) |>
  mutate(image_instance = row_number()) |>
  ungroup()

covid_references_sample_images <- covid_references_sample_images |>
  pivot_wider(
    names_from = image_instance,
    values_from = c(image_url, image_filename),
    names_prefix = "image_")
# 1889

covid_references_sample_text <- covid_references_agg |>
  select(c(text, id, conversation_id, created_at, image_url, image_filename)) |>
  ungroup() |>
  filter(is.na(image_url))


###############################################################################
# Extract every image/text-pair in the data

covid_full <- readRDS("E:/Data/Datasets/covid_norwegian_media.rds")
covid <- readRDS("E:/Data/Datasets/CADS_datasets/twitter_data.RDS")

# Need to add some extra columns to the dataset, want the ones with info about images
extra_columns <- c("id", "url.x", "url.y", "url.x.x", "url.y.y")

covid_new <- covid |>
  left_join(select(covid_full, all_of(extra_columns)), by = "id")

# Only keeping tweets with an image
covid_images <- covid_new |>
  drop_na(url.x)
# 32 242 tweets

# Since the images were collected some time after posting, some of the image URL's weren't valid at the time of collecting
# Want to also remove tweets with no collected image

covid_images_long <- covid_images|>
  pivot_longer(
    cols = c(url.x, url.y, url.x.x, url.y.y),
    names_to = "url_column",
    values_to = "image_url"
  )

covid_images_long <- covid_images_long |>
  mutate(image_filename = basename(image_url))

# Path to the folder containing images
images_folder <- "D:/Images/Covid/images_1"

# Check if the files exist
covid_images_long <- covid_images_long |>
  mutate(image_exists_1 = file.exists(file.path(images_folder, image_filename)))

# second_path
images_folder_2 <- "D:/Images/Covid/images_2"

# Check if the files exist, and remove tweets without a collected image
covid_images_long <- covid_images_long |>
  mutate(image_exists_2 = file.exists(file.path(images_folder_2, image_filename)))

covid_images_long <- covid_images_long |>
  mutate(image_exists = image_exists_1 | image_exists_2) |>
  filter(image_exists == TRUE)

# attach the images to the dataframe
image_folders <- c("D:/Images/Covid/images_1", "D:/Images/Covid/images_2")

get_image_path <- function(filename) {
  file_paths <- file.path(image_folders, filename)
  existing_file <- file_paths[file.exists(file_paths)]
  if (length(existing_file) > 0) {
    return(existing_file[1])  # Return the first found path
    } else {
    return(NA)  # Return NA if file not found
    }
}

covid_images_long_images <- covid_images_long |>
  mutate(across(starts_with("image_filename"), ~ map_chr(.x, get_image_path), .names = "image_path_{.col}"))

# Create a new workbook
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")

# Write text data (excluding raw image data)
writeData(wb, "Sheet1", covid_images_long_images |>
            select(-starts_with("image_data_")), startCol = 1, startRow = 1)

# Insert images into Excel
for (i in 1:nrow(covid_images_long_images)) {
  for (col in grep("^image_path_", names(covid_images_long_images), value = TRUE)) {
    image_path <- covid_images_long_images[[col]][i]
    
    if (!is.na(image_path) && file.exists(image_path)) {
      col_number <- which(names(covid_images_long_images) == col)  # Get column index
      insertImage(wb, "Sheet1", image_path, startRow = i + 1, startCol = col_number, width = 2, height = 2, units = "cm")
    }
  }
}

# Save workbook
saveWorkbook(wb, "E:/df_with_images.xlsx", overwrite = TRUE)

glimpse(covid_images_long_images)
# converts to wide format, every row is a single tweet
covid_images_wide <- covid_images_long_images |>
  select(-c(url_column, image_exists_1, image_exists_2)) |>
  group_by(id) |>
  mutate(image_instance = row_number()) |>
  ungroup() |>
  pivot_wider(
    names_from = image_instance,
    values_from = c(image_url, image_filename, image_path_image_filename),
    names_prefix = "image_")
# 30 085

# Saving this dataset
saveRDS(covid_images_wide, "E:/Data/Datasets/Poli_reco_datasets/image_text.RDS")

# remove RTs and remove usernames
covid_images_no_rt <- covid_images_wide |>
  filter(!str_detect(tweet, "^RT"))
# 27 210

removeUsernames <- function(tweet) {
  return(gsub("@[a-z,A-Z,_]*[0-9]*[a-z,A-Z,_]*[0-9]*", "USERNAME", tweet))
}

removeURL <- function(tweet) {
  return(gsub("http\\S+", "", tweet))
}
covid_images_no_rt$tweet <- apply(covid_images_no_rt["tweet"], 1, removeURL)
covid_images_no_rt$tweet <- apply(covid_images_no_rt["tweet"], 1, removeUsernames)

# saveRDS(covid_images_no_rt, "E:/Data/Datasets/Poli_reco_datasets/image_text_ano_nort.RDS")
# covid_images_no_rt <- readRDS("E:/Data/Datasets/Poli_reco_datasets/image_text_ano_nort.RDS")

# doing a small random sample, for some preliminary testing with manual labeling
covid_sample <- covid_images_no_rt |>
  sample_n(400) |>
  select(-c(author_hash, in_reply_to_user_hash, retweet_count, 
            reply_count, like_count, quote_count, impression_count, created_at,
            unnest_referenced_tweets_id, label, date_num, image_exists, conversation_id,
            url_1, url_2, url_3, url_4, url_5, url_6, url_7, url_8, 
            image_url_image_1, image_url_image_2, image_url_image_3, image_url_image_4,
            image_filename_image_1, image_filename_image_2, image_filename_image_3, 
            image_filename_image_4))

# Create a new workbook
wb_1 <- createWorkbook()
addWorksheet(wb_1, "Sheet1")

# Write text data (excluding raw image data)
writeData(wb_1, "Sheet1", covid_sample |>
            select(-starts_with("image_data_")), startCol = 1, startRow = 1)

# Insert images into Excel
for (i in 1:nrow(covid_sample)) {
  for (col in grep("^image_path_", names(covid_sample), value = TRUE)) {
    image_path <- covid_sample[[col]][i]
    
    if (!is.na(image_path) && file.exists(image_path)) {
      col_number <- which(names(covid_sample) == col)  # Get column index
      insertImage(wb_1, "Sheet1", image_path, startRow = i + 1, startCol = col_number, width = 4, height = 4, units = "cm")
    }
  }
}

# Save workbook
saveWorkbook(wb_1, "E:/sample_images_2.xlsx", overwrite = TRUE)
saveRDS(covid_sample, "E:/sample_images_2.RDS")
