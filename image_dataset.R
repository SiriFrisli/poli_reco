library(tidyverse)

# Code for combining images with the larger dataset
# Will first need to combine all the URL_columns into a single column
covid_long <- covid_full|>
  pivot_longer(
    cols = c(url.x, url.y, url.x.x, url.y.y),
    names_to = "url_column",
    values_to = "image_url"
  )

covid_long <- covid_long |>

covid_long <- covid_long |>
  mutate(image_filename = basename(image_url))

# Path to the folder containing images
images_folder <- "D:/Images/Covid/output_images_detected"

# Check if the files exist
covid_long <- covid_long |>
  mutate(image_exists = file.exists(file.path(images_folder, image_filename)))

covid_long |>
  count(image_exists)

covid_images <- covid_long |>
  filter(image_exists == TRUE)

covid_images <- covid_images |>
  select(-c(referenced_tweets, edit_history_tweet_ids, possibly_sensitive, lang, 
            reply_settings, mentions, cashtags, annotations, place_id, coordinates,
            copyright, country_codes, scope, cld2, cld3, media_keys))
# 15 834 tweets with images of people. This is with retweets. 

# saveRDS(covid_images, "E:/Data/Datasets/image_dataset/covid_images.RDS")

