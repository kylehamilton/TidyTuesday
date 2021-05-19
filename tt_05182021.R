library(tidytuesdayR)
library(ggstatsplot)
library(dplyr)
library(labourR)

options(scipen = 999)
 
# Data
tt_output <- tt_load_gh("2021-05-18")
tt_fileName <- as.character(tt_output) 
tt_dataDownload <- tt_download(tt_output, files = tt_fileName)
tt_data <- tt_dataDownload$survey
tt_data$ID <- seq(1:nrow(tt_data))

# Job Classification by Job Title
# International Standard Classification of Occupations
# Page 65 https://www.ilo.org/wcmsp5/groups/public/---dgreports/---dcomm/---publ/documents/publication/wcms_172572.pdf

tt_ISCO1 <- classify_occupation(
  tt_data,
  id_col = "ID",
  text_col = "job_title",
  lang = "en",
  num_leaves = 10,
  isco_level = 1
)

tt_data <- merge(x = tt_data, y = tt_ISCO1, by = "ID")

names(tt_data)[names(tt_data) == "iscoGroup"] <- "iscoGroup1"
names(tt_data)[names(tt_data) == "preferredLabel"] <- "preferredLabel1"

tt_ISCO2 <- classify_occupation(
  tt_data,
  id_col = "ID",
  text_col = "job_title",
  lang = "en",
  num_leaves = 10,
  isco_level = 2
)


tt_data <- merge(x = tt_data, y = tt_ISCO2, by = "ID")

names(tt_data)[names(tt_data) == "iscoGroup"] <- "iscoGroup2"
names(tt_data)[names(tt_data) == "preferredLabel"] <- "preferredLabel2"


tt_ISCO3 <- classify_occupation(
  tt_data,
  id_col = "ID",
  text_col = "job_title",
  lang = "en",
  num_leaves = 10,
  isco_level = 3
)

tt_data <- merge(x = tt_data, y = tt_ISCO3, by = "ID")

names(tt_data)[names(tt_data) == "iscoGroup"] <- "iscoGroup3"
names(tt_data)[names(tt_data) == "preferredLabel"] <- "preferredLabel3"


tt_data_ISCO = tt_data %>%
  filter(iscoGroup2 == 23) %>% # Only include Teaching Professionals
  filter(between(iscoGroup3, 231, 235)) %>% # Only include these types of Teaching Professionals
  filter(iscoGroup3 != 234) %>% # Remove primary school teachers (n=4)
  filter(annual_salary < 250001) # Remove high earners
  

ggbetweenstats(
  data = tt_data_ISCO,
  x = preferredLabel3,
  y = annual_salary,
  pairwise.comparisons = TRUE,
  bf.message = TRUE,
  title = "Teaching Professionals and Compensation",
  xlab = "Type of Teacher",
  ylab = "Annual Compensation",
  ggtheme = ggplot2::theme_bw()
)

