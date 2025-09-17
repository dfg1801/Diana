#########Specimen query cleaning

## Load packages
library(tidyverse)
library(here)
library(janitor)

## Load in data
data <- read_csv(here("data", "occurrence.csv")) # gathered honeycreeper info

### Remove non-honeycreepers
clean <- data %>%
  clean_names() %>%
  filter(!str_detect(dwc_scientific_name, pattern = "^carpodacus")) %>% # remove house finches, only other fringilid in HI
  remove_empty(c("rows", "cols"))  %>%
  mutate(dwc_recorded_by = str_remove_all(dwc_recorded_by, "collector\\(s\\)\\: ")) %>%
  mutate(dwc_recorded_by = str_replace_all(dwc_recorded_by, "henry c. palmer", "palmer, h c")) %>% # cleaning names lol
  mutate(dwc_recorded_by = str_replace_all(dwc_recorded_by, "palmer, henry c.", "palmer, h c")) %>% 
  mutate(dwc_recorded_by = str_replace_all(dwc_recorded_by, "h w henshaw", "henshaw, h w")) %>%
  mutate(dwc_recorded_by = str_replace_all(dwc_recorded_by, "henry wetherbee henshaw", "henshaw, h w")) %>%
  mutate(dwc_recorded_by = str_replace_all(dwc_recorded_by, "paul h. baldwin", "baldwin, p h")) %>%
  mutate(mutate(across(everything(), 
                       ~ str_remove_all(string = .x, pattern = "NA")))) %>% ## turn all "NA"s into true NAs
  mutate(year = as.character(idigbio_event_date)) %>% # clean up date, only want year
  mutate(across(year,  
                ~ ifelse(is.na(year), dwc_event_date, .))) %>%
  mutate(across(year,  
                ~ ifelse(is.na(year), dwc_verbatim_event_date, .))) %>%
  mutate(year = str_remove_all(year, "--")) %>% # remove all the ones with no date 
  mutate(year = str_remove_all(year, "\\[no verbatim date data\\]")) %>%
  mutate(year = str_remove_all(year, "\\?")) %>%
  mutate(year = str_remove_all(year, "00 xxx 0000")) %>%
  mutate(year = str_extract(year, "\\d{4}")) # make all the dates just year
  
write.csv(clean, here("data", "clean_specimens.csv"))

## look at the entries where the date is wierd
date_discrep <- clean %>%
  filter(is.na(idigbio_event_date) | is.na(dwc_event_date)) %>% # select rows that have NAs in either date col
  filter(str_detect(idigbio_event_date,"[0123456789]") | str_detect(dwc_event_date, "[0123456789]")) %>% # remove ones that have NO dates 
  mutate(idigbio_event_date = as.character(idigbio_event_date)) %>%
  mutate(across(idigbio_event_date,  
                ~ ifelse(is.na(idigbio_event_date), paste(dwc_event_date), .)))
### look at data trends
ggplot(clean, aes(x = year)) +
  geom_histogram()




# Create a vector of strings
my_strings <- c("This string has 1234 digits and also 5678 and 90123.",
                "Another string with numbers like 1111 and 2222.",
                "No four-digit sequences here.")

