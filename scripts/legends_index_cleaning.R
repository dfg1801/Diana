# Load required libraries
library(tidyverse)
library(here) 

# Read the text file
# Replace "your_file.txt" with the actual path to your text file

text_data <- readLines(here("data", "Legends_index_Birds.txt"))

# Remove empty lines
text_data <- text_data[text_data != ""]

# Initialize empty vectors to store extracted data
subjects <- c()
legends <- c()
books <- c()
pages <- c()
hsl_call_numbers <- c()
uhm_call_numbers <- c()
online_links <- c()

# Process the data
i <- 1
while (i <= length(text_data)) {
  line <- text_data[i]
  
  # Check if this is a "Subject:" line (start of new entry)
  if (str_detect(line, "^Subject:")) {
    # Extract subject
    subject <- str_extract(line, "(?<=Subject: ).*")
    
    # Initialize variables for this entry
    legend <- NA
    book <- NA
    page_info <- NA
    hsl_call <- NA
    uhm_call <- NA
    online <- NA
    
    # Process subsequent lines for this entry
    j <- i + 1
    while (j <= length(text_data) && !str_detect(text_data[j], "^Subject:")) {
      current_line <- text_data[j]
      
      # Extract legend name
      if (str_detect(current_line, "appears in the legend:")) {
        legend <- str_extract(current_line, '(?<=appears in the legend: ")[^"]*')
      }
      
      # Extract book information
      else if (str_detect(current_line, "in the book:")) {
        book <- str_extract(current_line, "(?<=in the book: ).*")
      }
      
      # Extract page information
      else if (str_detect(current_line, "on pages?:")) {
        page_info <- str_extract(current_line, "(?<=on pages?: ).*")
      }
      
      # Extract HSL Call Number
      else if (str_detect(current_line, "HSL Call Number:")) {
        hsl_call <- str_extract(current_line, "(?<=HSL Call Number: ).*")
      }
      
      # Extract UHM Call Number
      else if (str_detect(current_line, "UHM Call Number:")) {
        uhm_call <- str_extract(current_line, "(?<=UHM Call Number: ).*")
      }
      
      # Extract Online link
      else if (str_detect(current_line, "Online:")) {
        online <- str_extract(current_line, "(?<=Online: ).*")
      }
      
      j <- j + 1
    }
    
    # Store the extracted data
    subjects <- c(subjects, subject)
    legends <- c(legends, legend)
    books <- c(books, book)
    pages <- c(pages, page_info)
    hsl_call_numbers <- c(hsl_call_numbers, hsl_call)
    uhm_call_numbers <- c(uhm_call_numbers, uhm_call)
    online_links <- c(online_links, online)
    
    # Move to next entry
    i <- j
  } else {
    i <- i + 1
  }
}

# Create the data frame
df <- data.frame(
  Subject = subjects,
  Legend = legends,
  Book = books,
  Pages = pages,
  HSL_Call_Number = hsl_call_numbers,
  UHM_Call_Number = uhm_call_numbers,
  Online_Link = online_links,
  stringsAsFactors = FALSE
)

# Clean up the data (remove leading/trailing whitespace)
df <- df %>%
  mutate(across(everything(), str_trim))

# View the first few rows
head(df)

# Write to CSV
write.csv(df, here("output", "legends_index_birds.csv"), row.names = FALSE)

# Display summary
cat("Data transformation complete!\n")
cat("Total entries:", nrow(df), "\n")
cat("CSV file saved as 'hawaiian_legends_birds.csv'\n")

# Optional: View the structure of the final dataset
str(df)
