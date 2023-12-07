# Load necessary libraries
library(tidyverse)

# Read in raw data
raw_data <- readRDS("data-raw/company_data_Kopie.RDS")

# Perform data cleaning and transformation
df_company <- raw_data %>%
  separate(Price_Volume, into = c("Last_Price", "Last_Volume"), sep = " / ")  %>%
  rename(Volume_Total_Today = Volumen) %>%
  rename(Last_Price_Yesterday = LastPrice) %>%
  mutate(
    Last_Price = as.numeric(gsub("'", "", Last_Price)),
    Last_Volume = as.numeric(gsub("'", "", Last_Volume)),
    Last_Price_Yesterday = as.numeric(gsub("'", "", Last_Price_Yesterday)),
    Volume_Total_Today = as.numeric(gsub("'", "", Volume_Total_Today))
  )

# Save the processed data
usethis::use_data(df_company, overwrite = TRUE)
