library(dplyr)
library(here)

H1N1.df <- here("Data", "Pandemic (H1N1) 2009.csv") %>%
  read.csv(encoding = "UTF-8", stringsAsFactors = FALSE) # Read in csv file

H1N1.df <- H1N1.df %>% filter(Country != "Grand Total") # Filter out grand total column

unique.countries <- unique(H1N1.df$Country) # List of all unique countries
#unique.dates <- unique(H1N1.df[H1N1.df$Country == "United States of America",]$Update.Time)
unique.dates <- rev(unique(H1N1.df$Update.Time)) # List of all unique dates of updates

cases.df <- data.frame(Cases = integer(length(unique.countries)),row.names = unique.countries) # Create df for storing cases per country

for (i in 1:length(unique.dates)) { # Go through every date
  temp <- data.frame(integer(length(unique.countries)),row.names = unique.countries) # List of cases by country for a given date
  colnames(temp) <- c(unique.dates[i]) # Add current date to list of dates for columns
  cur.countries <- H1N1.df[H1N1.df$Update.Time == unique.dates[i], "Country"] # Pull data for given date
  temp[cur.countries, unique.dates[i]] <- H1N1.df[H1N1.df$Update.Time == unique.dates[i], "Cases"] # Add cases for current date to country list
  cases.df <- cbind(cases.df, temp) # Add list of cases on date i to full list
  if (i>1) { # If we are beyond the first day, modify numbers to reflect new cases on a given date
    if (i==2) {# Special case, second day is just difference between first and second day
      cases.df[,unique.dates[i]] <- cases.df[,unique.dates[i]] - cases.df[,unique.dates[i-1]]
    }
    else { # Otherwise, new cases each day is current day - sum of all previous days
      cases.df[,unique.dates[i]] <- cases.df[,unique.dates[i]] - (rowSums(cases.df[,unique.dates[1:i-1]]))
    }
  }
}

cases.df$Cases <- rowSums(cases.df[,unique.dates[1:length(unique.dates)]]) 
saveRDS(cases.df, file = here("Data", "cases.rds")) # Save formatted data

