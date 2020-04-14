library(dplyr)
library(here)

H1N1.df <- here("Data", "Pandemic (H1N1) 2009.csv") %>%
  read.csv(encoding = "UTF-8", stringsAsFactors = FALSE)

H1N1.df <- H1N1.df %>% filter(Country != "Grand Total")

unique.countries <- unique(H1N1.df$Country)
#unique.dates <- unique(H1N1.df[H1N1.df$Country == "United States of America",]$Update.Time)
unique.dates <- rev(unique(H1N1.df$Update.Time))

cases.df <- data.frame(Cases = integer(length(unique.countries)),row.names = unique.countries)

for (i in 1:length(unique.dates)) {
  temp <- data.frame(integer(length(unique.countries)),row.names = unique.countries)
  colnames(temp) <- c(unique.dates[i])
  cur.countries <- H1N1.df[H1N1.df$Update.Time == unique.dates[i], "Country"]
  temp[cur.countries, unique.dates[i]] <- H1N1.df[H1N1.df$Update.Time == unique.dates[i], "Cases"]
  cases.df <- cbind(cases.df, temp)
  if (i>1) {
    if (i==2) {
      cases.df[,unique.dates[i]] <- cases.df[,unique.dates[i]] - cases.df[,unique.dates[i-1]]
    }
    else {
      cases.df[,unique.dates[i]] <- cases.df[,unique.dates[i]] - (rowSums(cases.df[,unique.dates[1:i-1]]))
    }
  }
}

cases.df$Cases <- rowSums(cases.df[,unique.dates[1:length(unique.dates)]])
