setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

legis_curr.url <- "https://theunitedstates.io/congress-legislators/legislators-current.csv"
download.file(legis_curr.url, destfile = "legislators-current.csv")

legis_past.url <- "https://theunitedstates.io/congress-legislators/legislators-historical.csv"
download.file(legis_past.url, destfile = "legislators-historical.csv")

gov_control_url <- "https://raw.githubusercontent.com/MikeGruz/us-gov-control-historical/master/us_government_control_1857-2016.csv"
download.file(gov_control_url, destfile = "us_government_control_1857-2016.csv")

# *terms.csv:
# https://data.world/govtrack/us-congress-legislators

# congress_terms.csv:
# https://www.senate.gov/legislative/DatesofSessionsofCongress.htm

terms <- read_csv("congress_terms.csv")

terms <- terms %>%
  rowwise() %>%
  mutate(start.1 = stringr::str_extract_all(`Begin Date`, "[a-zA-Z]{3} \\d{1,2}, \\d{4}")[[1]][1],
         start.2 = stringr::str_extract_all(`Begin Date`, "[a-zA-Z]{3} \\d{1,2}, \\d{4}")[[1]][2],
         end.1 = stringr::str_extract_all(`Adjourn Date`, "[a-zA-Z]{3} \\d{1,2}, \\d{4}")[[1]][1],
         end.2 = stringr::str_extract_all(`Adjourn Date`, "[a-zA-Z]{3} \\d{1,2}, \\d{4}")[[1]][2]) %>%
  mutate(start.1 = as.Date(start.1, format="%b %d, %Y"),
         start.2 = as.Date(start.2, format="%b %d, %Y"),
         end.1 = as.Date(end.1, format="%b %d, %Y"),
         end.2 = as.Date(end.2, format="%b %d, %Y")) %>%
  filter(!is.na(Congress)) %>%
  select(congress=Congress, begin.date.1=start.2, end.date.1=end.2, begin.date.2=start.1, end.date.2=end.1)
terms$begin.date.1[1] <- as.Date("2023-01-03")
terms$end.date.1[1]   <- as.Date("2024-01-03")
terms$begin.date.2[1] <- as.Date("2024-01-03")
terms$end.date.2[1]   <- as.Date("2025-01-03")

write_csv(terms, file = "congress_terms.csv")
