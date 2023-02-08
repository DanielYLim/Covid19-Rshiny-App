library(tidyverse)
library(ggplot2)
library(countrycode)
library(ISOcodes)
### Downloading data from google cloud----
# dat <- read.csv("https://storage.googleapis.com/covid19-open-data/v3/epidemiology.csv")

# or

dat <- read.csv("epidemiology.csv")
### Cleaning the data ----

dat$location_key = countrycode(substr(dat$location_key, 1,2), "iso2c", "country.name")


unique(dat$location_key)
min(dat$date)
max(dat$date)

dat$date <- as.Date(dat$date)
names(dat)[2] <- "Country"



### Saving data ----

clean_dat <- dat %>%
  filter(date >= "2019-12-31" & date<= "2022-12-30") %>%
  group_by(Country, date) %>%
  summarise_all(sum) %>%
  arrange(date)

clean_dat = clean_dat %>% 
  filter(!is.na(Country))


clean_dat = clean_dat %>% 
  filter(Country!="Western Sahara")

saveRDS(clean_dat, "covid19_data.rds")



### Example plot ----

data_temp <- clean_dat %>% filter(Country=="Western Sahara")

ggplot(data = data_temp, aes(y=new_confirmed , x= date, color = Country )  ) +
  geom_line(size =1)+
  labs(color = "Country Name")


