## James Doyle
setwd("/Users/jamiedoyle/Desktop/Cherry-Blossoms-2023-main")
## Cherry Blossom Prediction Competition Code

# Install libraries
library(tidyverse)

# Import data
cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))
npn_individual <- read.csv("data/USA-NPN_individual_phenometrics_data.csv")
vanc <- read.csv("data/vancouver.csv")
#npn_intensity <- read.csv("data/USA-NPN_status_intensity_observations_data.csv")

# Visualize past data per location
ggplot(cherry[cherry["location"]=="washingtondc",], 
       aes(x=year, y=bloom_doy)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Cherry Blossom Bloom Dates in Washington DC", "Bloom Date Measured in Day of Year")

ggplot(cherry[cherry["location"]=="liestal",], 
       aes(x=year, y=bloom_doy)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Cherry Blossom Bloom Dates in Liestal", "Bloom Date Measured in Day of Year")

ggplot(cherry[cherry["location"]=="kyoto",], 
       aes(x=year, y=bloom_doy)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Cherry Blossom Bloom Dates in Kyoto", "Bloom Date Measured in Day of Year")


## Count value types to see range of data for values
dplyr::count(npn_individual, First_Yes_Year, sort = TRUE)
min(cherry$year)
max(cherry$year)
dplyr::count(cherry, location, sort = TRUE)


## Reformat the data for modeling
(npn_individual$First_Yes_Date <- as.Date(paste(npn_individual$First_Yes_Month, 
                                                npn_individual$First_Yes_Day, 
                                                npn_individual$First_Yes_Year), 
                                          format = "%m %d %Y"))
(npn_individual$Bloom_Day <- as.numeric(format(npn_individual$First_Yes_Date, "%j")))


## Create a model from the available data
# Values: Year, Lat, Long, Elevation + (Winter/Spring Temp/Precip?)
npn_trimmed <- data.frame(year = npn_individual$First_Yes_Year, 
                          lat = npn_individual$Latitude, 
                          long = npn_individual$Longitude,
                          alt = npn_individual$Elevation_in_Meters, 
                          bloom_date = npn_individual$First_Yes_Date,
                          bloom_day = npn_individual$Bloom_Day)
model <- lm(bloom_day ~ year + lat + long + alt, 
            npn_trimmed)


## Apply the model to the data points and make predictions
# predictions <- expand_grid(location = unique(cherry$location),
#                            year = 1900:2032, lat, long, alt) %>% 
#   bind_cols(predicted_doy = predict(model, newdata = .))

# I need a new Cherry set with the predicting years but copies of lat, long, & alt
(lats <- c(rep(mean(cherry[cherry$location=="kyoto", 'lat']), 10),
           rep(mean(cherry[cherry$location=="liestal", 'lat']), 10),
           rep(mean(cherry[cherry$location=="washingtondc", 'lat']), 10)))
(longs <- c(rep(mean(cherry[cherry$location=="kyoto", 'long']), 10),
            rep(mean(cherry[cherry$location=="liestal", 'long']), 10),
           rep(mean(cherry[cherry$location=="washingtondc", 'long']), 10)))
(alts <- c(rep(mean(cherry[cherry$location=="kyoto", 'alt']), 10),
           rep(mean(cherry[cherry$location=="liestal", 'alt']), 10),
           rep(mean(cherry[cherry$location=="washingtondc", 'alt']), 10)))
(years <- 2023:2032)
(cherry_future <- data.frame(year = years, lat = lats, long = longs, alt = alts,
                             location = c(rep("kyoto",10), rep("liestal", 10), 
                                          rep("washingtondc", 10))))

## Make the predictions
(pred_nums <- round(predict(model, cherry_future)))
# c(predictions, cherry_future$location)
(predictions <- data.frame(year=cherry_future$year, 
                           location=cherry_future$location, 
                           value=pred_nums) %>%
  pivot_wider(id_cols = c(year, location), 
              names_from = location, 
              values_from = value))

## Extrapolate to Vancouver
(vanc_future <- data.frame(year = years, 
                           lat = rep(vanc$lat, 10),
                           long = rep(vanc$long, 10),
                           alt = rep(vanc$alt, 10),
                           location = rep("vancouver", 10)))
predictions$vancouver <- round(predict(model, vanc_future))
predictions

## Export the results
write.csv(predictions, file = "cherry-predictions.csv",
          row.names = FALSE)


## Print the predictions as dates
as.Date(predictions$kyoto, origin = paste0(predictions$year,"-01-01"))  # 5/5
as.Date(predictions$liestal, origin = paste0(predictions$year,"-01-01"))  # 5/26
as.Date(predictions$washingtondc, origin = paste0(predictions$year,"-01-01"))  # 4/5
as.Date(predictions$vancouver, origin = paste0(predictions$year,"-01-01"))  # 4/27




