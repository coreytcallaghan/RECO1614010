## This R script accompanies Callaghan et al. Valuation of vagrant birds
## It is meant to allow reproducibility of the analysis performed
## It assumes working knowledge of R, directories, installing packages, and reading files in
## The data used for this analysis is available in the repository for download
## The survey data is anonymized and not traceable to individuals
## Any questions, please email c.callaghan@unsw.edu.au

## Packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)
library(Hmisc)
library(broom)

## Read data in
clean_data <- read_csv("Final Survey Data/clean_data_final.csv")


## First, filter out those who haven't taken the survey
## Total number of responses
nrow(clean_data)

clean_data <- clean_data %>%
  filter(eighteen != "No")

## Total number of responses who were above 18
nrow(clean_data)

clean_data <- clean_data %>%
  filter(attempt_to_see_the_birds != "No")

## final number of valid responses
nrow(clean_data)


#### Summarize demographic variables ####
prop.table(table(clean_data$sex))*100

summary(clean_data$age)

prop.table(table(clean_data$marital_status))*100

prop.table(table(clean_data$education))*100

prop.table(table(clean_data$employment_status))*100

#### Summarize birding experience ####
summary(clean_data$years_birding)

prop.table(table(clean_data$birding_experience))*100


## summarize people twitching the terns
prop.table(table(clean_data$successful))*100

prop.table(table(clean_data$number_of_trips))*100

prop.table(table(clean_data$sole_reason))*100

## summarize people's travel
prop.table(table(clean_data$mode_of_transportation))*100

## carpooling
prop.table(table(clean_data$car_pool))*100

summary(clean_data$car_pool_size, na.rm=TRUE)

## Creating dataset for travel cost method analysis
## Assign lat/long to each reported postcode from the respondents
## I use a metadata file which has lat/long for each postcode 
## and the associates suburbs. But, there are mutliple suburbs for each postcode, 
## so I then calculate an approximate location for each postcode, 
## by taking the centroid lat/long from any associated lat/longs.
postcodes_metadata <- read_csv("Australian Postcodes/Australian_Post_Codes_Lat_Lon.csv")

## calculate centroid for each postcode in postcodes metadata
postcodes_lat_lon <- postcodes_metadata %>%
  group_by(postcode) %>%
  summarise(mean_lat=mean(lat, na.rm=TRUE), mean_lon=mean(lon, na.rm=TRUE))

## Combine the postcode lat/lon with the visitor information
## One individual submitted a non-existent postcode, but I fixed it based on
## additional information in the survey questions and the likely mix up of the 
## postcodes as shown here they submitted 3657, but presumably meant 3567
clean_data <- clean_data %>%
  rename(postcode=home_post_code) %>%
  mutate(postcode=as.integer(gsub(3657, 3567, .$postcode))) %>%
  left_join(., postcodes_lat_lon, by="postcode")

## Assign values for the lat/long of the bird
clean_data$bird_lat <- -31.952389
clean_data$bird_lon <- 152.602037

## Calculate distance and travel time
## Note that the gmapsdistance relies on 
## A google maps interface, and as such there
## will be inherent variability in the travel time
## which google estimates. This could affect the results
## to some extent, and thereby, the results reported in the paper
## would be difficult to match exactly, based on this variability
## However, some preliminary assessments showed that the
## differences based on google were minimal and all produced
## comparable results
library(gmapsdistance)

origin <- clean_data %>%
  select(mean_lat, mean_lon) %>%
  unite(origin, mean_lat, mean_lon, sep="+") %>%
  .$origin

destination <- clean_data %>%
  select(bird_lat, bird_lon) %>%
  unite(destination, bird_lat, bird_lon, sep="+") %>%
  .$destination


results <- gmapsdistance(origin, destination, mode="driving", shape="long", combinations="pairwise")

distance_km <- as.vector(results$Distance[,3]/1000)
time_hours <- as.vector(results$Time[,3]/3600)

gdisttime <- data.frame(distance_km=distance_km,
                        time_hours=time_hours)



## Apply values to calculate travel cost
## standard operating cost of automobile (cents/km) 
## [https://www.ato.gov.au/Individuals/Income-and-deductions/Deductions-you-can-claim/Vehicle-and-travel-expenses/Car-expenses/]
standard_auto_cost <- 0.66

## Hourly wage rate
## Calculated on a 38 hour work week, based on 1,77.70 per work week 
## (http://www.abs.gov.au/ausstats/abs@.nsf/mf/6302.0). 
## This value is used to calculate the opportunity cost of 
## time based on half the hourly wage rate
hourly_wage_rate <- 31.04

## Create travel cost within dataset
# first bind full dataset with travel dataset calculated above
full_model_data <- bind_cols(clean_data, gdisttime) %>%
  # select any columns to be kept for analyses
  dplyr::select(number_of_trips, distance_km, time_hours,
                dining_cost, petrol_cost, accomodation_cost, airfare_cost,
                age, sex, marital_status, employment_status, education, postcode, 
                date_first_seen, `overnight?`, car_pool, car_pool_size) %>%
  # classify age into age groups
  mutate(age_group=cut(age, breaks=c(18, 34, 50, 65, Inf), labels=c("18-34", "35-50", "51-65", "66+"))) %>%
  # zero-fill accomodation
  replace_na(list(accomodation_cost=0)) %>%
  rename(overnight=`overnight?`) %>%
  mutate(number_of_trips = gsub("Once", 1, .$number_of_trips)) %>%
  mutate(number_of_trips = gsub("Twice", 2, .$number_of_trips)) %>%
  mutate(number_of_trips = gsub("Three times or more", 3, .$number_of_trips)) %>%
  mutate(number_of_trips = as.integer(as.character(.$number_of_trips))) %>%
  mutate(travel_cost_no_time = ((distance_km*2)*standard_auto_cost),
         opportunity_cost_half_hourly = (time_hours*(0.5*hourly_wage_rate))) %>%
  # if car-pooled, divide the travel cost by the number of car-pool size
  replace_na(list(car_pool_size=1)) %>%
  mutate(travel_cost_no_time = travel_cost_no_time/car_pool_size) %>%
  mutate(travel_cost_time = travel_cost_no_time+opportunity_cost_half_hourly) %>%
  mutate(travel_cost_and_accomodation_no_time=travel_cost_no_time+accomodation_cost,
         travel_cost_and_accomodation_time=travel_cost_time+accomodation_cost) %>%
  ## impute missing data
  mutate(sex=impute(sex),
         marital_status=impute(marital_status, fun="random"),
         employment_status=impute(employment_status, fun="random"),
         education=impute(education, fun="random"),
         age_group=impute(age_group, fun="random"))

# look at that dataset
str(full_model_data)


## Individual TC calculation
## Now, I want to assess the **average** individual's travel cost 
## to get to the site, and adjust this to represent the 
## overall estimate of value of the site. I regress the travel cost 
## against the categorical predictors in order to adjust it. 
## I'll do this once for time and once without time, 
## including accomodation in the estimate.

### First look at the histogram in order to know what distribution to use
## no time
hist(full_model_
     data$travel_cost_and_accomodation_no_time, breaks=50)

## time
hist(full_model_data$travel_cost_and_accomodation_time, breaks=50)

### Try a log-transform to see if it will look more normal
## no time
hist(log(full_model_data$travel_cost_and_accomodation_no_time), breaks=50)

## time
hist(log(full_model_data$travel_cost_and_accomodation_time), breaks=50)
## The log-transform does a reasonable job of making the data normal, 
## and so we can proceed with the gaussian distribution.

## Run a regression model
no_time <- glm(log(travel_cost_and_accomodation_no_time*number_of_trips) ~ overnight + sex + marital_status +
                 employment_status + education + age_group + car_pool, data=full_model_data, family=gaussian)

summary(no_time)

## export as clean table
## This is how table 1 was made
no_time_summary <- tidy(no_time)

# write_csv(no_time_summary, "Submission/Tables/Table 1/Table1.csv")

## Calculate the average predicted value and sum of predicted values
# The average cost per individual without opportunity cost of time included
mean(exp(no_time$fitted.values))

without_time_adjusted_value <- mean(exp(no_time$fitted.values))

# The overall estimate of the event without opportunity cost of time included - using only the respondents
sum(exp(no_time$fitted.values))

## Do the same thing, but including opportunity cost of time in the model
time <- glm(log(travel_cost_and_accomodation_time*number_of_trips) ~ overnight + sex + marital_status +
              employment_status + education + age_group + car_pool, data=full_model_data, family=gaussian)

summary(time)

## export as clean table
time_summary <- tidy(time)

# write_csv(time_summary, "Submission/Tables/Table 2/Table2.csv")


# The average cost per individual with opportunity cost of time included
mean(exp(time$fitted.values))

with_time_adjusted_value <- mean(exp(time$fitted.values))

# The overall estimate of the event with opportunity cost of time included - using only the respondents
sum(exp(time$fitted.values))

## Figures for paper
### Histogram of distance travelled by individuals
ggplot(full_model_data, aes(distance_km))+
  geom_histogram(breaks=seq(0, 2500, by = 50), 
                 col="Black", 
                 fill="gray80", 
                 alpha = .2) + 
  labs(x="Distance (km)", y="")+
  theme_classic()

### Percentage of visits from within 50 km and 100 km
## 100 km
((sum(full_model_data$distance_km <= 100))/nrow(full_model_data))*100

## 500 km
((sum(full_model_data$distance_km <= 500))/nrow(full_model_data))*100

## 1000 km
((sum(full_model_data$distance_km >= 1000))/nrow(full_model_data))*100

## Plot of visits over time
full_model_data$date_first_seen <- as.Date(full_model_data$date_first_seen, format="%d/%m/%Y")


full_model_data %>%
  group_by(date_first_seen) %>%
  summarise(Visits = n()) %>%
  ggplot(., aes(x=date_first_seen, y=Visits)) +
  geom_point()+
  geom_line()+
  theme_classic()+
  xlab("Date")+
  ylab("Visits")+
  theme(axis.text.x=element_text(vjust=1))+
  theme(axis.title.y=element_text(size=14), 
        axis.title.x=element_text(size=14),
        axis.text.x=element_text(size=11, color="black"), 
        axis.text.y=element_text(size=11, color="black"))+
  theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_line(color="gray90"))+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_line(color="gray90"))

### Number of people who visited within the first week 
(sum(full_model_data$date_first_seen <= "2017-12-15"))/nrow(full_model_data)*100

(sum(full_model_data$date_first_seen <= "2017-12-22"))/nrow(full_model_data)*100

## Summary of people's willingness to 'donate' to see the bird
## create a dataframe for each of the hypothetical entry points
five <- as.data.frame(prop.table(table(clean_data$`5$_donation`))*100)
five$value <- "$5"

twentyfive <- as.data.frame(prop.table(table(clean_data$`25$_donation`))*100)
twentyfive$value <- "$25"

fifty <- as.data.frame(prop.table(table(clean_data$`50$_donation`))*100)
fifty$value <- "$50"

seventyfive <- as.data.frame(prop.table(table(clean_data$`75$_donation`))*100)
seventyfive$value <- "$75"

hundred <- as.data.frame(prop.table(table(clean_data$`100$_donation`))*100)
hundred$value <- "$100"

conservation_potential <- bind_rows(five, twentyfive, fifty, seventyfive, hundred)

## now make a figure representing this
ggplot(conservation_potential)+
  geom_bar(aes(x=value, y=Freq, fill=Var1), stat="identity")+
  xlim("$100", "$75", "$50", "$25", "$5")+
  coord_flip()+
  theme_classic()+
  xlab("Theoretical donation value")+
  ylab("Proportion")+
  scale_fill_manual(values=c('grey76','grey20'), name="Response",
                    breaks=c("Yes", "No"),
                    labels=c("Yes", "No"))



## Estimate of total number of birders
## using eBird data
total_number_birders <- clean_data %>%
  select(date_first_seen, submit_to_eBird, how_many_birders_did_you_see)

total_eBird_records <- 113
proportion_of_respondents_who_submitted_to_eBird <- as.data.frame(prop.table(table(total_number_birders$submit_to_eBird))) %>% filter(Var1=="Yes") %>% .$Freq
total <- nrow(total_number_birders)

#eBird estimate
(total_eBird_records*total)/(total*proportion_of_respondents_who_submitted_to_eBird)

eBird_total_estimate <- (total_eBird_records*total)/(total*proportion_of_respondents_who_submitted_to_eBird)

## using birder number estimates
total_number_birders$date_first_seen <- as.Date(total_number_birders$date_first_seen, format="%d/%m/%Y")

total_estimate <- total_number_birders %>%
  group_by(date_first_seen) %>%
  summarise(mean=mean(how_many_birders_did_you_see, na.rm=TRUE),
            max=max(how_many_birders_did_you_see, na.rm=TRUE))

ggplot(total_estimate, aes(x=date_first_seen, y=max))+
  geom_point()+
  geom_line()+
  stat_smooth(method="loess", se=FALSE)

## get the smoothed values
## This looks at how different the results would be if 
## we used a smoothed curve, instead of
## the method above, but there was little difference
model <- loess(max ~ as.numeric(date_first_seen), data=total_estimate)
sum(predict(model))

sum(total_estimate$max)
sum(total_estimate$mean)

birders_total_estimate <- sum(total_estimate$max)

## Total economic estimates
without_time_adjusted_value*eBird_total_estimate

without_time_adjusted_value*birders_total_estimate

with_time_adjusted_value*eBird_total_estimate

with_time_adjusted_value*birders_total_estimate

## Total conservation funds
conservation_potential$estimate1 <- eBird_total_estimate

conservation_potential$estimate2 <- birders_total_estimate

conservation_total_estimate <- conservation_potential %>%
  filter(Var1 == "Yes") %>%
  arrange(Freq) %>%
  mutate(total_freq = Freq - lag(Freq, default = 0)) %>%
  mutate(value_cost=as.integer(as.character(value))) %>%
  group_by(value) %>% 
  summarise(funds1=(((total_freq/100)*estimate1)*value_cost),
            funds2=(((total_freq/100)*estimate2)*value_cost))












