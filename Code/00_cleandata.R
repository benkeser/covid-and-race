##########################################
#### Script for cleaning raw data ########

rm(list=ls())

# -- libraries -- #
library(rgdal); library(maptools); library(maps);library(raster);library(spdep)
library(INLA);library(dplyr);library(ggplot2);library(readr);library(ggthemes)
library(labelled); library(ggsci); library(htmlwidgets); library(plotly);
library(grid); library(gridExtra)

######################################
### Preparing the Shapefile ##########

full_state<- shapefile("/County_COVID/Data/Shapes/cb_2018_us_state_500k")
# plot(coordinates(full_state))
# states<-crop(full_state,extent(-175,-55,19,70))
states<-crop(full_state,extent(-175,-55,19,50))
# plot(coordinates(states))
# adding a variable for merging with the case data
states@data<-states@data%>%mutate(statefp=as.numeric(GEOID))
states@data$id<-rownames(states@data)

######################################
### Preparing the Case /County_COVID/Data ##########
dat <- read_csv("/County_COVID/Data/coviddata.csv")
x <- table(dat$countyfips)

# countyfips==56045 in there twice, one with missing covid deaths
# also, no case data for puerto rico

# dat <- dat %>% filter(!(countyfips==56045 & is.na(coviddeaths)), !is.na(covidcases))
# remove Alaska, no pm2.5 data
dat <- dat %>% filter(!(countyfips==56045 & is.na(coviddeaths)), !is.na(covidcases),state!="Alaska")

# convert proportions to percents
dat$pctunemployed <- dat$pctunemployed * 100
dat$pctuninsured <- dat$pctuninsured * 100
dat$peroccgreaterthan1 <- dat$peroccgreaterthan1 * 100

# add covid case and death rate
dat$covidcases_rate <- dat$covidcases / dat$population * 100000
dat$coviddeaths_rate <- dat$coviddeaths / dat$population * 100000

# imputing missing values with median
dat$heartdiseasedeathrate[is.na(dat$heartdiseasedeathrate)] <- median(dat$heartdiseasedeathrate, na.rm = TRUE)
dat$socialdistance[is.na(dat$socialdistance)] <- median(dat$socialdistance, na.rm = TRUE)

# for this variable only missingness due to left censoring deaths < 10
# for counties with 10-20 deaths, rate is censored by reporting agency, but we include
dat$cerebroandhypertensiondeaths[dat$cerebroandhypertensiondeaths < 10 | is.na(dat$cerebroandhypertensiondeaths)] <- 5
dat$cerebroandhypertensiondeathrate[dat$cerebroandhypertensiondeaths < 10] <- 5/dat$population[dat$cerebroandhypertensiondeaths < 10]* 100000
dat$cerebroandhypertensiondeathrate[dat$cerebroandhypertensiondeaths >=10 & dat$cerebroandhypertensiondeaths <20] <- dat$cerebroandhypertensiondeaths[dat$cerebroandhypertensiondeaths >=10 & dat$cerebroandhypertensiondeaths <20] / dat$population[dat$cerebroandhypertensiondeaths >=10 & dat$cerebroandhypertensiondeaths <20] * 100000

dat$pctunemployed[is.na(dat$pctunemployed)] <- median(dat$pctunemployed, na.rm = TRUE)

# only censoring in HIV rate is due to left censoring
dat$HIVrate[is.na(dat$HIVrate)] <- 2.5 / dat$population[is.na(dat$HIVrate)]

##########################################
### Get length of exposure data ##########
long_data <- read_csv("/County_COVID/Data/covid_confirmed_usafacts_416.csv")
n_days_since_start <- ncol(long_data) - 5
count_days <- function(x){
  tmp <- which(x[5:ncol(x)] > 0)
  if(length(tmp) == 0){
    return(0)
  }else{
    return(n_days_since_start - tmp[1] + 1)
  }
}
idx_first_case <- rep(NA, nrow(long_data))
for(i in seq_along(idx_first_case)){
  idx_first_case[i] <- count_days(long_data[i,])
}

merge_data <- data.frame(countyfips = long_data$countyFIPS,
                         time_since_first_case = idx_first_case)

dat <- left_join(dat, merge_data)

######################################
### Labeling data for tables ##########
label_frame <- list(
  c("population", "Population"),
  c("covidcases", "COVID-19 cases"),
  c("coviddeaths", "COVID-19 deaths"),
  c("percdiabetes", "Pct. diabetes"),
  c("heartdiseasedeathrate", "Heart disease death rate"),
  c("HIVrate", "HIV infection rate"),
  c("pctuninsured", "Pct. uninsured"),
  c("percentwhite", "Pct. white"),
  c("percentblack", "Pct. black"),
  c("pctunemployed", "Pct. unemployed"),
  c("hypertensiondeaths", "Hypertension deaths"),
  c("urbanicity2013", "Urbanicity score"),
  c("percentover65", "Pct. over 65 years old"),
  c("cerebroandhypertensiondeaths", "Cerebrovascular and hypertension deaths"),
  c("cerebroandhypertensiondeathrate", "Cerebrovascular and hypertension death rate"),
  c("pm25", "PM2.5"),
  c("covidcases_rate", "COVID-19 case rate"),
  c("coviddeaths_rate", "COVID-19 death rate"),
  c("socialdistance", "Social distancing score"),
  c("peroccgreaterthan1", "Pct. occupancy > 1 per room"),
  c("time_since_first_case", "Days since first case")
)

# label data
lapply(label_frame, function(x){
  var_label(dat[[x[1]]]) <<- x[2]
})

# make adjacency matrix and add to data
nb.r <- poly2nb(states, queen = FALSE)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE) 

# -- add state ID that corresponds to the adjacency matrix into the data -- #
StateIDs <- states@data[,c("statefp","id")] %>% 
          mutate(id=as.numeric(id))
dat <- dat %>% 
     left_join(StateIDs) %>%
     mutate(id=as.numeric(id))
