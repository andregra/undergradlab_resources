rm(list = ls())       # Clear variables

library(haven)
library(tidyverse)
library(dplyr)
#library(tidyr)
library(ggplot2)
library(seasonal)
library(foreign)
library(sf)
library(plm)
library(fixest)
library(data.table)

path <- "C:/Users/andre/Dropbox/ucsd_classes/research/cohabit_housing"

##IPUMS CPS data
#load in data
#cps data after 2000
cps <- read_dta(paste0(path,"/data/cps_00007.dta/cps_00007.dta",sep=""))

##cps data before 2000
cps_old <- read_dta(paste0(path,"/data/cps_00006.dta/cps_00006.dta",sep=""))

### COHABIT DEFINITION
## RELATE==1114 gives us unmarried partner of hh head starting March 1995
##PECOHAB gives any cohabit lineno in hh starting 2007


## pecohab which households are cohabitating
## month // year -- dates of when interviewed
## serial -- household ID
## county -- ID of the county we're in

## keep adults
cps <- subset(cps, age>=18)
cps_old <- subset(cps_old, age>=18)

vars_to_keep <- c("serial", "metfips", "age", "year", "month", "educ", "relate", "pecohab", "sex", "lineno","county")
cps <- cps %>% dplyr::select(all_of(vars_to_keep))
vars_to_keep <- c("serial", "metfips", "age", "year", "month", "educ", "relate", "sex","county")
cps_old <- cps_old %>% dplyr::select(all_of(vars_to_keep))

##keep only metro areas
cps <- subset(cps, county!=0)
cps_old <- subset(cps_old, county!=0)

gc()
#we're going to create 2 datasets at the county level

## we can then aggregate our county level estimates to metro area, with consistent crosswalk
## otherwise the metfips and metarea variables change too much, just assign counties

##one is at the age-sex-cell using PECOHAB
##one is at the hh level using RELATE

# for each we'll create monthly and yearly versions
## as part of this we'll create a match efficiency for cohabitating partners


##----------------
## RELATE dataset
##----------------
##assortative matching result based on RELATE command
cps_old <- bind_rows(cps_old, cps)
cps_old$rel_cohab <- ifelse(cps_old$relate==1114 | cps_old$relate==1116 | cps_old$relate==117,1,0)
cps_old$rel_roommate <- ifelse(cps_old$relate==1115,1,0)
cps_old$rel_child <- ifelse(cps_old$relate==301,1,0)

##pull just hh heads and positives
cps_old <- subset(cps_old, rel_cohab==1 | rel_roommate==1 | rel_child==1 | relate==101)

# Convert to data.table by reference (no copy made)
setDT(cps_old)

gc()

# Compute group-wise max values efficiently
cps_old[, `:=`(
  maxroom = max(rel_roommate, na.rm = TRUE),
  maxchild = max(rel_child, na.rm = TRUE),
  maxcohab = max(rel_cohab, na.rm = TRUE)
), by = .(serial, year, month)]


cps_old <- cps_old[rel_cohab == 1 | relate == 101]

## find households with roommates, with cohabitating partner, with children
#cps_old <- cps_old %>%
 # group_by(serial, year,month) %>%
  #mutate(across(c(rel_roommate, rel_child, rel_cohab), max, .names = "max{.col}"))

##create a school years variable
cps_old <- cps_old[, educ_years := fcase(
  educ == 2, 0,
  educ == 10, 2.5,
  educ == 20, 6.5,
  educ == 30, 8.5,
  educ == 40, 10,
  educ == 50, 11,
  educ == 60, 12,
  educ == 70, 13,
  educ == 71, 12,
  educ == 73, 13,
  
  educ == 81, 14,
  educ == 91, 15,
  educ == 92, 15,
  educ == 111, 17,
  educ == 123, 19,
  educ == 124, 19,
  educ == 125, 22,
  default = NA_real_
)]

## define match distance using data.table
cps_old <- cps_old[
  order(serial, year, month, maxcohab),  # sort within group
  `:=`(
    ed_diff = ifelse(maxcohab == 1, (educ_years[1] - educ_years)^2, NA_real_),
    age_diff = ifelse(maxcohab == 1, (age[1] - age)^2, NA_real_)
  ),
  by = .(serial, year, month)
]

cps_old[, `:=`(
  max_ed_diff = max(ed_diff, na.rm = TRUE),
  max_age_diff = max(age_diff, na.rm = TRUE)
), by = .(serial, year, month)]


rel_met_my <- cps_old[relate == 101, .(
  cohab = mean(maxcohab, na.rm = TRUE),
  roommate = mean(maxroom, na.rm = TRUE),
  wparent = mean(maxchild, na.rm = TRUE),
  ed_diff = mean(max_ed_diff, na.rm = TRUE),
  age_diff = mean(max_age_diff, na.rm = TRUE),
  count = .N
), by = .(county, year, month)]

write.dta(rel_met_my,  paste(path,"/output/relate_county_my.dta",sep=""))

rel_met_y <- cps_old[relate == 101, .(
  cohab = mean(maxcohab, na.rm = TRUE),
  roommate = mean(maxroom, na.rm = TRUE),
  wparent = mean(maxchild, na.rm = TRUE),
  ed_diff = mean(max_ed_diff, na.rm = TRUE),
  age_diff = mean(max_age_diff, na.rm = TRUE),
  count = .N
), by = .(county, year)]

write.dta(rel_met_my,  paste(path,"/output/relate_county_y.dta",sep=""))

## clear big data 
rm(cps_old)

gc()


#collapse cohabit rate to the county level
#cps$ym <- paste(cps$year,cps$month,"01",sep="-")
#cps$date <- as.Date(cps$ym, format="%Y-%m-%d")

#cps_agg <- cps_agg %>%
 # mutate(cpsmonth = as.numeric(format(date, "%m")),
  #       cpsyear = as.numeric(format(date,"%Y")))

##---------------------------------------
##PECOHAB age-sex cells
##---------------------------------------
#only defined 2007 onward
cps <- subset(cps, year>=2007)

cps$ind_cohab <- ifelse(cps$pecohab > 0, 1, 0)
## get education and age of partner

## define age groups
cps <- cps  %>%
  mutate(age_group = cut(
    age,
    breaks = seq(18, 98, by = 5),      # 0-4, 5-9, ..., 95-99
    include.lowest = TRUE,
    right = FALSE,
    labels = paste(seq(18, 93, by = 5), seq(22, 98, by = 5), sep = "-")
  ))


setDT(cps)

##create a school years variable
cps[, educ_years := fcase(
  educ == 2, 0,
  educ == 10, 2.5,
  educ == 20, 6.5,
  educ == 30, 8.5,
  educ == 40, 10,
  educ == 50, 11,
  educ == 60, 12,
  educ == 70, 13,
  educ == 71, 12,
  educ == 73, 13,
  
  educ == 81, 14,
  educ == 91, 15,
  educ == 92, 15,
  educ == 111, 17,
  educ == 123, 19,
  educ == 124, 19,
  educ == 125, 22,
    default = NA_real_
)]


# Create a version of cps with partner info
partner_info <- cps[, .(partner_lineno = lineno, partner_age = age, partner_educyears = educ_years, serial, year, month)]

# Perform the self-join using keys
setkey(partner_info, serial, year, month, partner_lineno)
setkey(cps, serial, year, month, pecohab)

# Left join: match pecohab to partner_lineno within the same household/time
cps_with_partner_age <- partner_info[cps]

cps_with_partner_age[, age_diff := (partner_age - age)^2]
cps_with_partner_age[, ed_diff := (partner_educyears - educ_years)^2]


##individuals at the met, month, year level
ind_met_my <- cps_with_partner_age[, .(
  mcohab = mean(ind_cohab, na.rm = TRUE),
  med_diff = mean(ed_diff, na.rm = TRUE),
  mage_diff = mean(age_diff, na.rm = TRUE),
  count = .N
), by = .(county, month, year)]

write.dta(ind_met_my,  paste(path,"/output/pecohab_county_my.dta",sep=""))

##individuals at the met, age-sex, year level
ind_met_y <- cps_with_partner_age[, .(
  mcohab = mean(ind_cohab, na.rm = TRUE),
  med_diff = mean(ed_diff, na.rm = TRUE),
  mage_diff = mean(age_diff, na.rm = TRUE),
  count = .N
), by = .(metfips, age_group, sex, year)]

write.dta(ind_met_y,  paste(path,"/output/pecohab_county_y.dta",sep=""))




##--------------------------------------------
##IGNORE EVERYTHING BELOW THIS LINE
##--------------------------------------
### mapping runs
#nyc 36061
# LA 06037
subset(cps_agg, statefip==36 & is.na(mean_cohab)!=1) %>% 
  ggplot( aes(x=date, y=mean_cohab)) +
  geom_line() 

cps_agg$date2 <- as.Date(paste(cps_agg$cpsyear, cps_agg$cpsmonth, "01", sep = "-"))

subset(cps_agg, county==36061 & is.na(mean_cohab)!=1) %>% 
  ggplot( aes(x=date2, y=mean_cohab)) +
  geom_line() 

subset(cps_agg, county==6075 & is.na(mean_cohab)!=1) %>% 
  ggplot( aes(x=date2, y=mean_cohab)) +
  geom_line() 

series <- subset(cps_agg, statefip==36 & is.na(mean_cohab)!=1)

series2 <- ts(series[c("mean_cohab")], start = c(2007,1), frequency = 12)


cps_agg2 <- cps_agg %>%
  group_by(date) %>%
  summarize(mean_cohab2 = mean(mean_cohab))

series <- subset(cps_agg2, is.na(mean_cohab2)!=1)

try <- seas(series)

seasonally_adjusted <- final(try)

# Plot
plot(series, main = "Original vs. Seasonally Adjusted")
lines(seasonally_adjusted, col = "red")


cps_agg2 %>%
  ggplot( aes(x=date, y=mean_cohab2)) +
  geom_line()


zillow_ny <- subset(zillow_long, county_fips_num==36061)
zillow_ny$zillow_dateformat <- as.Date(zillow_ny$zillow_date, format = "%Y-%m-%d")

zillow_ny %>%
  ggplot( aes(x=zillow_dateformat, y=zillow_index)) +
  geom_line()

#merge data
cps_zillow <- merge(cps_agg,zillow, by.x="county", by.y="county_fips_num")

#transform to panel data
long_data <- pivot_longer(
  cps_zillow,
  cols = starts_with("2"), # Specify the columns to reshape
  names_to = "zillow_date",          # Name for the variable column
  values_to = "zillow_index"             # Name for the value column
)

long_H <- pivot_longer(
  H_filtered,
  cols = starts_with("Attended"),
  names_to = "Years",
  values_to = "did_attend"
)

#note that this data now gives us each county-month-year connected to the full zillow history of this county
# we can change this to be just zillow observations from the same month year

long_data$zillow_dateformat <- as.Date(long_data$zillow_date, format = "%Y-%m-%d")

long_data$zillowmonth <- as.numeric(format(long_data$zillow_dateformat, "%m"))
long_data$zillowyear <- as.numeric(format(long_data$zillow_dateformat, "%Y"))

#cut to only matching month years
long_data <- subset(long_data, zillowyear==year)
long_data <- subset(long_data, zillowmonth==month)


