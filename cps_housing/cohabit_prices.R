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


## connect cps and price data

##-------------------------------
## generate price datasets
##------------------------------
##BRING IN HOUSE PRICES
zillow <- read_csv(paste0(path,"/data/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month (1).csv",sep=""))  
redfin <- read.delim(paste0(path,"/data/county_market_tracker.tsv000/county_market_tracker.tsv000 ",sep=""))
redfin_metro <- read.delim(paste0(path,"/data/redfin_metro_market_tracker.tsv000/redfin_metro_market_tracker.tsv000 ",sep=""))
redfin <- subset(redfin, property_type=="All Residential")
redfin_metro <- subset(redfin_metro, PROPERTY_TYPE=="All Residential" & IS_SEASONALLY_ADJUSTED=="false")

bogin <- readxl::read_xlsx("C:/Users/andre/Dropbox/ucsd_classes/tiebout/data/housing/Bogin_2018/HPI_AT_BDL_county.xlsx", skip=6)
  
#create fips code in zillow data by adding state to county FIPS

zillow$county_fips <- paste(zillow$StateCodeFIPS,zillow$MunicipalCodeFIPS,sep="")

#note that in the cps data, we are given a number, so leading zeroes are removed. 
#do the same to zillow data:

zillow$county_fips_num <- as.numeric(zillow$county_fips)



zillow_long <- pivot_longer(
  zillow,
  cols = starts_with("2"), # Specify the columns to reshape
  names_to = "zillow_date",          # Name for the variable column
  values_to = "zillow_index"             # Name for the value column
) 


zillow_long$zillow_dateformat <- as.Date(zillow_long$zillow_date, format = "%Y-%m-%d")
zillow_long$zillowmonth <- as.numeric(format(zillow_long$zillow_dateformat, "%m"))
zillow_long$zillowyear <- as.numeric(format(zillow_long$zillow_dateformat, "%Y"))

## this panel is at the year month level , but use the full date variable "zillow date format"
panel <- pdata.frame(zillow_long,index=c("county_fips","zillow_dateformat"))  

## we define change from the last month, or from the last two months (lag2)
panel$z_change <- (panel$zillow_index - lag(panel$zillow_index))/ lag(panel$zillow_index)
panel$z_lagchange <- (lag(panel$zillow_index) - lag(panel$zillow_index,2))/ lag(panel$zillow_index,2)
panel$z_lagchange2 <- (lag(panel$zillow_index,2) - lag(panel$zillow_index,3))/ lag(panel$zillow_index,3)
panel$z_logchange <- (log(panel$zillow_index) - log(lag(panel$zillow_index)))

panel <- subset(panel, is.na(zillow_index)!=1)


### produce a panel at the yearly level
##first aggregate zillow_long to the year

zillow_yearlong <- zillow_long %>%
  dplyr::group_by(zillowyear, county_fips) %>%
  dplyr::summarize(mean_price = mean(zillow_index))


##yearly panel
yearpanel <- pdata.frame(zillow_yearlong,index=c("county_fips","zillowyear"))  

## define a 5-year change, annualized (annualized just means the percent change divided by num of years)
yearpanel$change5year_annualized <- ((yearpanel$mean_price - lag(yearpanel$mean_price,5))/ lag(yearpanel$mean_price,5)) / 5


##----------------------------
## merge to CPS
##----------------------------
rel_county_my <- read_dta(paste(path,"/output/relate_county_my.dta",sep=""))

##merge in metros
metros <- read_dta(paste(path,"/data/cbsa2fipsxw.dta",sep=""))
metros$county <- as.numeric(paste(metros$fipsstatecode,metros$fipscountycode,sep=""))
rel_county_my <- merge(rel_county_my, metros, by="county")

## merge metro data to redfin
rel_metro_my <- rel_county_my %>%
  dplyr::group_by(year, month, cbsacode) %>%
  dplyr::summarize(avg_cohab = weighted.mean(cohab,count, na.rm=TRUE),
                   avg_room=weighted.mean(roommate,count, na.rm=TRUE),
                   avg_wparent=weighted.mean(wparent,count, na.rm=TRUE),
                   avg_ed_diff=weighted.mean(ed_diff,count, na.rm=TRUE),
                   avg_age_diff=weighted.mean(age_diff,count, na.rm=TRUE),
                   
                   count = sum(count))

redfin_metro <- redfin_metro %>%
  mutate(date=as.Date(PERIOD_BEGIN, format = "%Y-%m-%d"),
         month = as.numeric(format(date, "%m")),
         year = as.numeric(format(date, "%Y"))
  )

rel_metro_my <- merge(rel_metro_my, redfin_metro, by.x=c("cbsacode","month","year"), 
                       by.y=c("TABLE_ID","month","year"))

##merge county data to zillow
rel_county_my <- merge(rel_county_my, as.data.frame(panel), by.x=c("county","month","year"), 
                    by.y=c("county_fips_num","zillowmonth","zillowyear"))

## differences in cohab and roommate
panel <- pdata.frame(rel_county_my,index=c("county_fips","zillow_dateformat"))  

## we define change from the last month, or from the last two months (lag2)
panel$cohab_change <- (panel$cohab - lag(panel$cohab))/ lag(panel$cohab)
panel$eddiff_change <- (panel$ed_diff - lag(panel$ed_diff))/ lag(panel$ed_diff)
panel$agediff_change <- (panel$age_diff - lag(panel$age_diff))/ lag(panel$age_diff)


#add bogin

boginprices <- merge(panel, bogin, by.x=c("county","year"), 
                            by.y=c("FIPS code","Year"), all.x=TRUE)

boginprices <- boginprices %>%
  dplyr::rename(annual_change=`Annual Change (%)`,
                hpi1990=`HPI with 1990 base`,
                hpi2000=`HPI with 2000 base`,
                state = State.x) %>%
  dplyr::select(-c("State.y"))
## print dataset
write_dta(boginprices,  paste(path,"/output/relate_panel.dta",sep=""))


## differences in cohab and roommate
rel_metro_my$date <- as.Date(rel_metro_my$PERIOD_BEGIN, format = "%Y-%m-%d")

mpanel <- pdata.frame(rel_metro_my,index=c("cbsacode","date"))  

## we define change from the last month, or from the last two months (lag2)
mpanel$cohab_change <- (mpanel$avg_cohab - lag(mpanel$avg_cohab))/ lag(mpanel$avg_cohab)
mpanel$eddiff_change <- (mpanel$avg_ed_diff - lag(mpanel$avg_ed_diff))/ lag(mpanel$avg_ed_diff)
mpanel$agediff_change <- (mpanel$avg_age_diff - lag(mpanel$avg_age_diff))/ lag(mpanel$avg_age_diff)
mpanel$p_change <- (mpanel$MEDIAN_SALE_PRICE - lag(mpanel$MEDIAN_SALE_PRICE))/ lag(mpanel$MEDIAN_SALE_PRICE)

write_dta(mpanel,  paste(path,"/output/relatemetro_panel.dta",sep=""))





## sanity check
#negative relation between cohab and price
model <- feols(cohab ~ log(zillow_index) + SizeRank  + year + roommate | State + month , data = rel_county_my, cluster="county")
summary(model)

## roommate postiively predicted by price, but not cohab
model <- feols(avgcohab ~ log(MEDIAN_SALE_PRICE) + year + HOMES_SOLD | month +STATE_CODE, data = rel_metro_my, cluster="cbsacode")
summary(model)


## what about ed_diffs?
model <- feols(ed_diff ~ (as.numeric(`Annual Change (%)`)) + cohab  | county + year , data = rel_county_y, cluster="county")
summary(model)

## fixed effects reg
model <- feols(cohab ~ log(zillow_index) | county + year + month, data = subset(rel_county_my))
summary(model)


## your final dataset --
# county, year, month, mean_cohab, price variables .....
"county price panel"
## extract the coefficiet and standard errors from each model to produce the graph
## chat gpt -- how to store beta coefficents from 3 regressions in a new dataset


##reg in differences -- lags matter?
model <- feols(cohab_change ~ z_lagchange |  year + month , data = subset(panel), cluster="county")
summary(model)
## as cohabitation increases, ed differences increases -- tightness in market
model <- feols(agediff_change ~ z_lagchange + cohab_change |  county + year + month , data = subset(panel))
summary(model)
# not in the levels tho
model <- feols(age_diff ~ z_lagchange + cohab | county*year + year + month , data = subset(panel))
summary(model)

##Homes sold and list to sale have an effect -- tightness of housing market?
model <- feols(avgcohab ~  log(MEDIAN_LIST_PRICE) + log(NEW_LISTINGS) + HOMES_SOLD_YOY +avgroom  | cbsacode + year + month, data = subset(rel_metro_my))
summary(model)


## yearly variation
rel_county_y <- read_dta(paste(path,"/output/relate_county_y.dta",sep=""))

rel_county_y <- merge(rel_county_y, bogin, by.x=c("county","year"), 
                       by.y=c("FIPS code","Year"))

## fixed effects reg
model <- feols(cohab ~ as.numeric(`HPI with 1990 base`) + roommate | county + year, data = rel_county_y)
summary(model)

model <- feols(cohab ~ as.numeric(`Annual Change (%)`) | county + year, data = rel_county_y)
summary(model)

#sanity check
model <- feols(wparent ~ as.numeric(`HPI`) + year | State + month, data = rel_county_y, cluster="county")
summary(model)


rel_county_my$test <- as.Date(rel_county_my$zillow_date)

##-------------------------------------
## plot trends
##------------------------------------
# Compute average value per time point
trends <- panel %>%
  dplyr::group_by(as.Date(zillow_dateformat)) %>%
  filter(is.finite(cohab_change)) %>%
  dplyr::summarize(avg_cohab = mean(cohab, na.rm=TRUE),
                   avg_room=mean(roommate, na.rm=TRUE),
                   avg_cohabchange=mean(cohab_change, na.rm=TRUE),
                   avg_hhchange = mean(z_change, na.rm=TRUE),
                   avg_agediff = mean(age_diff, na.rm=TRUE),
                   avg_eddiff = mean(ed_diff, na.rm=TRUE)
)

trends_y <- rel_county_y %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(avg_cohab = mean(cohab, na.rm=TRUE),
                   avg_room=mean(roommate, na.rm=TRUE),
                   avg_agediff = mean(age_diff, na.rm=TRUE),
                   avg_eddiff = mean(ed_diff, na.rm=TRUE)
  )

trends_met <- rel_metro_my %>%
  dplyr::group_by(date, STATE_CODE) %>%
  dplyr::summarize(avg_cohab = mean(avgcohab, na.rm=TRUE),
                   avg_room=mean(avgroom, na.rm=TRUE)
            
  )


# Plot
ggplot(trends, aes(x = `as.Date(zillow_dateformat)`)) +
  geom_line(aes(y = avg_cohab), color = "blue", size = 1) +
  labs(title = "Match Quality over Time", x = "Time", y = "Value") +
  theme_minimal()

ggplot(trends_y, aes(x = year)) +
  geom_line(aes(y = avg_room), color = "blue", size = 1) +
  geom_line(aes(y = avg_cohab), color = "red", size = 1) +
  labs(title = "Yearly Cohab", x = "Time", y = "Value") +
  theme_minimal()

ggplot(trends, aes(x = `as.Date(zillow_dateformat)`)) +
  geom_line(aes(y = avg_cohabchange), color = "blue", size = 1) +
  geom_line(aes(y = avg_hhchange), color = "red", size = 1) +
  labs(title = "Cohabitation and HH Price Change Over Time", x = "Time", y = "Value") +
  theme_minimal()

ggplot(subset(trends_met, STATE_CODE=="NY"), aes(x = date, y = avg_room, color = STATE_CODE)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Multiple Lines Over Time",
       x = "Time",
       y = "Value",
       color = "Group")



##-------------------------------------------------------------------------------------------
##---------------------------------------------------------------------------------------------


cps$monthyear <- as.Date(zillow_long$zillow_date, format = "%Y-%m-%d")

cps_agg$monthyear <- paste(cps_agg$cpsyear,cps_agg$cpsmonth,sep="-")



panel <- pdata.frame(cps_zillow,index=c("county","date"))  
panel$change <- (panel$zillow_index - lag(panel$zillow_index))/ lag(panel$zillow_index)
panel$lagchange <- (lag(panel$zillow_index) - lag(panel$zillow_index,2))/ lag(panel$zillow_index,2)


## 

##package zoo for easy volatility calculation 





if (i >= 4 && df$id[i] == df$id[i-3]) {
  # Get the last 4 months' returns
  last_4_returns <- panel_data$return[(i-3):i]
  
  # Calculate the standard deviation (volatility)
  df$volatility_4m[i] <- sd(last_4_returns)
}
}

## we only have data past 2007

basic_reg  <- feols(mean_cohab ~ change | county + cpsyear, data = subset(panel,cpsyear>=2007))
summary(basic_reg)


basic_reg  <- feols(mean_cohab ~ lagchange | county + cpsyear, data = subset(panel,cpsyear>=2007))
summary(basic_reg)


basic_reg  <- feols(mean_cohab ~ log(lag(zillow_index)) | county + cpsyear, data = subset(panel,cpsyear>=2007))
summary(basic_reg)


