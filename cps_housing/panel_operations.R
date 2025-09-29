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

path <- "C:/Users/andre/Dropbox/ucsd_classes/research/cohabit_housing"

zillow <- read_csv(paste0(path,"/data/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month (1).csv",sep=""))  

##clean county fips for easier matching to cps
zillow$county_fips <- paste(zillow$StateCodeFIPS,zillow$MunicipalCodeFIPS,sep="")
zillow$county_fips_num <- as.numeric(zillow$county_fips)

##change data from wide to long format
zillow_long <- pivot_longer(
  zillow,
  cols = starts_with("2"), # Specify the columns to reshape
  names_to = "zillow_date",          # Name for the variable column
  values_to = "zillow_index"             # Name for the value column
) 

##clean date information -- need to make the dates into R date format
zillow_long$zillow_dateformat <- as.Date(zillow_long$zillow_date, format = "%Y-%m-%d")
zillow_long$zillowmonth <- as.numeric(format(zillow_long$zillow_dateformat, "%m"))
zillow_long$zillowyear <- as.numeric(format(zillow_long$zillow_dateformat, "%Y"))

##if I want to do all my analysis at a month-year level, I need a month-year variable, ignoring day of month
zillow_long$year_month <- as.numeric(format(zillow_long$zillow_dateformat, "%Y%m"))


##------------------------
## create panel using the plm package
##--------------------------

## this panel is at the year month level , but use the full date variable "zillow date format"
panel <- pdata.frame(zillow_long,index=c("county_fips","zillow_dateformat"))  

## we define change from the last month, or from the last two months (lag2)
panel$change <- (panel$zillow_index - lag(panel$zillow_index))/ lag(panel$zillow_index)
panel$lagchange <- (lag(panel$zillow_index) - lag(panel$zillow_index,2))/ lag(panel$zillow_index,2)
panel$logchange <- (log(panel$zillow_index) - log(lag(panel$zillow_index)))

### produce a panel at the yearly level
##first aggregate zillow_long to the year

zillow_yearlong <- zillow_long %>%
  group_by(zillowyear, county_fips) %>%
  summarize(mean_price = mean(zillow_index))


##yearly panel
yearpanel <- pdata.frame(zillow_yearlong,index=c("county_fips","zillowyear"))  

## define a 5-year change, annualized (annualized just means the percent change divided by num of years)
yearpanel$change5year_annualized <- ((yearpanel$mean_price - lag(yearpanel$mean_price,5))/ lag(yearpanel$mean_price,5)) / 5


##----------------------
## Volatility 
##----------------------
## use zoo to calculate a month-month volatility in price change, defined as the standard deviation in #monthly price changes
library(zoo)

##define window size for rolling standard deviation calc
window_size <- 5

# Compute rolling standard deviation within each panel group, applying this to the "% change variable"
panel <- panel %>%
  group_by(county_fips) %>%
  mutate(rolling_sd = rollapply(change, width = window_size, FUN = sd, fill = NA, align = "right"),
         rolling_mean = rollapply(change, width = window_size, FUN = mean, fill = NA, align = "right"))


##--------------------------------
## graph trends example
##-----------------------------------
## graphs for new york county
new_york <- subset(panel, county_fips==36061)

new_york %>%
  ggplot( aes(x=zillow_dateformat, y=change)) +
  geom_line()

new_york$date <- as.Date(new_york$zillow_dateformat)

## prices (mean)

ggplot(new_york, aes(x = date)) +
  geom_line(aes(y = change, color = "House Price MoM", group = county_fips), size = 1) +
  geom_line(aes(y = rolling_mean, color = "Rolling Mean House Price", group = county_fips), size = 1, linetype = "dashed") +
  labs(title = "Panel Data Line Graph",
       x = "Month-Year",
       y = "% Change",
       color = "Variables") +
  scale_x_date(
    breaks = as.Date(c("2010-01-01", "2013-01-01", "2016-01-01", "2019-01-01")),  # Show only these dates
    date_labels = "%Y-%m"  # Format labels to show only the year-month
  ) +  theme_minimal()

## volatility 
ggplot(new_york, aes(x = date)) +
  geom_line(aes(y = change, color = "House Price Change", group = county_fips), size = 1) +
  geom_line(aes(y = rolling_sd, color = "Volatility", group = county_fips), size = 1) +
  labs(title = "Panel Data Line Graph",
       x = "Month-Year",
       y = "",
       color = "Variables") +
  scale_x_date(
    breaks = as.Date(c("2010-01-01", "2013-01-01", "2016-01-01", "2019-01-01")),  # Show only these dates
    date_labels = "%Y-%m"  # Format labels to show only the year-month
  ) +  theme_minimal()


