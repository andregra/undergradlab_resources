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
library(zoo)

path <- "C:/Users/andre/Dropbox/ucsd_classes/research/cohabit_housing"
overleaf <- "C:/Users/andre/Dropbox/Apps/Overleaf/Cohabitation and House Prices"

county_panel <- read_dta(paste(path,"/output/relate_panel.dta",sep=""))
metro_panel <- read_dta(paste(path,"/output/relatemetro_panel.dta",sep=""))


# Compute average value per time point
trends <- county_panel %>%
  dplyr::group_by(as.Date(zillow_date)) %>%
  filter(is.finite(cohab_change)) %>%
  dplyr::summarize(avg_cohab = weighted.mean(cohab,count, na.rm=TRUE),
                   avg_room=weighted.mean(roommate,count, na.rm=TRUE),
                   avg_wparent=weighted.mean(wparent,count, na.rm=TRUE),
                   avg_cohabchange=weighted.mean(cohab_change,count, na.rm=TRUE),
                   avg_hhchange = weighted.mean(z_change,count, na.rm=TRUE),
                   avg_agediff = weighted.mean(age_diff,count, na.rm=TRUE),
                   avg_eddiff = weighted.mean(ed_diff,count, na.rm=TRUE),
  )

##----------------------------------------------
#cbsatitle=="New York-Newark-Jersey City, NY-NJ"
##----------------------------------------------
trends_big <- subset(county_panel,SizeRank<100) %>%
  dplyr::group_by(as.Date(zillow_date)) %>%
  filter(is.finite(cohab_change)) %>%
  dplyr::summarize(avg_cohab = weighted.mean(cohab,count, na.rm=TRUE),
                   avg_room=weighted.mean(roommate,count, na.rm=TRUE),
                   avg_wparent=weighted.mean(wparent,count, na.rm=TRUE),
                   avg_cohabchange=weighted.mean(cohab_change,count, na.rm=TRUE),
                   avg_hhchange = weighted.mean(z_change,count, na.rm=TRUE),
                   avg_agediff = weighted.mean(age_diff,count, na.rm=TRUE),
                   avg_eddiff = weighted.mean(ed_diff,count, na.rm=TRUE),
  )

trends_nyc <- subset(county_panel,cbsatitle=="New York-Newark-Jersey City, NY-NJ") %>%
  dplyr::group_by(month=as.Date(zillow_date)) %>%
  filter(is.finite(cohab_change)) %>%
  dplyr::summarize(avg_cohab = weighted.mean(cohab,count, na.rm=TRUE),
                   avg_room=weighted.mean(roommate,count, na.rm=TRUE),
                   avg_wparent=weighted.mean(wparent,count, na.rm=TRUE),
                   avg_cohabchange=weighted.mean(cohab_change,count, na.rm=TRUE),
                   avg_hhchange = weighted.mean(z_change,count, na.rm=TRUE),
                   avg_agediff = weighted.mean(age_diff,count, na.rm=TRUE),
                   avg_eddiff = weighted.mean(ed_diff,count, na.rm=TRUE),
  )

trends_nyc <- trends_nyc %>% arrange(month)

# Step 3: Create 3-month rolling averages
trends_nyc <- trends_nyc %>%
  mutate(
    roll3_cohab = rollapply(avg_cohab, width = 3, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    roll3_hhchange = rollapply(avg_hhchange, width = 3, FUN = mean, align = "right", fill = NA, na.rm = TRUE)
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
ggplot(trends, aes(x = `as.Date(zillow_date)`)) +
  geom_line(aes(y = avg_eddiff), color = "blue", size = 1) +
  labs(title = "Match Quality over Time", x = "Time", y = "Value") +
  theme_minimal()


##----------------------------------------
## Cohabitation Trends over Time
##---------------------------------------
trends_long <- trends %>%
  pivot_longer(cols = c(avg_room, avg_cohab),
               names_to = "category",
               values_to = "value")

# 2. Plot with better theming and legend
ggplot(trends_long, aes(x = `as.Date(zillow_date)`, y = value, color = category)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("avg_room" = "steelblue", "avg_cohab" = "tomato"),
    labels = c("Cohabiting", "Roommates")
  ) +
  labs(
    title = "Household Organization Over Time",
    x = "Date",
    y = "Proportion of Households",
    color = "Household Type"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "12 months") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

ggsave(paste(overleaf,"/houseorg_trends.png",sep=""), plot = last_plot(), 
       width = 8, height = 5, dpi = 300, units = "in")


## structural shocks? look just at big places
## print this graph 


ggplot(trends_nyc, aes(x = month)) +
  geom_line(aes(y = roll3_cohab), color = "red", size = 1) +
  geom_line(aes(y = roll3_hhchange), color = "blue", size = 1) +
  labs(title = "Household Organization Over Time", x = "Month-Year", y = "Proportion") +
  theme_minimal()

trends_long <- trends_nyc %>%
  pivot_longer(cols = c(roll3_cohab, roll3_hhchange),
               names_to = "category",
               values_to = "value")

# 2. Plot with better theming and legend
ggplot(trends_long, aes(x = month, y = value, color = category)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("roll3_hhchange" = "steelblue", "roll3_cohab" = "tomato"),
    labels = c("Cohabiting","Zillow MoM HPI Change")
  ) +
  labs(
    title = "Cohabiting and Prices in NYC",
    x = "Date",
    y = "3-month Smoothed Proportion",
    color = ""
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "12 months") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

ggsave(paste(overleaf,"/housenyc_trends.png",sep=""), plot = last_plot(), 
       width = 8, height = 5, dpi = 300, units = "in")


ggplot(trends_big, aes(x = `as.Date(zillow_date)`)) +
  geom_line(aes(y = avg_cohabchange), color = "blue", size = 1) +
  geom_line(aes(y = avg_hhchange), color = "red", size = 1) +
  labs(title = "Cohabitation and HH Price Change Over Time", x = "Time", y = "Value") +
  theme_minimal()