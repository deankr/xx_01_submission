library(data.table)
library(dplyr)
library(tidyr)
library(surveillance)
library(ggplot2)
library(forecast)
library(tseries)
library(openxlsx)


# You will need to change your working directory
setwd("/home/katie/xx_01_submission/xx_01-master")

fileSources = file.path("code", list.files("code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

#CreateFakeData()
d <- readRDS("data_raw/individual_level_data.RDS")

#6. Create a dataset that contains the aggregated number of sick people per day per municipality
d$value = as.numeric(gsub(" person", "", d$value))

d2 <- d %>%
  group_by(location, date) %>%
  summarize(value = sum(value))

#7. Ensure that your aggregated dataset includes rows/days with zero sick people
d3 <- d2 %>%
  group_by(location) %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"))

d3$value[is.na(d3$value)] <- 0

#8. Collapse your data down to iso-year/iso-weeks for each municipality
d4 <- d3 %>%
  mutate(date = paste(isoWeekYear(as.Date(date))$ISOYear, 
                      sep="-", 
                      sprintf("%02i", isoWeekYear(as.Date(date))$ISOWeek)))

d4 <- d4 %>%
  group_by(location, date) %>%
  summarize(value = sum(value))

saveRDS(d4, file="./data_clean/all_municp")

d4 <- readRDS("./data_clean/all_municp")
municip <- readRDS("./data_raw/norwayLocations.RDS")

for (i in unique(c(d4$location))[1:422]) {
  #9. Split the data into training data and production data
  dtrain <- data.frame()
  dprod <- data.frame()
  dtrain <- d4 %>%
    filter(substr(date, 1, 4) != 2010 & location == i)
  
  dprod <- d4 %>%
    filter(substr(date, 1, 4) == 2010 & location == i)
  
  #10. Use the training data to create a regression model
  
  #ggplot(dtrain, aes(date, value, group=1)) +
  #geom_point() +
  #geom_line() +
  #scale_x_discrete(breaks=c("2000-01","2001-01","2002-01","2003-01","2004-01","2005-01",
  #"2006-01","2007-01","2008-01","2009-01"))
  
  #adf.test(dtrain$value, alternative="stationary")
  #Acf(dtrain$value, main='')
  #Pacf(dtrain$value, main='')
  
  #implemented a seasonal model based on the code
  smodel <- auto.arima(ts(dtrain$value, frequency=52), seasonal=TRUE, allowdrift=FALSE, stepwise=TRUE, approximation=TRUE)
  
  #11. For the training data, create a 2 standard deviation prediction interval
  upper <- fitted(smodel) + 1.96*sqrt(smodel$sigma2)
  lower <- fitted(smodel) - 1.96*sqrt(smodel$sigma2)
  
  dtrain$timestep = seq(1, length(dtrain$value))
  dtrain$fitmean = smodel$fitted
  dtrain$upper <- upper
  dtrain$lower <- lower
  
  #12. Identify the potential outbreaks in the training data and 13. Exclude potential outbreaks from the dataset
  outbreaks <- data.frame(Week = character(), 
                          Cases = integer (), 
                          stringsAsFactors=FALSE)
  
  dtrain$value_no_ob <- dtrain$value
  
  id_outbreaks <- for (a in 1:length(dtrain$value)) 
    if (dtrain$value[a] > dtrain$upper[a]) {
      new_row <- data.frame(Week = dtrain$date[a], Cases = dtrain$value[a])
      outbreaks <- rbind(outbreaks, new_row)
      dtrain$value_no_ob[a] <- NA
    } 
  
  # 14. Refit the model using the new training data (without any outbreaks in it)
  smodel2 <- auto.arima(ts(dtrain$value_no_ob, frequency=52), D=1, seasonal=TRUE, allowdrift=FALSE, stepwise=TRUE, approximation=TRUE)
  
  # 15. Create a 2 standard deviation prediction interval for the production data
  smodel3 <- auto.arima(ts(dprod$value), seasonal=TRUE,allowdrift = FALSE, stepwise=TRUE, approximation=TRUE)
  
  upper <- fitted(smodel3) + 1.96*sqrt(smodel3$sigma2)
  lower <- fitted(smodel3) - 1.96*sqrt(smodel3$sigma2)
  
  dprod$timestep = seq(nrow(dtrain)+1, nrow(dtrain) + nrow(dprod))
  dprod$fitmean = smodel3$fitted
  dprod$upper = upper
  dprod$lower = lower
  
  # 16. Identify potential outbreaks in the production data
  dprod$value_no_ob <- dprod$value
  
  for (b in 1:length(dprod$value)) 
    if (dprod$value[b] > dprod$upper[b]) {
      new_row <- data.frame(Week = dprod$date[b], Cases = dprod$value[b])
      outbreaks <- rbind(outbreaks, new_row)
      dprod$value_no_ob[b] <- NA
    } 
  
  # 17. Create and save excel sheet with the potential outbreaks results/municipality_results/outbreaks_MunicipXXXX.xlsx 
  xl_wb <- createWorkbook()
  addWorksheet(xl_wb, sheetName = paste("outbreaks_", dtrain$location[1]))
  writeData(xl_wb, sheet = 1, outbreaks)
  saveWorkbook(xl_wb, paste("./results/municipality_results/", "outbreaks_", dtrain$location[1], ".xlsx"), overwrite=TRUE)
  
  # 18. Create and save a figure that provides a good overview of the situation in the municipality in 
  #results/municipality_results/overview_MunicipXXXX.png
  fcast_smodel3 <- forecast(smodel3, h = 12)
  all_years <- rbind(dtrain, dprod)
  
  
  for (c in 1:nrow(fcast_smodel3$lower)){
    new_row <- data.frame(location=dtrain[1,1], date = NA, value = NA, 
                          timestep = nrow(all_years)+1, fitmean = fcast_smodel3$mean[c], upper = fcast_smodel3$upper[c],
                          lower = fcast_smodel3$lower[c], value_no_ob = NA)
    all_years <- rbind(as.data.frame(all_years), new_row)
  }
  
  saveRDS(all_years, file=paste("./data_clean/all_years_",dtrain$location[1]))
  
  ggplot(all_years, aes(timestep)) +
    geom_ribbon(data=all_years,aes(ymin=lower,ymax=upper, fill = "Expected"), alpha=0.6) +
    geom_line(aes(x=timestep,y=fitmean, color="Predicted")) +
    geom_line(aes(x=timestep,y=value, color = "Observed")) +
    geom_ribbon(data=all_years, aes(ymin=upper, ymax=80, fill = "Higher than expected"), alpha=.8) +
    scale_y_continuous("Disease X Cases") +
    scale_x_continuous("Years", breaks = seq(1, length(all_years$timestep), 52), 
                       labels = c("01/2000", "01/2001", "01/2002", "01/2003",
                                  "01/2004", "01/2005", "01/2006", "01/2007",
                                  "01/2008", "01/2009", "01/2010", "01/2011")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_manual(values = c("Expected" = "steelblue1", "Higher than expected" = "lightsalmon1")) +
    scale_color_manual(values = c("Predicted" = "grey", "Observed" = "black")) +
    labs(title = paste(municip[which(municip$municip == i),][2]), color = "", fill = "") +
    ggsave(paste("./results/municipality_results/", "overview_", paste(dtrain$location[1]),".png"))
}

#Creative exercise
library(rgdal)

d5 <- d4
d5$municipName <- NA
d5$countyName <- NA

for (e in 1:nrow(d5)){
  names <- paste(municip[which(municip$municip == paste(d5[e,1])),])
  d5[e,4] <- names[2]
  d5[e,5] <- names[4]
}

county_numbers <- d5 %>%
  group_by(countyName) %>%
  summarise(count = n_distinct(municipName))

d5_wk52 <- d5 %>%
  filter(date == "2010-52") %>%
  group_by(countyName) %>%
  summarize(value = sum(value))

d5_wk52$incidence <- as.integer(unlist(d5_wk52$value/county_numbers[,2]))
d5_wk52[11,1] <- "Ãstfold"
d5_wk52[19,] <- c("Nord-Trøndelag", d5_wk52$value[16], d5_wk52$incidence[16])
d5_wk52[16,1] <- "Sør-Trøndelag"
d5_wk52[4,1] <- "Finnmark"
d5_wk52[7,1] <- "Møre og Romsdal"
d5_wk52[13,1] <- "Sogn og Fjordane"

colnames(d5_wk52) <- c("id", "value", "Incidence")

norway <-readOGR(dsn = "../xx_01-master/results/creative_assignment/Data/gadm36_NOR_1.shp", encoding = "UTF-8")
ggnorway <- fortify(norway, region = "NAME_1")

ggnorway_final <- merge(ggnorway, d5_wk52, by="id", all=TRUE)
ggnorway_final$Incidence <- as.integer(ggnorway_final$Incidence) 

wk52 <- ggplot(ggnorway_final) +
  geom_polygon(aes(long, lat, group=group, fill = Incidence)) +
  geom_path(color="black", aes(long,lat, group=group)) +
  theme_bw() + 
  scale_fill_continuous(low='#EEEEEE', high='red', limits = c(17,30)) +
  labs(title = "Disease X incidence (per 10,000) in Norway: 52/2010", fill = "Incidence per 10,000")
ggsave("./results/creative_assignment/wk_52.png")

##WEEK 51
d5_wk51 <- d5 %>%
  filter(date == "2010-51") %>%
  group_by(countyName) %>%
  summarize(value = sum(value))

d5_wk51$incidence <- as.integer(unlist(d5_wk51$value/county_numbers[,2]))
d5_wk51[11,1] <- "Ãstfold"
d5_wk51[19,] <- c("Nord-Trøndelag", d5_wk51$value[16], d5_wk51$incidence[16])
d5_wk51[16,1] <- "Sør-Trøndelag"
d5_wk51[4,1] <- "Finnmark"
d5_wk51[7,1] <- "Møre og Romsdal"
d5_wk51[13,1] <- "Sogn og Fjordane"
colnames(d5_wk51) <- c("id", "value", "Incidence")
ggnorway_final51 <- merge(ggnorway, d5_wk51, by="id", all=TRUE)
ggnorway_final51$Incidence <- as.integer(ggnorway_final51$Incidence)

wk51 <- ggplot(ggnorway_final51) +
  geom_polygon(aes(long, lat, group=group, fill = Incidence)) +
  geom_path(color="black", aes(long,lat, group=group)) +
  theme_bw() + 
  scale_fill_continuous(low='#EEEEEE', high='red', limits = c(17,30)) +
  labs(title = "Disease X incidence (per 10,000) in Norway: 51/2010", fill = "Incidence per 10,000")

ggsave("./results/creative_assignment/wk_51.png")

##WEEK50
d5_wk50 <- d5 %>%
  filter(date == "2010-50") %>%
  group_by(countyName) %>%
  summarize(value = sum(value))

d5_wk50$incidence <- as.integer(unlist(d5_wk50$value/county_numbers[,2]))
d5_wk50[11,1] <- "Ãstfold"
d5_wk50[19,] <- c("Nord-Trøndelag", d5_wk50$value[16], d5_wk50$incidence[16])
d5_wk50[16,1] <- "Sør-Trøndelag"
d5_wk50[4,1] <- "Finnmark"
d5_wk50[7,1] <- "Møre og Romsdal"
d5_wk50[13,1] <- "Sogn og Fjordane"
colnames(d5_wk50) <- c("id", "value", "Incidence")
ggnorway_final50 <- merge(ggnorway, d5_wk50, by="id", all=TRUE)
ggnorway_final50$Incidence <- as.integer(ggnorway_final50$Incidence)

wk50 <- ggplot(ggnorway_final50) +
  geom_polygon(aes(long, lat, group=group, fill = Incidence)) +
  geom_path(color="black", aes(long,lat, group=group)) +
  theme_bw() + 
  scale_fill_continuous(low='#EEEEEE', high='red', limits = c(17,30)) +
  labs(title = "Disease X incidence (per 10,000) in Norway: 50/2010", fill = "Incidence per 10,000")

ggsave("./results/creative_assignment/wk_50.png")

##WEEK49
d5_wk49 <- d5 %>%
  filter(date == "2010-49") %>%
  group_by(countyName) %>%
  summarize(value = sum(value))

d5_wk49$incidence <- as.integer(unlist(d5_wk49$value/county_numbers[,2]))
d5_wk49[11,1] <- "Ãstfold"
d5_wk49[19,] <- c("Nord-Trøndelag", d5_wk49$value[16], d5_wk49$incidence[16])
d5_wk49[16,1] <- "Sør-Trøndelag"
d5_wk49[4,1] <- "Finnmark"
d5_wk49[7,1] <- "Møre og Romsdal"
d5_wk49[13,1] <- "Sogn og Fjordane"
colnames(d5_wk49) <- c("id", "value", "Incidence")
ggnorway_final49 <- merge(ggnorway, d5_wk49, by="id", all=TRUE)
ggnorway_final49$Incidence <- as.integer(ggnorway_final49$Incidence)

wk49 <- ggplot(ggnorway_final49) +
  geom_polygon(aes(long, lat, group=group, fill = Incidence)) +
  geom_path(color="black", aes(long,lat, group=group)) +
  theme_bw() + 
  scale_fill_continuous(low='#EEEEEE', high='red', limits = c(17,30)) +
  labs(title = "Disease X incidence (per 10,000) in Norway: 49/2010", fill = "Incidence per 10,000")
ggsave("./results/creative_assignment/wk_49.png")
