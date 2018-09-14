library(ggplot2)
library(tidyverse)
library(lubridate)
library(viridis)

setwd("C:/Users/fando/Box Sync/stat-215-a/lab 1")
dat <- read.csv("data/sonoma-data-all.csv", header = TRUE)
loc <- read.table('data/mote-location-data.txt', header = TRUE)


# Date --------------------------------------------------------------------
# Code in this section is taken from R/load.R.
# make sure to run the command in 'shell command date.txt' in terminal before
# running the code below
path <- "data/"
# load the numbers data
epoch_nums <- read.table(paste0(path, "sonoma-dates-epochNums.txt"), 
                         col.names=NA,
                         colClasses = "character")
# remove the surplus rows
epoch_nums <- epoch_nums[3:(nrow(epoch_nums) - 1), ]
# manually input the first entry
epoch_nums[1] <- "1"

# load in the dates data
epoch_dates <- read.table(paste0(path, "sonoma-dates-epochDates.txt"), 
                          col.names=NA, 
                          colClasses="character")
# the first entry was read incorrectly. 
# remove the surplus rows
epoch_dates <- epoch_dates[7:(nrow(epoch_dates) - 1), ]
# manually input the first entry
epoch_dates[1] <- "Tue Apr 27 17:10:00 2004"

# load in the days data
epoch_days <- read.table(paste0(path, "sonoma-dates-epochDays.txt"), 
                         col.names=NA,
                         colClasses = "character")
# the first entry was read incorrectly. 
# remove the surplus rows
epoch_days <- epoch_days[3:(nrow(epoch_days) - 1), ]
# manually input the first entry
epoch_days[1] <- "12536.0069444444"

# combine all three variables into a data frame
epoch_df <- data.frame(number = epoch_nums,
                       date = epoch_dates,
                       day = epoch_days)

cleanDatesData <- function(date_df) {
  # Arguments:
  #   date_df: a data.frame in the format of the output of the 
  #     loadDatesData() function
  # Returns:
  #   a data.frame similar to the input `dates` but with cleaned variables
  #     (number, day, date, time, datetime)
  
  # convert the dates variable to lubridate format
  date_df <- date_df %>% 
    # separate date variable into two variables: time and date
    # remove times
    mutate(date_sep = gsub("\\w+:\\w+:\\w+ ", "", date), 
           # remove day of the week
           date_sep = gsub("(Mon|Tue|Wed|Thu|Fri|Sat|Sun)", "", date_sep),
           # extract times
           time_sep = str_extract(as.character(date), " \\w+:\\w+:\\w+"),
           # combine date and time into single datetime variable
           datetime = mdy_hms(paste(date_sep, time_sep)),
           # convert day to a number
           day = as.numeric(as.character(day))) %>%
    # remove original date vairable and rename date_sep and time_sep
    select(-date, date = date_sep, time = time_sep)
  
  return(date_df)
}

epoch_df <- cleanDatesData(epoch_df)
epoch_df$number <- as.numeric(epoch_df$number)

# Merge datasets ----------------------------------------------------------

dat <- dat %>% 
  inner_join(loc, by = c('nodeid' = 'ID')) %>% 
  inner_join(epoch_df, by = c('epoch' = 'number')) %>% 
  select(-result_time)

dat <- dat[!duplicated(dat), ]

vars <- c('humid_adj', 'humid_temp', 'hamatop', 'hamabot')

summary(dat[vars])

# humid_adj ---------------------------------------------------------------
# I. Negative Values
neg.humid.node <- filter(dat, humid_adj < 0) %>% 
                    group_by(nodeid) %>% 
                    summarise(n=n())
print(neg.humid.node[['nodeid']])

## node 29, 123, 141, 78, 198
# node 29
node29 <- filter(dat, nodeid == 29)
summary(node29[vars]) 
# NA 1: drop humid_adj and humid_temp since all obs have same values

# node 123
node123 <- filter(dat, nodeid == 123)
summary(node123[vars]) # seems normal
# Take a closer look and plot node 123 values against time
# Plots of humidity and temperature show the same trend:
# apparently something went wrong when epoch > 4900
# Let's make those values null (NA 2)
ggplot(node123)+geom_point(aes(x=epoch, y=humid_adj))
ggplot(node123)+geom_point(aes(x=epoch, y=humid_temp))
ggplot(node123)+geom_point(aes(x=epoch, y=hamatop))
ggplot(node123)+geom_point(aes(x=epoch, y=hamabot))

# node 141
node141 <- filter(dat, nodeid == 141)
summary(node141[vars])
# Take a closer look and plot node 141 values against time
# Plots of humidity and temperature show the same trend:
# apparently something went wrong when epoch > 8900
# Let's make those values null (NA 3)
ggplot(node141)+geom_point(aes(x=epoch, y=humid_adj)) + geom_vline(xintercept=8900)
ggplot(node141)+geom_point(aes(x=epoch, y=humid_temp))+ geom_vline(xintercept=8900)
ggplot(node141)+geom_point(aes(x=epoch, y=hamatop))
ggplot(node141)+geom_point(aes(x=epoch, y=hamabot))

# node 198
node198 <- filter(dat, nodeid == 198)
summary(node198[vars])
# Take a closer look and plot node 198 values against time
# We found one outlier: all columns are out of normal range when epoch == 3472
# Let's make the epoch == 3472 point null (NA 4)
ggplot(node198)+geom_point(aes(x=epoch, y=humid_adj))
ggplot(node198)+geom_point(aes(x=epoch, y=humid_temp))
ggplot(node198)+geom_point(aes(x=epoch, y=hamatop))
ggplot(node198)+geom_point(aes(x=epoch, y=hamabot))

# node 78
node78 <- filter(dat, nodeid == 78)
summary(node78[vars])
# Take a closer look and plot node 78 values against time
# humid_adj went wrong when epoch > 2800
# temperature went wrong when epoch > 2950
# Let's make those values null (NA 5)
ggplot(node78)+geom_point(aes(x=epoch, y=humid_adj)) + geom_vline(xintercept=2800)
ggplot(node78)+geom_point(aes(x=epoch, y=humid_temp)) + geom_vline(xintercept=2950)
ggplot(node78)+geom_point(aes(x=epoch, y=hamatop))
ggplot(node78)+geom_point(aes(x=epoch, y=hamabot))

# Implement NA 1 - NA 5 and make all invalid points NAs
dat <- dat %>%
  mutate(humid_adj = if_else((nodeid == 29) | ((nodeid == 123) & (epoch > 4900)) |
                             ((nodeid == 141) & (epoch > 8900)) | ((nodeid == 78) & (epoch > 2800))|
                               ((nodeid == 198) & (epoch == 3472)),
                             NA_real_, humid_adj),
         humid_temp = if_else((nodeid == 29) | ((nodeid == 123) & (epoch > 4900)) |
                               ((nodeid == 141) & (epoch > 8900)) | ((nodeid == 78) & (epoch > 2950)) |
                                ((nodeid == 198) & (epoch == 3472)),
                              NA_real_, humid_temp),
         hamatop = if_else((nodeid == 29) | ((nodeid == 198) & (epoch == 3472)), NA_real_, hamatop),
         hamabot = if_else((nodeid == 29) | ((nodeid == 198) & (epoch == 3472)), NA_real_, hamabot)) %>%
  as.data.frame()

# II. Values > 100
large.humid.node <- filter(dat, humid_adj > 100) %>% 
  group_by(nodeid) %>% 
  summarise(n=n())
print(large.humid.node[['nodeid']])

## node 42, 145, 3, 118
# node 42
node42 <- filter(dat, nodeid == 42)
summary(node42[vars])
# all values > 100 are only greater at 0.00 level, no need to change anything.

# node 145
node145 <- filter(dat, nodeid == 145)
summary(node145[vars])
ggplot(node145)+geom_point(aes(x=epoch, y=humid_adj)) + geom_vline(xintercept=3150)
ggplot(node145)+geom_point(aes(x=epoch, y=humid_temp)) + geom_vline(xintercept=3150)
ggplot(node145)+geom_point(aes(x=epoch, y=hamatop))
ggplot(node145)+geom_point(aes(x=epoch, y=hamabot))
# Take a closer look and plot node 145 values against time
# Plots of humidity and temperature show the same trend:
# apparently something went wrong when epoch > 3150
# Let's make those values null (NA 1)

# node 3
node3 <- filter(dat, nodeid == 3)
summary(node3[vars])
ggplot(node3)+geom_point(aes(x=epoch, y=humid_adj)) + geom_vline(xintercept=3750)
ggplot(node3)+geom_point(aes(x=epoch, y=humid_temp)) + geom_vline(xintercept=3700)
ggplot(node3)+geom_point(aes(x=epoch, y=hamatop))
ggplot(node3)+geom_point(aes(x=epoch, y=hamabot))
# Take a closer look and plot node 3 values against time
# humid_adj went wrong when epoch > 3750
# temperature went wrong when epoch > 3700
# Let's make those values null (NA 2)

# node 118
node118 <- filter(dat, nodeid == 118)
summary(node118[vars])
ggplot(node118)+geom_point(aes(x=epoch, y=humid_adj))
ggplot(node118)+geom_point(aes(x=epoch, y=humid_temp))
ggplot(node118)+geom_point(aes(x=epoch, y=hamatop))
ggplot(node118)+geom_point(aes(x=epoch, y=hamabot))
# Take a closer look and plot node 118 values against time
# humid_adj went wrong when epoch = 8717, 8718, 8719; let's make them null (NA 3)
# other variables seem normal

# Implement NA 1 - NA 3 and make all invalid points NAs
dat <- dat %>%
  mutate(humid_adj = if_else(((nodeid == 145) & (epoch > 3150)) |
                             ((nodeid == 3) & (epoch > 3750)) |
                             ((nodeid == 118) & (epoch %in% c(8717, 8718, 8719))),
                             NA_real_, humid_adj),
         humid_temp = if_else(((nodeid == 145) & (epoch > 3150)) |
                               ((nodeid == 3) & (epoch > 3700)),
                             NA_real_, humid_temp)) %>%
  as.data.frame()


# Temperature -------------------------------------------------------------

ggplot(dat)+geom_point(aes(x=epoch, y=humid_temp))
ggplot(dat)+geom_point(aes(x=Height, y=humid_temp))
ggplot(dat)+geom_point(aes(x=voltage, y=humid_temp))

# all seems normal


# hamatop -----------------------------------------------------------------

ggplot(dat)+geom_point(aes(x=epoch, y=hamatop))
# we can see a group of suspicious outliers when hamatop > 150000

large.hamatop.node <- filter(dat, hamatop > 150000) %>% 
  group_by(nodeid) %>% 
  summarise(n=n())
print(large.hamatop.node[['nodeid']])

# node 40
node40 <- filter(dat, nodeid == 40)
summary(node40[vars])
ggplot(node40)+geom_point(aes(x=epoch, y=humid_adj))
ggplot(node40)+geom_point(aes(x=epoch, y=humid_temp))
ggplot(node40)+geom_point(aes(x=epoch, y=hamatop))
ggplot(node40)+geom_point(aes(x=epoch, y=hamabot))
# humid_adj, humid_temp and hamabot seem normal, 
# but hamatop has some strangely high values that do not 
# go along with the general pattern, especially when epoch > 38
# Let's make all the outliers null
dat <- dat %>%
  mutate(hamatop = if_else(((nodeid == 40) & (hamatop > 150000)),
                             NA_real_, hamatop)) %>%
  as.data.frame()


# hamabot -----------------------------------------------------------------

ggplot(dat)+geom_point(aes(x=epoch, y=hamabot))
ggplot(dat)+geom_point(aes(x=Height, y=hamabot))
ggplot(dat)+geom_point(aes(x=voltage, y=hamabot))

# Voltage -----------------------------------------------------------------

hist(dat$voltage)
# in log, voltage is within 2~3 range
# in net, voltage is aroung 180 ~ 300 range
# the small group of voltage > 1000 is very weird

high.voltage.node <- filter(dat, voltage > 1000) %>% 
  group_by(nodeid) %>% 
  summarise(n=n())
print(high.voltage.node[['nodeid']])

## node 134, 141, 145
# node 134
node134 <- filter(dat, nodeid == 134)
hist(node134$voltage)
ggplot(node134)+geom_point(aes(x=voltage, y=humid_adj))
ggplot(node134)+geom_point(aes(x=voltage, y=humid_temp))
ggplot(node134)+geom_point(aes(x=voltage, y=hamatop))
ggplot(node134)+geom_point(aes(x=voltage, y=hamabot))
# Albeit the outliers have strangely high voltage, their humidity, temp, hamatop
# and hamabot seem normal. Let's just make the high voltage values null in this case.

# node 141
node141 <- filter(dat, nodeid == 141)
hist(node141$voltage)
ggplot(node141)+geom_point(aes(x=voltage, y=humid_adj))
ggplot(node141)+geom_point(aes(x=voltage, y=humid_temp))
ggplot(node141)+geom_point(aes(x=voltage, y=hamatop))
ggplot(node141)+geom_point(aes(x=voltage, y=hamabot))
# Albeit the outliers have strangely high voltage, their humidity, temp, hamatop
# and hamabot seem normal. Let's just make the high voltage values null in this case.

# node 145
node145 <- filter(dat, nodeid == 145)
hist(node145$voltage)
ggplot(node145)+geom_point(aes(x=voltage, y=humid_adj))
ggplot(node145)+geom_point(aes(x=voltage, y=humid_temp))
ggplot(node145)+geom_point(aes(x=voltage, y=hamatop))
ggplot(node145)+geom_point(aes(x=voltage, y=hamabot))
# Albeit the outliers have strangely high voltage, their humidity, temp, hamatop
# and hamabot seem normal. Let's just make the high voltage values null in this case.

low.voltage.node <- filter(dat, voltage < 2) %>% 
  group_by(nodeid) %>% 
  summarise(n=n())
print(low.voltage.node[['nodeid']])

dat <- dat %>%
  mutate(voltage = if_else(((nodeid %in% c(134, 141, 145)) & (voltage > 1000)) |
                           ((nodeid %in% c(29, 128, 134, 141, 142, 143, 145)) & (voltage < 2)),
                           NA_real_, voltage)) %>%
  as.data.frame()


# One value per epoch-nodeid pair -----------------------------------------

# we can use voltage to distinguish net and log values
# in log, voltage is within 2~3 range
# in net, voltage is aroung 200 ~ 300 range

dat$id <- paste(dat$epoch, dat$nodeid, sep = '-')
log <- filter(dat, voltage < 100)
net <- filter(dat, voltage > 100)

log <- filter(log, !(log$id %in% log[duplicated(log$id), 'id']))
net <- filter(net, !(net$id %in% net[duplicated(net$id), 'id']))

# adjust voltage in net so it is in the same scale as log
voltage.net.log <- full_join(log, net, by = 'id', suffix = c('.log', '.net')) %>%
  filter((voltage.log != 0) & (voltage.net != 0)) %>% 
  select(voltage.log, voltage.net)
fit.vol <- lm(voltage.log ~ voltage.net, data = voltage.net.log)
summary(fit.vol)
ggplot(voltage.net.log)+geom_point(aes(x=voltage.log, y=predict(fit.vol)))

net$voltage_adj <- predict(fit.vol, data.frame(voltage.net = net$voltage))
log$voltage_adj <- log$voltage

dat.all <- data.frame(net)
extra.log <- filter(log, !(log$id %in% net$id))

dat.all <- rbind(dat.all, extra.log)

# More Prep
dat.all$hour <- format(strptime(dat.all$time,format = '%H:%M:%S'), "%H")

# Outlier Rejection -------------------------------------------------------

par(mfrow = c(2,2))
ggplot(dat.all) +
  geom_point(aes(x=voltage_adj, y=humid_temp)) + 
    geom_vline(xintercept=2.4, color = 'red')
ggplot(dat.all)+
  geom_point(aes(x=voltage_adj, y=humid_adj)) + 
    geom_vline(xintercept=2.4, color = 'red')
ggplot(dat.all)+
  geom_point(aes(x=voltage_adj, y=hamatop)) + 
    geom_vline(xintercept=2.4, color = 'red')
ggplot(dat.all)+
  geom_point(aes(x=voltage_adj, y=hamabot)) + 
    geom_vline(xintercept=2.4, color = 'red')


# Data Exploration --------------------------------------------------------

dat.all %>% 
  mutate(bins=cut(Height, breaks=seq(10, 70, by = 10))) %>% 
  {ggplot(.,aes(x=epoch, fill=bins)) +
      geom_histogram(binwidth=3)+
      theme_minimal() +
      labs(fill='height') +
      ggtitle("Histogram, by epoch and height") +
      theme(
        plot.title = element_text(color="black", size=14, hjust = 0.5),
        axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12)
      ) +
      scale_fill_manual(drop=FALSE,values = c(viridis(nlevels(.$bins)), 
                                              viridis(nlevels(.$bins), direction = -1)))}

# Graph 1 -----------------------------------------------------------------

may20 <- filter(dat.all, (date == " May 20 2004") & (humid_temp < 25)) %>% 
  mutate(height_cat = cut(Height, breaks = seq(10, 70, by = 15)),
         hour = as.numeric(hour) + 1,
         hour_str = if_else((7 < hour) & (hour < 19), 'day', 'night'),
         hour_cat = cut(hour, breaks = seq(0, 24, by = 4)),
         direc_EW = if_else(Direc %in% c('E', 'ESE', 'NE'), 'E', 'W'))

ggplot(may20) +
  geom_point(aes(x=hour, y=humid_temp, color = Height), size = 3) +
  scale_colour_gradient(low = "#132B43", high = "#b0dbfc") +
  theme_minimal() +
  labs(fill='Height') +
  ggtitle("Temperature by hour") +
  ylab('Temperature') +
  xlab('Hour') +
  theme(
    plot.title = element_text(color="black", size=18, hjust = 0.5),
    axis.title.x = element_text(color="black", size=14),
    axis.title.y = element_text(color="black", size=14)
  )

ggplot(may20) +
  geom_point(aes(x=hour, y=humid_adj, color = Height), size = 3) +
  scale_colour_gradient(low = "#132B43", high = "#b0dbfc") +
  theme_minimal() +
  labs(fill='Height') +
  ggtitle("Humidity by hour") +
  ylab('Humidity') +
  xlab('Hour') +
  theme(
    plot.title = element_text(color="black", size=18, hjust = 0.5),
    axis.title.x = element_text(color="black", size=14),
    axis.title.y = element_text(color="black", size=14)
  )

ggplot(may20) +
  geom_boxplot(aes(x=hour_cat, y=humid_adj, fill = hour_cat)) +
  theme_light() +
  scale_fill_brewer(palette="Blues") +
  facet_grid(.~height_cat, switch = 'x') +
  labs(fill='Hour') +
  ggtitle("Distribution of Humidity, by Height and Hour") +
  ylab('Humidity') +
  xlab('Height Bin') +
  theme(
    plot.title = element_text(color="black", size=18, hjust = 0.5),
    axis.title.x = element_text(color="black", size=14),
    axis.title.y = element_text(color="black", size=14),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_text(face = 'bold', size = 12),
    legend.title=element_text(size=12),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(colour = 'black', face = 'bold', size = 12)
  ) 

ggplot(may20) +
  geom_point(aes(x=hour, y=hamatop, color = Height), size = 3) +
  scale_colour_gradient(low = "#132B43", high = "#b0dbfc") +
  theme_minimal() +
  labs(fill='Height') +
  ggtitle("Incident PAR by hour") +
  ylab('Incident PAR') +
  xlab('Hour') +
  theme(
    plot.title = element_text(color="black", size=18, hjust = 0.5),
    axis.title.x = element_text(color="black", size=14),
    axis.title.y = element_text(color="black", size=14)
  )

# Graph 2 -----------------------------------------------------------------

may20.height.node <- may20 %>% 
  group_by(Height, nodeid, Tree) %>% 
  summarise(n=n())

may20.4nodes <- filter(may20, nodeid %in% c(c(80, 14, 46, 110, 42, 113)))

may20.4nodes$nodeid = factor(may20.4nodes$nodeid, levels=c(80, 14, 46, 110, 42, 113))
levels(may20.4nodes$nodeid) <- paste('Node', c(80, 14, 46, 110, 42, 113))

ggplot(may20.4nodes, aes(x=humid_adj, y=humid_temp)) +
  geom_point(aes(colour = Height), size = 3) +
  geom_smooth(method="lm", se=FALSE, size = 2, color = 'orangered') +
  facet_grid(.~nodeid) +
  theme_light() +
  ggtitle("Humidity vs. Temperature over 1 Day, by Height") +
  ylab('Temperature') +
  xlab('Humidity') +
  theme(
    plot.title = element_text(color="black", size=18, hjust = 0.5),
    axis.title.x = element_text(color="black", size=12),
    axis.title.y = element_text(color="black", size=12),
    strip.text.x = element_text(size = 11)
  )


# Graph 3 -----------------------------------------------------------------


ggplot(may20.4nodes, aes(x=humid_temp, y=hamatop)) +
  geom_point(aes(colour = Height), size = 3) +
  geom_smooth(method="lm", se=FALSE, size = 2, color = 'orangered') +
  facet_grid(height_cat~.) +
  theme_light() +
  ggtitle("Incident PAR vs. Temperature over 1 Day, by Height Bin") +
  ylab('Incident PAR') +
  xlab('Temperature') +
  theme(
    plot.title = element_text(color="black", size=18, hjust = 0.5),
    axis.title.x = element_text(color="black", size=12),
    axis.title.y = element_text(color="black", size=12),
    strip.text.y = element_text(size = 11)
  )


# Graph 4 -----------------------------------------------------------------

ggplot(dat.all, aes(x=hour, y=humid_temp)) +
  geom_point(aes(colour = Height), size = 3) +
  geom_hline(yintercept=27.5, color = 'orangered', size = 2) +
  facet_wrap(~Tree) +
  theme_light() +
  ggtitle("Temperature over 1 Day, by Tree") +
  ylab('Temperature') +
  xlab('Hour') +
  theme(
    plot.title = element_text(color="black", size=18, hjust = 0.5),
    axis.title.x = element_text(color="black", size=12),
    axis.title.y = element_text(color="black", size=12),
    strip.text.y = element_text(size = 11),
    strip.text = element_text(colour = 'white', face = 'bold', size = 12)
  )


# explore -----------------------------------------------------------------

# additional exploratory plots. See the jpeg plots in R/additional plots.

ggplot(may20) +
  geom_point(aes(x=hour, y=hamabot, color = Height), size = 3) +
  scale_colour_gradient(low = "#132B43", high = "#b0dbfc") +
  theme_minimal() +
  labs(fill='Height') +
  ggtitle("Reflected PAR by hour") +
  ylab('Reflected PAR') +
  xlab('Hour') +
  theme(
    plot.title = element_text(color="black", size=18, hjust = 0.5),
    axis.title.x = element_text(color="black", size=14),
    axis.title.y = element_text(color="black", size=14)
  ) 


ggplot(may20[may20$height_cat %in% c('(25,40]', '(40,55]', '(55,70]'), ]) +
  geom_boxplot(aes(x=height_cat, y=hamabot, fill = as.factor(hour))) +
  theme_minimal() +
  labs(fill='Hour') +
  ggtitle("Distribution of Reflected PAR, by Height and Hour") +
  ylab('Reflected PAR') +
  xlab('Height Bin') +
  theme(
    plot.title = element_text(color="black", size=18, hjust = 0.5),
    axis.title.x = element_text(color="black", size=14),
    axis.title.y = element_text(color="black", size=14),
    axis.text.x=element_text(face = 'bold', size = 12),
    axis.text.y=element_text(face = 'bold', size = 12),
    legend.title=element_text(size=12)
  ) 

