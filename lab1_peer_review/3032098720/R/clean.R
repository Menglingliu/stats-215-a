library(plyr)
library(dplyr)

# remove entries with missing data
net = na.omit(net)
log = na.omit(log)

# voltage inconsistency
net$voltage = net$voltage / 100

# keep entries with voltage between 2.4V and 3V and humidity between 0 and 100
net = net[between(net$voltage,2.4, 3) & between(net$humidity,0,100),]
log = log[between(log$voltage,2.4, 3) & between(log$humidity,0,100),]

# remove entries with depth >50, nodeid = 65535, parent = 65535
net = net[net$depth <=50 & net$nodeid != 65535 & net$parent != 65535,]
log = log[log$depth <=50 & log$nodeid != 65535 & log$parent != 65535,]

# remove entries with temperature reading above 60
net = net[net$humid_temp <= 60,]
log = log[log$humid_temp <= 60,]

# remove duplicates using epoch to represent time
net = distinct(net,epoch, nodeid, .keep_all = TRUE)
log = distinct(log,epoch, nodeid, .keep_all = TRUE)

# combine cleaned datasets, add label
net$label = 'net'
log$label = 'log'
all_new <- rbind(net,log)

# join location onto cleaned dataset
colnames(mote_loc)[1] <- 'nodeid'
all_new_loc <- join(all_new, mote_loc, by = 'nodeid')

# only keep 'edge', height below 40
all_subset <- all_new_loc[all_new_loc$Tree == 'edge' & all_new_loc$Height <= 40, ]