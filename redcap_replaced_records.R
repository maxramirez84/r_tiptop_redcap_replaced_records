
# Read REDCap logging file (It must be manually downloaded from the REDCap 
# project. There's no API call to retrieve logs)
kLoggingFileName <- "TIPTOPHHSEndlineMozambique_Logging_2021-11-03_1457.csv"
logs <- read.csv(kLoggingFileName)
colnames(logs) <- c("datetime", "username", "action", "data")

# Keep only logs related to record creation through the API
kRecordCreatedPattern <- "Created Record (API)"
logs <- logs[which(startsWith(logs$action, kRecordCreatedPattern)), ]

# Keep only log entries which are duplicated on the Action column, i.e.
# records created more than once or replaced records
replaced.cond <- duplicated(logs$action) | duplicated(logs$action, fromLast = T)
replaced <- logs[replaced.cond, ]
replaced <- replaced[order(replaced$action, replaced$datetime), ]