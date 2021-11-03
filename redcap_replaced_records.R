
# Read REDCap logging file (It must be manually downloaded from the REDCap 
# project. There's no API call to retrieve logs)
kLoggingFileName <- "TIPTOPHHSEndlineMozambique_Logging_2021-11-03_1457.csv"
logs <- read.csv(kLoggingFileName)

# Keep only logs related to record creation through the API
kRecordCreatedPattern <- "Created Record (API)"
logs <- logs[which(startsWith(logs$Action, kRecordCreatedPattern)), ]

# Keep only log entries which are duplicated on the Action column, i.e.
# records created more than once or replaced records
replaced.cond <- duplicated(logs$Action) | duplicated(logs$Action, fromLast = T)
replaced <- logs[replaced.cond, ]
replaced <- replaced[order(replaced$Action, replaced$Time...Date), ]