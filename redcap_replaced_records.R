library(stringr)
library(redcapAPI)

FormatREDCapData <- function(log.string) {
  
  key.value.pairs <- trimws(str_split(log.string, ",")[[1]])
  
  key.value.pairs.vector = c()
  for (i in key.value.pairs) {
    key.value <- trimws(str_split(i, "=")[[1]])
    key.value.pairs.vector[key.value[1]] <- str_remove_all(key.value[2], "'")
  }
  
  return(key.value.pairs.vector)
}

# Read REDCap logging file (It must be manually downloaded from the REDCap 
# project. There's no API call to retrieve logs)
kLoggingFileName <- "TIPTOPHHSEndlineMozambique_Logging_2021-11-03_1457.csv"
logs <- read.csv(kLoggingFileName)
colnames(logs) <- c("datetime", "username", "action", "data")

# Keep only logs related to record creation through the API
kRecordCreatedPattern <- "Created Record \\(API\\)"
logs <- logs[which(startsWith(logs$action, kRecordCreatedPattern)), ]

# Keep only log entries which are duplicated on the Action column, i.e.
# records created more than once or replaced records
replaced.cond <- duplicated(logs$action) | duplicated(logs$action, fromLast = T)
replaced <- logs[replaced.cond, ]
replaced <- replaced[order(replaced$action, replaced$datetime), ]

# Extract record id from the action column
replaced$record_id <- trimws(str_remove(replaced$action, kRecordCreatedPattern))