library(stringr)
library(redcapAPI)

source("tokens.R")

# Parameters
kLoggingFileName <- "TIPTOPHHSEndlineMozambique_Logging_2021-11-03_1457.csv"
kRecordCreatedPattern <- "Created Record (API)"
kRecordCreatedPatternScaped <- "Created Record \\(API\\)"
kFileFields <- c("anc_card_pic")

FormatREDCapData <- function(log.string) {
  
  key.value.pairs <- trimws(str_split(log.string, ",")[[1]])
  
  key.value.pairs.vector = c()
  for (i in key.value.pairs) {
    key.value <- trimws(str_split(i, "=")[[1]])
    key.value.pairs.vector[key.value[1]] <- str_remove_all(key.value[2], "'")
  }
  
  record.data <- data.frame(as.list(key.value.pairs.vector))
  return(record.data)
}

# Read REDCap logging file (It must be manually downloaded from the REDCap 
# project. There's no API call to retrieve logs)
logs <- read.csv(kLoggingFileName)
colnames(logs) <- c("datetime", "username", "action", "data")

# Keep only logs related to record creation through the API
logs <- logs[which(startsWith(logs$action, kRecordCreatedPattern)), ]

# Keep only log entries which are duplicated on the Action column, i.e.
# records created more than once or replaced records
replaced.cond <- duplicated(logs$action) | duplicated(logs$action, fromLast = T)
replaced <- logs[replaced.cond, ]
replaced <- replaced[order(replaced$action, replaced$datetime), ]

# Extract record id from the action column
replaced$record_id <- trimws(str_remove(
  string  = replaced$action, 
  pattern = kRecordCreatedPatternScaped
))

# Connect to REDCap and remove data from replaced records
redcap.connection <- redcapConnection(kREDCapAPIURL, kAPIToken)
field.names <- exportFieldNames(redcap.connection)

for (i in unique(replaced$record_id)) {
  # Create empty record to be imported in REDCap for removing data of replaced
  # record. We want to keep the record empty with their history.
  empty.record <- c()
  empty.record[field.names$export_field_name] <- ''
  empty.record["record_id"] <- i
  
  # Remove files attached to the replaced record
  for (j in kFileFields) {
    # The function deleteFiles crashes if there's no file attached to the record
    tryCatch( 
      {
        deleteFiles(redcap.connection, i, j)
      },
      error = function(cond) {
        message(paste("This record doesn't contain a file in", j))
      }
    )
  }
  
  # Erase data from replaced record
  empty.record.df <- data.frame(as.list(empty.record))
  importRecords(
    rcon              = redcap.connection, 
    data              = empty.record.df, 
    overwriteBehavior = "overwrite"
  )
}