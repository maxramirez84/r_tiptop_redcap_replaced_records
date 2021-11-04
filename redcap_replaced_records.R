library(stringr)
library(redcapAPI)
library(dplyr)

source("tokens.R")

# Parameters
kLoggingFileName <- "TIPTOPHHSEndlineMozambique_Logging_2021-11-03_1457.csv"
kRecordCreatedPattern <- "Created Record (API)"
kRecordCreatedPatternScaped <- "Created Record \\(API\\)"
kFileFields <- c("anc_card_pic")

FormatREDCapData <- function(log.string) {
  #browser()
  # Extract key-value pairs by regex
  kKeyValuePattern   <- "\\w+ = '(\\w*[-!: ,\\\".\\(\\)\\*]*\\w*)*'"
  kMultiChoicePatter <- "\\w+\\(\\d+\\) = checked"
  record.data.fields <- c(
    unlist(str_extract_all(
      string  = log.string,
      pattern = kKeyValuePattern
    )),
    unlist(str_extract_all(
      string  = log.string,
      pattern = kMultiChoicePatter
    ))
  )
  
  # For each field
  key.value.pairs.vector = c()
  if (length(record.data.fields) > 0) {
    # Split key and value separated by the equal sign
    for (j in record.data.fields) { 
      key.value.pair <- trimws(unlist(strsplit(j, "=")))
      
      # If multi-choice variable, change name from var_name(n) to var_name___n
      kMultiChoiceREDCapPattern <- ".*\\((\\d+)\\)"
      key   <- key.value.pair[1]
      value <- key.value.pair[2]
      if (grepl(kMultiChoiceREDCapPattern, key)) {
        key <- paste0(
          unlist(strsplit(key, "\\("))[1], 
          "___", 
          sub(kMultiChoiceREDCapPattern, "\\1", key)
        )
      }
      key.value.pairs.vector[key] <- str_remove_all(value, "'")
    }
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

# Connect to REDCap
redcap.connection <- redcapConnection(kREDCapAPIURL, kAPIToken)
field.names <- exportFieldNames(redcap.connection)

# Remove data from replaced records
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

# Import replaced and replacement data as NEW records
data <- c()
data[field.names$export_field_name] <- NA
data <- data.frame(as.list(data))

for (i in replaced$data) {
  print(i)
  data <- bind_rows(data, FormatREDCapData(i))
}

data <- data[-1, ]
next.record.id <- exportNextRecordName(redcap.connection)
new.records.ids <- next.record.id:(next.record.id + nrow(data) - 1)
data$record_id <- new.records.ids

importRecords(redcap.connection, data, batch.size = 500)
