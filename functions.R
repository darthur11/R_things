#Data manipulation:
library(dplyr)
library(tidyr)
library(magrittr)
library(reshape2)
#Strings:
library(stringr)
library(stringi)
library(lubridate)
#Excel:
library(openxlsx)
#DBs:
library(RODBC)
library(RMariaDB)
library(RPostgreSQL)
#Vis:
library(ggplot2)
library(highcharter)
#Cloud services:
library(googledrive)
library(googlesheets)


##### Current time with timezone -----
current_time<-now()
attributes(current_time)$tzone<-'Asia/Almaty'

##### Concatenation of 2 strings -----
`%+%` <- function(a, b) paste(a,b, sep="")

##### Days in month -----
days_in_month <- function(date){
  require(lubridate) <- 
    day(floor_date(as.Date(date),'month') + months(1) - days(1))
}

##### Generate out_name.xlsx from list of dataframes -----
makeExcel<-function(lst, out_name){
  wb <- createWorkbook()
  width_adjuster <- 2.5
  bldStyle  <- createStyle(fontSize = 11, fontColour = "black", textDecoration = c("BOLD"))
  for (i in 1:length(names(lst))) {
    addWorksheet(wb, names(lst)[i])
    writeData(wb, names(lst)[i], lst[[names(lst)[i]]], colNames=TRUE, headerStyle = bldStyle)
    
    width_vec <- apply(lst[[names(lst)[i]]], 2, function(x) max(nchar(as.character(x)) + width_adjuster, na.rm = TRUE))
    width_vec_header <- nchar(colnames(lst[[names(lst)[i]]]))  + width_adjuster
    
    width_vec_max <- pmax(width_vec, width_vec_header)
    
    setColWidths(wb, sheet = names(lst)[i], cols = 1:ncol(lst[[names(lst)[i]]]), widths = width_vec_max)
  
  } 
  saveWorkbook(wb, out_name, overwrite = TRUE)
}



##### Set UTF8 values -----
set_utf8 <- function(x){
  # Declare UTF-8 encoding on all character strings:
  for (i in 1:ncol(x)){
    if (is.character(x[, i])) Encoding(x[, i]) <- "UTF-8"
  }
  # Same on column names:
  for (name in colnames(x)){
    Encoding(name) <- "UTF-8"
  }
  x
}



##### Percent format -----
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}



##### SlackR using -----
API_TOKEN <- ""
slackr_setup(api_token = API_TOKEN)
emojies <- c(':dancing_lisa:',
             ':dancing_bear:',
             ':dancing_pinguin:',
             ':fry_dancing:',
             ':snoop_dancing:')
EMOJI <- sample(emojies,1)
COMMENT <- paste0(EMOJI,' Statistics for ',format(current_time-days(1), '%Y-%m-%d'), '. Sent @ ', format(current_time, "%d-%m-%Y %X"))
tmp_df<-df_prev %>% filter(group==teams[i]) %>% select(1:8)
png(filename = "/output/output.png", width=815,height=25+25*nrow(tmp_df),bg = "white")
grid.table(tmp_df)
dev.off()
slackr_upload(filename = "/output/output.png",channel="", initial_comment = COMMENT)



##### Clean phones -----
clean_phones <- function(phone, invalid = NA) {
  phone <- as.character(phone)                            #converting to character to can operate
  phone <- trimws(phone)                                  #remove whitespace
  phone <- strsplit(phone, ",")[[1]][1]       #getting first element of separeted by comma vector
  phone <- as.numeric(paste0(unlist(
    regmatches(phone, gregexpr("[[:digit:]]", phone))),
    collapse = ''))                                       #digits only, delete all other sympols                                  #digits only, delete all other sympols
  if(!is.na(phone) && nchar(phone) == 11) { phone <- gsub("(^87)", "77", phone) }
  if(!is.na(phone) && nchar(phone) == 12) { phone <- gsub("(^87)", "7", phone) }                      #convert '87...' to '7..' phone format
  phone[!nchar(phone) %in% c(11)] <- invalid            #keep only 11 digit numbers
  phone
}

##### Connections to different databases -----
credentials <- readRDS('C:/Users/Artur.Dossatayev/Documents/credentials/cred.RDS')
mssql <- odbcDriverConnect(paste0('driver={SQL Server};server=',
                               credentials$mssql$local, 
                               ';database=',
                               credentials$mssql$db,
                               ';uid=', 
                               credentials$mssql$login, 
                               ';pwd=', 
                               credentials$mssql$password,
                               ';'))

redshift <- dbConnect(dbDriver("PostgreSQL"), 
                    host=credentials$redshift$server,
                    port=credentials$redshift$port,
                    user=credentials$redshift$login,
                    dbname=credentials$redshift$db,
                    password=credentials$redshift$password)

mysql <- dbConnect(MariaDB() ,user=mysql$UPSELL$login,
                    port = credentials$mysql$port,   
                    host=credentials$mysql$server,
                    dbname=credentials$mysql$db,
                    password=credentials$mysql$password)
