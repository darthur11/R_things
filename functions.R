library(openxlsx)

##### Current time with timezone -----
current_time<-now()
attributes(current_time)$tzone<-'Asia/Almaty'



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



##### Update statement function -----

update_stmt<-function(upd,param, val){
  if(is.na(val)){
    upd<-paste0(upd,param,"=","NULL,")   
  } else if(is.numeric(val)){
    upd<-paste0(upd,param,"=",val,",")
  } else if(is.character(val)){
    val<-gsub("[\',\",\n]",' ',val)
    upd<-paste0(upd,param,"=", "'",val,"',")
  } else if(is.Date(val)){
    upd<-paste0(upd,param,"=","'",val,"',")
  } 
  upd
}

---example:
#for (col in cols) {
#	upd_stmt<-update_stmt(upd_stmt,col,df[1,c(col)])
#}
#upd_stmt<-paste0(substr(upd_stmt,1,nchar(upd_stmt)-1),' where id = ',df[1,1])


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



