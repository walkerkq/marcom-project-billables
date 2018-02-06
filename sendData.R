library(googlesheets)


sendData <- function(df){

  # load sheet
  ss_id <- gs_title("All_Wufoo_Projects")
  id <- gs_read(ss=ss_id)
  keep_cols <- names(id)
  
  # cut our df down to match
  df_slim <- df[ , names(df) %in% keep_cols]
  
  # only update if the column names match
  if(length(df_slim[1,]) == 24){
    
    gs_add_row(ss=ss_id, input=df_slim)
    message <- "Status: Successfully sent to Google Sheets."
    
  } else {
    message <- "Status: Error. Data does not match. Check column names."
  }
  return(message)
}