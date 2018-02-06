library(rvest)
library(dplyr)
library(magrittr)

keep_codes <- c("WEB5000", "WEB5500", "WEB5555", "WEB5566", "WEB5600", "WEB5700",
                "COM0006", "COM0007", "COM0008", "COM0010", "COM0011", "COM0013", 
                "COM0014", "COM0015", "COM0016", "COM0019", "COM0022", "COM0023",
                "COM0032", "COM0033", "COM0034", "COM0035", "COM0038", "COM0041",
                "COM0044", "COM0046", "COM0047", "COM0048", "COM0049", 
                "WEB0020", "WEB5010", "COM5500")

# Requires a df with the column "Job.Number"

processData <- function(df){
  
  if(!"Job.Number" %in% colnames(df)) {
    
    project_data <- "Error: This file does not have a column titled 'Job.Number'"
    
  } else {
    
    # remove blanks
    df <- df[!is.na(df$Job.Number) & df$Job.Number != "", ]
    
    # grab labor and cost data from TMA
    project_data <- NULL
    for(proj in seq_along(df[,1])){
      id <- df[proj, "Job.Number"]
      return_html <- read_html(paste("http://webtma:11010/wo_charges.html?", id, sep=""))
      # re-create the table
      unstructured <- return_html %>%
        html_nodes(xpath="//table[@class='tableInContainer']//tr//td") %>%
        html_text()
      if(length(unstructured)==0){
        row <- data.frame(ID=id, Labor=0, 
                          Cost=0, Date.Billed=NA, 
                          df[proj, names(df) != "Job.Number"],
                          stringsAsFactors=F)
      } else {
        tbl <- unstructured[1:(length(unstructured)-3)] %>%
          matrix(ncol=5,  byrow=T) %>%
          data.frame(stringsAsFactors=F) %>% 
          set_colnames(c("Date.Billed", "Code", "Code.Desc", "Labor", "Cost")) %>%
          mutate(Labor = as.numeric(Labor),
                 Cost = as.numeric(gsub("\\$|\\,", "", Cost)))
        # summarise and collapse into one row
        row <- data.frame(ID=id, Labor=sum(tbl$Labor[tbl$Code %in% keep_codes]), 
                          Cost=sum(tbl$Cost), Date.Billed=tbl$Date.Billed[1], 
                          df[proj, names(df) != "Job.Number"],
                          stringsAsFactors=F)
      }
      # add to the master df
      project_data <- rbind(project_data, row)
    }
    # adjust dates for outliers and blanks
    project_data$Date.Billed <- as.Date(project_data$Date.Billed, format="%m/%d/%Y")
    avg_date <- median(project_data$Date.Billed, na.rm=T)
    project_data$Date.Billed[is.na(project_data$Date.Billed)] <- avg_date
    month.start <- format(avg_date, "%Y-%m-01")
    month.end <- format(avg_date, "%Y-%m-31")
    project_data$Date.Billed[project_data$Date.Billed < month.start | project_data$Date.Billed > month.end] <- avg_date
    
  }
  
  return(project_data)
}


