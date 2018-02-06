sendData <- function(df){
  
  write.csv(df, "my_df.csv", row.names=F)
  print("I sent it")
  
}