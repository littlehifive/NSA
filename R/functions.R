# clean spaces in text responses
clean_space <- function(data, var){
  data[,var] <- gsub("\\r", "", data[, var])
  data[,var] <- gsub("\\n", "", data[, var])
  data[,var] <- gsub("^\\s+", "", data[, var])
  data[,var] <- gsub("\\s+$", "", data[, var])
  return(data)
}
