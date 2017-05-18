# Filtering out Bad Data
# Each filter accepts a data frame and a list of columns
# Returns filtered data frame
 
# Check Names for Special Characters
stringFilter <- function(x, cols) {
  for(c in cols) {
    x[grepl("[^[:space:]A-z0-9/'-/-]",x[,c]),"Bad_Data"] <- colnames(x)[c] #TRUE
  }
  
  return(x)
}

# Phone Number Check
phoneFilter <- function(x, cols) {
  for(c in cols) {
    x[(nchar(as.character(x[,c]))<7&nchar(as.character(x[,c]))!=0)
      |grepl("[[:alpha:]]", x[,c]), "Bad_Data"] <- colnames(x)[c]#TRUE
  }
  return(x)
}

# Email Check
emailFilter <- function(x, cols) {
  for(c in cols) {
    x[!grepl("@",x[,c])&nchar(as.character(x[,c]))!=0, "Bad_Data"] <- colnames(x)[c]#TRUE
  }
  return(x)
}