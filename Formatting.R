# Formatting
# Getting data ready for filtering

# Names
# Nicknames were found in quotations
# (sp?) was found all over dataset
nameFormat <- function(x, cols) {
  for(c in cols) {
    # Nicknames
    n <- gsub("\"(.*?)\"","",x[,c])
    # Taking out (sp?)
    n <- gsub('\\(sp\\?\\)',"",n)
    # Removing whitespace from beginning and end
    n <- gsub("^\\s+|\\s+$", "", n)
    # Lowercase
    n <- tolower(n)
    x[,c] <- n
  }
  
  return(x)
}

# Phone Numbers
# Removes all punctuation and then replaces extensions with :
phoneFormat <- function(x, cols) {
  for(c in cols) {
    # Punctuation
    n <- gsub( "[[:punct:]]", "", x[,c])
    # Spaces
    n <- gsub(" ", "", n, fixed = TRUE)
    # Standardize Extensions
    n <- tolower(n)
    n <- gsub("ext",":", n)
    n <- gsub("x", ":", n)
    x[,c] <- n
  }
  
  return(x)
}