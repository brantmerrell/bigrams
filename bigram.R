# txtFile <- "data_3.txt"
bigram <- function(txtFile){
  # read file
  words <- readLines(txtFile)
  
  # split text into individual words by spaces, slashes, dashes, commas, periods, and semicolons
  words <- unlist(strsplit(words, "[\n\r\t /,;]"))
  
  # remove punctuation that precedes or trails words (but not possessive  apostrophes)
  words <- gsub("^[[:punct:]]+|[[:punct:]]+$", "", words)
  
  # remove empty elements from words vector
  words <- words[grepl(".", words)]
  
  # begin vector of bigrams as empty
  bigrams <- c()
  
  # iterate through words to build biagrams vector
  for(n in 2:length(words)){
    bigrams <- c(bigrams,
                 paste(words[n-1], words[n]))
  }

  bigrams <- table(bigrams)
  
  # create png file named the same as the txt file
  if(!dir.exists("gallery")) dir.create("gallery")
  pngFile <- file.path("gallery", paste("R",gsub("txt$","png",txtFile), sep = "_"))
  
  # open connection to png file
  png(pngFile)
  
  # create histogram of bigrams
  hist(matrix(bigrams),
       main = paste("Histogram of bigrams in", txtFile, "- R"),
       xlab = "Tally of bigrams",
       ylab = "Frequency of tallies",
       border = "blue",
       col = "green")
  
  # close connection to png file
  dev.off()
  
  # print name of png file
  print(pngFile)
}

ARGS <- commandArgs()[6]
if(grepl("txt$", ARGS, ignore.case = T)){
  bigram(ARGS)
}
