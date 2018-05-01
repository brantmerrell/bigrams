# txtFile <- "data_2.txt"
bigram <- function(txtFile, output="png", borderColor = "darkred", barColor = "royalblue", threshold=0){
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
  
  # Obtain tallies for the unique bigrams 
  bigrams <- table(bigrams)
  
  # eliminate bigrams that fall below threshold
  bigrams <- bigrams[threshold <= bigrams]
  # convert to data frame for bucketing
  bigrams <- data.frame(bgrm = row.names(bigrams), tally = matrix(bigrams),
                        # begin alphabin as ":other:" because other categories are later added
                        alphabin = ":other:",
                        # avoid factors for compatibility with alphabetic bucketing
                        stringsAsFactors = F)
  
  bigrams <- bigrams[order(bigrams$tally, decreasing = T),]
  
  # seq to 26+1 because bucketing without overlap requires tempSeq[n]-1
  # buckets are between seq values but category "other" exists, so length.out = #(buckets) + 1 - 1
  tempSeq <- round(seq(1, 27, length.out = 5)) 
  # iterate through ranges of alphabet to bucket alphabetically
  for(n in seq(tempSeq)[-1]){
    # define pattern to match first character of bigram if within range of letters
    tempPattern <- paste(c("^[", letters[tempSeq[n-1]:(tempSeq[n]-1)],"]"), collapse = "")
    # match rows that follow pattern, and modify column "alphabin"
    bigrams[grepl(tempPattern, bigrams$bgrm, ignore.case = T),"alphabin"] <- 
      # string together first and last letter in range to insert into bigrams[<subset>,"alphabin]
      paste(LETTERS[c(tempSeq[n-1], tempSeq[n]-1)], collapse = "-")
  }
  
  DF <- aggregate(bigrams$tally, list(bigrams$alphabin), FUN=sum)
  DF <- DF[order(DF$x),]
  if(output=="png") {
    # create png file named the same as the txt file
    if(!dir.exists("gallery")) dir.create("gallery")
    pngFile <- file.path("gallery", paste("R",gsub(".{4}$",".png",txtFile), sep = "_"))

    # open connection to png file
    png(pngFile)
  }
  COLS <- colors(T)
  tempSeq <- seq(1, nrow(bigrams), length.out = 6)
  par(mfcol=c(2,2))
  hist(bigrams$tally,
       # Set freq at F for probability densities
       freq = F,
       # display txtFile unless it is located in a temp folder - then abbreviate to temp.txt
       main = paste("Histogram for", ifelse(grepl("tmp/",txtFile), "tmp.txt", txtFile), 
                    # display programming language that generated output
                    "- R"), 
       # name X & Y axes
       xlab = "", ylab = "Bigram Probability Density",
       # set bar & border colors
       border = borderColor, 
       col = sample(COLS[grepl("blue",COLS)]))
  
  bplt <- barplot(head(bigrams$tally), names.arg = head(bigrams$bgrm), main = "Highest Frequency",
          xlab = "", ylab = "Bigram Count", horiz = F, las=2, col = sample(COLS[grepl("red",COLS)], nrow(head(bigrams))))
  text(bplt, head(bigrams$tally), head(bigrams$tally), pos = 1)
  bplt <- barplot(DF$x, names.arg = DF$Group.1,  main = "Alphabetic Buckets", xlab = "", 
                  ylab = "Bigram Count", col = sample(COLS[grepl("green",COLS)], nrow(DF)))
  text(bplt, DF$x, DF$x, pos = 1)
  bplt <- barplot(log(bigrams[tempSeq,"tally"]), names.arg = bigrams[tempSeq,"bgrm"], 
                  main = "Log of Examples", xlab = "", ylab = "Log of Bigram Count", horiz = F, 
                  las=2, col = sample(COLS[grepl("gray",COLS)], length(tempSeq)))
  text(bplt, log(bigrams[tempSeq,"tally"]), round(log(bigrams[tempSeq,"tally"]),1), pos = 1, col = "royalblue")
  # close connection to png file
  if(output=="png") {
    dev.off()
    # print name of png file
    print(pngFile)
  }
  
}

ARGS <- commandArgs()[6]
if(grepl("txt$", ARGS, ignore.case = T)){
  bigram(ARGS)
}
