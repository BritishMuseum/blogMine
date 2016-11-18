#' Get URLs to blog post full text for all posts
#' by scraping them out of each page of the
#' main blog aggregator
#' 

# Set the working directory

setwd('/Users/danielpett/githubProjects/blogMine/')

# List of packages needed
list.of.packages <- c(
  "RCurl", "XML","jsonlite", 
  "stringr", "ggplot2", "reshape2",
  "igraph", "d3Network" 
)

# Install packages if not already
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Initiate the libraries needed to get data from the blog.
library(RCurl)
library(XML)
library(stringr)


# determined by inspecting the first page
n <- 74 
# pre-allocate list to fill
links <- vector("list", length = n)

links[[1]] <-  unname(xpathSApply(htmlParse(getURI("https://blog.britishmuseum.org/")),"//h2/a/@href"))

# Now loop through the rest of the pages to get data

for(i in 1:n){
  # track progress by showing the iteration we're up to
  print(i)
  # get all content on the i+1 th page of the main blog list
  blogdata <- htmlParse(getURI(paste0("https://blog.britishmuseum.org/page/", i+1,"/")))
  # extract links for all posts
  links[[i+1]] <- unname(xpathSApply(blogdata,"//h2/a/@href"))
}

#' Start with output from scraping for URLs, now pull full text
#' get text from each post using the URLs we just got

# Create a big list of URL to scrape data
linksall <- unlist(links)

# Create an empty data.frame to store the full text in and get date and author of full text also
blogtext <- data.frame(text =  vector(length = length(linksall)),                                     
                       date = vector(length = length(linksall)),
                       day = vector(length = length(linksall)),
                       year = vector(length = length(linksall)),
                       time = vector(length = length(linksall)),
                       author = vector(length = length(linksall))
)

# make a list to store comments for each blog post
names <- vector("list", length = length(linksall))

# loop over the URLs to pull full text, etc. from each URL
# includes error handling in case a field is empty, etc.
for(i in 1:length(linksall)){
  # track progress
  print(i)
  # get URL
  blogdata <- htmlParse(getURI(linksall[[i]]))
  # get text from URL
  result <- try(
    blogtext[i,1] <- gsub('Share this:Like this:Like Loading...:Filed under','', xpathSApply(blogdata, "//*/div[@class='entry']", xmlValue))
  ); if(class(result) == "try-error") next;
  # get date of blog post
  # first month and day
  result <- try(
    blogtext[i,2] <- strsplit(xpathSApply(blogdata, "//*/span[@class='date']", xmlValue), ",")[[1]][1],
  ); if(class(result) == "try-error") next;
  # and then year, and remove excess white space
  result <- try(
    blogtext[i,3] <- str_sub(gsub("\\s","", strsplit(xpathSApply(blogdata, "//*/span[@class='date']", xmlValue), ",")[[1]][2]),0,4)
  ); if(class(result) == "try-error") next;
  # and then time, and remove excess white space
  result <- try(
    blogtext[i,4] <- str_sub(gsub("\\s","", strsplit(xpathSApply(blogdata, "//*/span[@class='date']", xmlValue), ",")[[1]][2]),-7)
  ); if(class(result) == "try-error") next;
  # and then author
  result <- try(
    blogtext[i,5] <- xpathSApply(blogdata, "//*/span[@class='contributor']", xmlValue)
  ); if(class(result) == "try-error") next;
  # and the names of the commenters 
  result <- try(
    commenters[[i]] <- xpathSApply(blogdata, "//*/cite[@class='fn']", xmlValue)
  ); if(class(result) == "try-error") next;
}

# add columns of URLs to the fulltext post
blogtext$url <- linksall

#' clean out non-ASCII characters and formatting

# remove non-ASCII characters
Encoding(blogtext[,1]) <- "latin1" 
iconv(blogtext[,1], "latin1", "ASCII", sub="")
# remove newline character
blogtext[,1] <- gsub("\n","", blogtext[,1]) 
# save as CSV so others can use it
write.csv(blogtext, 'britishmuseum.csv')