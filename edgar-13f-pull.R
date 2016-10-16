library(data.table)
library(plyr)
library(stringr)
library(XML)


# Script takes one CIK code at a time and 
# 1) Retrieves search results page for all 13-F filings for that CIK code
# 2) parses filenames from the search results 
# 3) downloads (to current dir) the filings using the filenames 
# 4) parses each filing for holdings data.

# Could make this a loop that takes multiple CIKs.

# some sample CIK codes
# Evercore 0001482689
# blackrock  0001006249 https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=0001006249&type=13F-HR&dateb=&owner=exclude&count=40
# wellington 0000902219 https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=0000902219&type=13F-HR&dateb=&owner=exclude&count=40
# Soros Wealth Management  0001029160

# set current CIK
cik <- "0001029160" 

raw.dest <- "raw-13F/"
out.dest <- "out/"

# #############################################################################
# 1) get search results page 

index.page <- paste("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=",cik,"&type=13F-HR&dateb=&owner=include&count=20",sep="")
download.file(index.page, destfile = paste(raw.dest, cik,"-index.html", sep=""))

# parse the downloaded index page to get the actual filename of each filing. Each filing has an "accension number" (acc.no)
doc = htmlParse(paste(raw.dest, cik,"-index.html", sep=""))
# there is one filing in each row - just grab the whole line of text
filing.rows <- xpathSApply(doc, "//tr", xmlValue)
# for now, don't use amended ones - just ignore 13F-HRA
filing.rows <- grep("13F-HR\n", filing.rows, value=TRUE)



# #############################################################################
# 2 and 3) loop through list of filings, parse filenames, download files, return data frame with summary info

# URL format for retrieving filings uses CIK code with no leading zeros
cik <- gsub("^[0]*","", cik)
# substr(cik,4,10)

filings <- data.frame(rbindlist(lapply(filing.rows, function(x) {
  
  # messy parsing of accension number, file date from each line of text
  acc.no.dashes <- substr(unlist(strsplit(x, "Acc-no: "))[2],0,20) # accension number with dashes
  #print(acc.no.dashes)
  acc.no <- gsub("[^0-9]","",acc.no.dashes) # accension number no dashes
  #print(acc.no)
  file.date <- str_trim(unlist(strsplit(x, "\n"))[4])
  
  # make url to download filing
  remote.page <- paste("https://www.sec.gov/Archives/edgar/data/",cik,"/",acc.no,"/",acc.no.dashes,".txt",sep="")
  # what we save it as locally
  local.page <- paste(raw.dest, cik, "-", file.date, "-",acc.no, ".xml", sep="")

  download.file(remote.page, destfile = local.page)
  return(data.frame(cik, acc.no, acc.no.dashes, file.date,local.page, remote.page))
})),stringsAsFactors=FALSE)

# want info as characters, not factors
filings$local.page <- as.character(filings$local.page)
filings$remote.page <- as.character(filings$remote.page)
filings$cik <- as.character(filings$cik)
filings$acc.no <- as.character(filings$acc.no)
filings$acc.no.dashes <- as.character(filings$acc.no.dashes)
filings$file.date <- as.Date(as.character(filings$file.date))

# only keep filings after 2Q 2013 -- filings before this are in unstructured html
filings <- filings[filings$file.date > as.Date("2013-06-01"),]


# #############################################################################
# 4) loop through each filing, parse out the holdings, save it

df.filings <- data.frame(rbindlist(lapply(1:nrow(filings),function(x) {
  # search for what is in the /informationTable tags
  print(filings[x,"local.page"])
  # read in filing text
  text <- readLines(filings[x,"local.page"])
  
  # there are two XML docs in this one text file -- one with summary info, one with the holding information
  summary.doc = xmlTreeParse(text[grep("<summaryPage",text):grep("</summaryPage",text)], 
                          useInternalNodes = TRUE)
  info.doc = xmlTreeParse(text[grep("<informationTable",text):grep("</informationTable",text)], 
                             useInternalNodes = TRUE)
  
  # parse XML into a list
  data.sum <- xmlToList(summary.doc)
  data.info <- xmlToList(info.doc)
  
  # put holding info into data frame
  df.info <- ldply(data.info, data.frame)
    
  # add  identifying information to each row
  df.info$cik <- filings[x,"cik"]
  df.info$accno <- filings[x,"accno"]
  df.info$file.date <- filings[x,"file.date"]
  df.info$num.holdings <- data.sum$tableEntryTotal
  df.info$value.holdings <- data.sum$tableValueTotal
  
  df.info
})))

#dim(df.filings)

# sometimes, depending on company's XML  .... 
# have extraneaous column that contains the name of the xsd file - get rid of it
df.filings <- df.filings[,!(names(df.filings) %in% c(".id","X..i.."))]
# has extra NA row for each filing - get rid of it
df.filings <- df.filings[which(!is.na(df.filings$nameOfIssuer)),]

write.csv(df.filings, file=paste(out.dest, cik, "-holdings.csv", sep=""), row.names = FALSE)

