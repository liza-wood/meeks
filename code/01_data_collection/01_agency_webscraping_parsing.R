
# Set up ----
library(tools)
library(textmineR)
library(tidytext)
library(tidyverse)
library(reshape)
library(tm)
library(rvest) ## this is the main package
library(httr) ## this is the main package
library(pdftools)
library(readtext)
library(tokenizers)
library(ggplot2)
library(textdata)
library(corpus)
library(dplyr)
setwd("~/Box/truckee/")
source("code/scraping_agencies/functions.R")


#######################################
#  STATE   
#######################################

#######################################
## 1. CalTrans (COMPLETE) -----
agency = "caltrans"
agency_type = "state"
#######################################
# Plans pages ----
#I used webscraper to get the spreadsheet, so now I can combine all three

metadata = read.csv("data/agency_webscrapes/caltrans.csv")
metadata$agency = agency
metadata$agency_type = agency_type

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_edited/caltrans_edit.csv',
  row.names = FALSE)



#######################################
#  MPOS     
#######################################
#######################################
## 1. AMBAG (COMPLETE) -----
# This took both pulling from the web via R (for the report page, which had two separate pages), and webscraping, which I did using the extension and then uploaded and merged. I did some deletion and renaming of columns by hand though, which I know was bad form.

host <- 'http://www.ambag.org' #main page
agency = "ambag"
agency_type = "MPO & COG"
#######################################
# Plans pages ----
#I used webscraper to get the spreadsheet, so now I can combine all three

metadata1 = read.csv("data/agency_webscrapes/ambag_plans.csv")
metadata1$topic = str_remove(metadata1$topic, pattern = "https://ambag.org/plans/")
metadata1$topic = gsub("-", " ", metadata1$topic)
metadata1$doc_title = paste(metadata1$topic, metadata1$title, sep=' ')
metadata1$doc_title = toTitleCase(metadata1$title)
metadata1$doc_type = metadata1$title
metadata1$doc_subject = ""
metadata1$doc_year = ""
metadata1$agency = agency
metadata1$agency_type = agency_type

metadata1 = metadata1 %>% select(-topic, -title)

# Reports and MTIP pages ----

metadata2 = read.csv("data/agency_webscrapes/ambag_reports.csv")
metadata2$agency = agency
metadata2$agency_type = agency_type

# Compiling as metadata ----
metadata = rbind(metadata1, metadata2)

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/ambag_metadata.csv',
  row.names = FALSE)



#######################################
## 2. SLOCOG (COMPLETE) -----
agency = "slocog"
agency_type = "MPO & COG & RTPA"
#######################################
# This is all webscraped without any manual editing ----
df = read.csv("data/agency_webscrapes/slocog.csv")
df = df %>% select(topic1, topic2, link, link.href)

# This is a horrible way of getting rid of rows I don't want. Need to get better
grepdf = as.data.frame(grepl("mailto", df$link.href))
grepdf2 = as.data.frame(grepl("loan", df$link.href))
grepdf3 = as.data.frame(grepl("mythbusters", df$link.href))
grepdf4 = as.data.frame(grepl("video", df$link.href))
df = cbind(df,grepdf, grepdf2, grepdf3, grepdf4) 

columns = df[,5]
df = df %>% filter(columns == FALSE) 
columns = df[,6]
df = df %>% filter(columns == FALSE) 
columns = df[,7]
df = df %>% filter(columns == FALSE) 
columns = df[,8]
df = df %>% filter(columns == FALSE) 

colnames(df) = c("doc_subject", "doc_subject2", "doc_title", "url", "rm1", "rm2", "rm3", "rm4")
df$msg = ""
df$doc_type = ""
df$doc_year = ""
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df %>% select(url, msg, doc_title, doc_type, doc_subject, doc_subject2, doc_year, agency, agency_type)


## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/slocog_metadata.csv',
  row.names = FALSE)


#######################################
## 3. SBCAG (COMPLETE) -----
host <- 'http://www.sbcag.org' #main page
agency = "sbcag"
agency_type = "MPO & COG & RTPA & CMA"
#######################################
# Documents page ----
## Setting up the host and response site, and basic agency info
response <- GET(host, path = '/documents.html') #where pdfs are
page <- read_html(response) 

## Find the html_nodes by either using Inspect the html code of selector gadget extension

report_links <- html_nodes(page, css = '#wsite-content .paragraph a') 
report_urls <- sapply(report_links, html_attr, name = 'href') 
titles = report_links %>% html_text()

metadata1 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  doc_year = "",
  agency = agency,
  agency_type = agency_type)

# Planning: airports ----
response <- GET(host, path = 'airport-land-use-commission.html') #where pdfs are
page <- read_html(response) 

## Find the html_nodes by either using Inspect the html code of selector gadget extension

report_links <- html_nodes(page, css = '#wsite-content li a') 
report_urls <- sapply(report_links, html_attr, name = 'href') #URL attributes
titles = report_links %>% html_text()

metadata2 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  doc_year = "",
  agency = agency,
  agency_type = agency_type)

# Planning: housing ----
## Setting up the host and response site, and basic agency info
response <- GET(host, path = 'rhna.html') #where pdfs are
page <- read_html(response) 

## Find the html_nodes by either using Inspect the html code of selector gadget extension

report_links <- html_nodes(page, css = '#wsite-content div:nth-child(5) a') 
report_urls <- sapply(report_links, html_attr, name = 'href') #URL attributes
titles = report_links %>% html_text()

metadata3 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  doc_year = "",
  agency = agency,
  agency_type = agency_type)

# Planning: resilience ----
## Setting up the host and response site, and basic agency info
response <- GET(host, path = 'tnra.html') #where pdfs are
page <- read_html(response) 

## Find the html_nodes by either using Inspect the html code of selector gadget extension

report_links <- html_nodes(page, css = '#wsite-content li a') 
report_urls <- sapply(report_links, html_attr, name = 'href') #URL attributes
titles = report_links %>% html_text()

metadata4 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  doc_year = "",
  agency = agency,
  agency_type = agency_type)

# Planning: Santa Ynez ----
## Setting up the host and response site, and basic agency info
response <- GET(host, path = 'tnra.html') #where pdfs are
page <- read_html(response) 

## Find the html_nodes by either using Inspect the html code of selector gadget extension

report_links <- html_nodes(page, css = 'div~ div+ .paragraph a') 
report_urls <- sapply(report_links, html_attr, name = 'href') #URL attributes
titles = report_links %>% html_text()

metadata5 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  doc_year = "",
  agency = agency,
  agency_type = agency_type)

# Active transport and congestion seems to be in documents section

# Compiling as metadata ----
# this is creating a directory to store the minutes you want
metadata = rbind(metadata1, metadata2, metadata3, metadata4, metadata5)

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/sbcag_metadata.csv',
  row.names = FALSE)


#######################################
## 4. BCAG (COMPLETE) -----
host <- 'http://www.bcag.org/#' #main page
agency = "bcag"
agency_type = "MPO & COG & RTPA"
#######################################
# All webscraped without any manual editing ----

df = read.csv("data/agency_webscrapes/bcag.csv")
df = df %>% select(topic1, link, link.href)

grepdf = as.data.frame(grepl("javascript", df$link.href))
df = cbind(df, grepdf)
columns = df[,4]
df = df %>% filter(columns == FALSE) 

colnames(df) = c("doc_subject", "doc_title", "url", "rm1")

df$msg = ""
df$doc_type = ""
df$doc_year = ""
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df %>% select(url, msg, doc_title, doc_type, doc_subject, doc_year, agency, agency_type)

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/bcag_metadata.csv',
  row.names = FALSE)



#######################################
## 5. SRTA (COMPLETE) -----
agency = "srta"
agency_type = "MPO & RTPA"
#######################################
# All webscraped without any manual editing ----
df1 = read.csv("data/agency_webscrapes/srta_planning.csv")
df1 = df1 %>% select(topic1, topic2, link, link.href)
df2 = read.csv("data/agency_webscrapes/srta_projects.csv")
df2 = df2 %>% select(topic1, topic2, link, link.href)
df3 = read.csv("data/agency_webscrapes/srta_participation.csv")
df3 = df3 %>% select(topic1, topic2, link, link.href)
df = rbind(df1, df2, df3)

grepdf = as.data.frame(grepl("mailto", df$link.href))
df = cbind(df, grepdf)
columns = df[,5]
df = df %>% filter(columns == FALSE) 

colnames(df) = c("doc_subject", "doc_subject2", "doc_title", "url", "rm1")

df$msg = ""
df$doc_type = ""
df$doc_year = ""
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df %>% select(url, msg, doc_title, doc_type, doc_subject, doc_subject2, doc_year, agency, agency_type)

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/srta_metadata.csv',
  row.names = FALSE)




#######################################
## 6. FCOG (COMPLETE) -----
agency = "fcog"
agency_type = "MPO & COG"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/fcog.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/fcog_metadata.csv',
  row.names = FALSE)


#######################################
## 7. KernCOG (COMPLETE) -----
agency = "kerncog"
agency_type = "MPO & COG & RTPA"
#######################################
# All webscraped without any manual editing ----
df = read.csv("data/agency_webscrapes/kerncog.csv")
df = df %>% select(topic1, link, link.href)

colnames(df) = c("doc_subject", "doc_title", "url")

df$msg = ""
df$doc_type = ""
df$doc_year = ""
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df %>% select(url, msg, doc_title, doc_type, doc_subject, doc_year, agency, agency_type)

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/kerncog_metadata.csv',
  row.names = FALSE)


#######################################
## 8. KingsCOG (COMPLETE) -----
host <- 'https://www.kingscog.org' #main page
agency = "kingscog"
agency_type = "MPO & RTPA"
#######################################
# 2018 RTP ----
response <- GET(host, path = '/rtp_adopted') 
page <- read_html(response) 

report_links <- html_nodes(page, css = '.sectionIntro a , a span span') 
report_urls <- sapply(report_links, html_attr, name = 'href') #URL attributes
titles = report_links %>% html_text()

metadata1 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  doc_year = "",
  agency = agency,
  agency_type = agency_type)

# 2014 RTP ----
response <- GET(host, path = '/2014_rtp') 
page <- read_html(response) 

report_links <- html_nodes(page, css = '.link') 
report_urls <- sapply(report_links, html_attr, name = 'href') #URL attributes
titles = report_links %>% html_text()

metadata2 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  doc_year = "",
  agency = agency,
  agency_type = agency_type)

# Manually scraped data ----
metadata3 = read.csv("data/agency_webscrapes/kingscog_manual.csv")
metadata3$agency = agency
metadata3$agency_type = agency_type


# Compiling as metadata ----
# this is creating a directory to store the minutes you want
metadata = rbind(metadata1, metadata2, metadata3)

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/kingscog_metadata.csv',
  row.names = FALSE)




#######################################
## 9. MCTC (COMPLETE) -----
agency = "mctc"
agency_type = "MPO & RTPA & CTC"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/mctc.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/mctc_metadata.csv',
  row.names = FALSE)



#######################################
## 10. MCAG (COMPLETE) -----
agency = "mcag"
agency_type = "MPO & COG & RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/mcag.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/mcag_metadata.csv',
  row.names = FALSE)


#######################################
## 11. SJCOG (COMPLETE) -----
agency = "sjcog"
agency_type = "MPO & COG & RTPA & CMA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/sjcog.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/sjcog_metadata.csv',
  row.names = FALSE)



#######################################
## 12. StanCOG (COMPLETE) -----
agency = "stancog"
agency_type = "MPO & COG & RTPA & LTA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/stancog.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/stancog_metadata.csv',
  row.names = FALSE)



#######################################
## 13. TCAG (COMPLETE) -----
agency = "tcag"
agency_type = "MPO & COG & RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/tcag.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/tcag_metadata.csv',
  row.names = FALSE)



#######################################
## 14. MTC (COMPLETE) -----
agency = "mtc"
agency_type = "MPO & RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/mtc.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/mtc_metadata.csv',
  row.names = FALSE)


#######################################
## 15. SACOG (COMPLETE) -----
agency = "sacog"
agency_type = "MPO & COG & RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/sacog.csv")
df$agency = agency
df$agency_type = agency_type

metadata = df 

# label type of document
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/sacog_metadata.csv',
  row.names = FALSE)



#######################################
## 16. SANDAG (COMPLETE) ----
host <- 'https://www.sandag.org/' #main page
agency = "sandag"
agency_type = "MPO & RTPA & CMA"
#######################################
# Collecting everything from one documents page ----
response <- GET(host, path = 'index.asp?fuseaction=publications.alphalist') #where pdfs are
page <- read_html(response) 

report_links <- html_nodes(page, css = '.bodylink') 
report_urls <- sapply(report_links, html_attr, name = 'href') #URL attributes
titles = report_links %>% html_text()

metadata <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  doc_year = "",
  agency = agency,
  agency_type = agency_type)

# Compiling as metadata ----
# this is creating a directory to store the minutes you want
metadata$document_id <- 1:nrow(metadata)

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/sandag_metadata.csv',
  row.names = FALSE)




#######################################
## 17. SCAG (COMPLETE) ----
agency = "scag"
agency_type = "MPO & COG & RTPA"
#######################################
# This is from the rvest page, but it does not work... ----
# https://www.datacamp.com/community/tutorials/r-web-scraping-rvest
get_last_page <- function(html){
  
  pages_data <- html %>% 
    # The '.' indicates the class
    html_nodes('grdFeaturedpage') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
}

first_page <- read_html('http://sustain.scag.ca.gov/Pages/Documents.aspx')
latest_page_number <- get_last_page(first_page)

# This also did not not work...
page <- read_html('http://sustain.scag.ca.gov/Pages/Documents.aspx') 

report_links <- html_nodes(page, css = '.fileDownload a') 
report_urls <- sapply(report_links, html_attr, name = 'href') #URL attributes
titles = html_text(page, css = '.SCAGDocLibTitle a')

metadata <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  doc_year = "",
  agency = agency,
  agency_type = agency_type)

# All manually scraped ----
df = read.csv("data/agency_webscrapes/scag.csv")
df$agency = agency
df$agency_type = agency_type
df = df %>% select(-X)

# Compiling as metadata ----
metadata = df 

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/scag_metadata.csv',
  row.names = FALSE)

#######################################
## 18. TRPA (COMPLETE)  ----
agency = "trpa"
agency_type = "RTPA & MPO"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/trpa.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/trpa_metadata.csv',
  row.names = FALSE)














# REMOVE THE  & from the agency listing
# put all the metadata into a md.raw folder
# remove document number

#In clearning
# get rid of year
# remove subjects and plan types that I don't like
# create docid that has all the info: document number and agency title 








#######################################
# COUNTY 
#######################################

#######################################
## 1. SanBenitoCOG  ----
host = 'http://sanbenitocog.org/'
agency = "sanbenitocog"
agency_type = "COG & RTPA"
#######################################
# Projects: Highways ----
response <- GET(host, path = '/highway-projects/') 
page <- read_html(response) 

report_links <- html_nodes(page, css = '#et-boc a') 
report_urls <- sapply(report_links, html_attr, name = 'href') 
titles = report_links %>% html_text()

metadata1 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "road",
  agency = agency,
  agency_type = agency_type)


# Projects: bicycle and pedestrian ----
response <- GET(host, path = 'bicycle-pedestrian/') 
page <- read_html(response) 

report_links <- html_nodes(page, css = '#et-boc a') 
report_urls <- sapply(report_links, html_attr, name = 'href') 
titles = report_links %>% html_text()

metadata2 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "at",
  agency = agency,
  agency_type = agency_type)

# Projects: airports ----
response <- GET(host, path = 'aluc/') 
page <- read_html(response) 

report_links <- html_nodes(page, css = '#et-boc a') 
report_urls <- sapply(report_links, html_attr, name = 'href') 
titles = report_links %>% html_text()

metadata3 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "aluc",
  agency = agency,
  agency_type = agency_type)

# Planning :  ----
response <- GET(host, path = 'planning/') 
page <- read_html(response) 

report_links <- html_nodes(page, css = '#et-boc a') 
report_urls <- sapply(report_links, html_attr, name = 'href') 
titles = report_links %>% html_text()

metadata4 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  agency = agency,
  agency_type = agency_type)

# Resources :  ----
response <- GET(host, path = 'resources/') 
page <- read_html(response) 

report_links <- html_nodes(page, css = '#et-boc a') 
report_urls <- sapply(report_links, html_attr, name = 'href') 
titles = report_links %>% html_text()

metadata5 <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  agency = agency,
  agency_type = agency_type)

# Compiling as metadata ----
# this is creating a directory to store the minutes you want
metadata = rbind(metadata1, metadata2, metadata3, metadata4, metadata5)

# Assigning type
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/sanbenitocog_metadata.csv',
  row.names = FALSE)


#######################################
## 2. SCCRTC  ----
agency = "sccrtc"
agency_type = "RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/sccrtc.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

# Assigning type
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/sccrtc_metadata.csv',
  row.names = FALSE)


#######################################
## 3. TAMC  ----
agency = "tamc"
agency_type = "RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/tamc.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

# Assigning type
assignsubject()
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/tamc_metadata.csv',
  row.names = FALSE)

#######################################
## 4. TCTC  ----
agency = "tctc"
agency_type = "RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/tctc.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

# Assigning type
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/tctc_metadata.csv',
  row.names = FALSE)

#######################################
## 5. ACTC  ----
agency = "actc"
agency_type = "MTC"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/alamedactc.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

# Assigning type
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/actc_metadata.csv',
  row.names = FALSE)



#######################################
## 6. CCAG_sanmateo  ----
agency = "ccagsanmeteo"
agency_type = "CMA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/ccag_sanmateo.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

# Assigning type
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/ccagsanmateo_metadata.csv',
  row.names = FALSE)
#######################################
## 7. NVTA  ----
agency = "nvta"
agency_type = "CMA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/nvta.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 

# Assigning type
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/nvta_metadata.csv',
  row.names = FALSE)

#######################################
## 8. VTA  ----
agency = "vta"
agency_type = "CMA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/vta.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/vta_metadata.csv',
  row.names = FALSE)
#######################################
## 9. STA  ----
agency = "sta"
agency_type = "CMA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/sta.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/sta_metadata.csv',
  row.names = FALSE)

#######################################
## 10. SCTA  ----
agency = "scta"
agency_type = "CMA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/scta.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/scta_metadata.csv',
  row.names = FALSE)

#######################################
## 11. CalaCOG  ----
agency = "calacog"
agency_type = "COG & RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/calacog.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df 
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/calacog_metadata.csv',
  row.names = FALSE)

#######################################
## 12. HCAOG  ----
agency = "hcaog"
agency_type = "COG & RTPA"
#######################################
# Scraped html to get pop-ups ----
df = read.csv("data/agency_webscrapes/hcaog_html.csv")
df2 = separate(df, html, c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "url", "c11", "c12", "c13", "c14", "c15", "c16"),sep = '"', remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")
df = df2 %>% select(url)

#Scraped titles to match
title = read.csv("data/agency_webscrapes/hcaog_title.csv")

metadata <- data.frame(
  url = df$url,
  msg = '', stringsAsFactors = FALSE,
  doc_title = title$doc_title,
  doc_type = "",
  doc_subject = "",
  agency = agency,
  agency_type = agency_type)

# Compiling as metadata ----
assigntype()
assignsubject()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/hcaog_metadata.csv',
  row.names = FALSE)



#######################################
## 13. LakeAPC  ----
agency = "lakeapc"
agency_type = "COG & RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/lakeapc.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/lakeapc_metadata.csv',
  row.names = FALSE)

#######################################
## 14. MCOG  ----
agency = "mcog"
agency_type = "COG & RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/mcog.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/mcog_metadata.csv',
  row.names = FALSE)

#######################################
## 15. PCTPA  ----
agency = "pctpa"
agency_type = "CMA & RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/pctpa.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/pctpa_metadata.csv',
  row.names = FALSE)
#######################################
## 16. EDCTC  ----
agency = "edctc"
agency_type = "RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/edctc.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/edctc_metadata.csv',
  row.names = FALSE)


#######################################
## 17. CVAG  ----
host = 'http://cvag.org/'
agency = "cvag"
agency_type = "COG"
#######################################
# Library page ----
response <- GET(host, path = 'library.htm') 
page <- read_html(response) 

report_links <- html_nodes(page, css = '#contentfull a') 
report_urls <- sapply(report_links, html_attr, name = 'href') 
titles = report_links %>% html_text()


metadata <- data.frame(
  url = paste(host, report_urls, sep = ''),
  msg = '', stringsAsFactors = FALSE,
  doc_title = paste(titles, sep = ''),
  doc_type = "",
  doc_subject = "",
  agency = agency,
  agency_type = agency_type)

# Compiling as metadata ----
assigntype()
assignsubject()

metadata$doc_title = str_remove(metadata$doc_title, "^\\s+|\\s+$")

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/cvag_metadata.csv',
  row.names = FALSE)

#######################################
## 18. ICTC  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 19. LAMET  ----
agency = "lamet"
agency_type = "CTC"
#######################################
# Had to go through multiple iterations given that this was a massive amount of data ----
#df = read.csv("data/agency_webscrapes/lamet.csv")
#string = df$resources.href
#df$pdf = str_detect(string, ".pdf")
#df = df %>% filter(pdf == TRUE)
#metadata = df
#write.csv(df, "data/agency_webscrapes/lamet.csv", row.names = F)

# Edited it a bit to do the assignment
#metadata = read.csv("data/agency_webscrapes/lamet.csv")
#assigntype()
#assignsubject()
#write.csv(metadata, "data/agency_md_unedited/lamet_metdata.csv", row.names = F)


metadata = read.csv("data/agency_md_unedited/lamet_metdata2.csv")
metadata$agency = agency
metadata$agency_type = agency_type
filterdocuments()
write.csv(md.filtered, "data/agency_md_unedited/lamet_metdata3.csv", row.names = F)


#######################################
## 20. OCCOG  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 21. RCTC  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 22. VCTC  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 23. SBCTA  ----
agency = "sbcta"
agency_type = "CMA & COG & CTC"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/sbcta.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/sbcta_metadata.csv',
  row.names = FALSE)

#######################################
## 24. TTD  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 25. ABAG  ----
#######################################

# HAVEN'T DONE YET

#######################################
## 26. OCTA ----
agency = "octa"
agency_type = "CTC"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/octa.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/octa_metadata.csv',
  row.names = FALSE)


#######################################
## 27. SBCCOG  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 28. WRCOG  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 29. LCPW  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 30. DNLTC  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 31. MODOC ----
agency = "modoc"
agency_type = "RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/modoc.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()
assignsubject()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/modoc_metadata.csv',
  row.names = FALSE)

#######################################
## 32. SCLTC  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 33. TEHAMA  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 34. NCTC ----
agency = "nctc"
agency_type = "CTC & RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/nctc.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/nctc_metadata.csv',
  row.names = FALSE)

#######################################
## 35. YOLO  ----
#######################################
# I created this all by hand, and made it straight into an edited document



#######################################
## 36. MPW  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 37. NCTPA ----
agency = "nctpa"
agency_type = "CMA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/nctpa.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/nctpa_metadata.csv',
  row.names = FALSE)
# Addition

#######################################
## 38. SFCTA ----
agency = "sfcta"
agency_type = "CMA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/sfcta.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/sfcta_metadata.csv',
  row.names = FALSE)

#######################################
## 39. TAM  ----
agency = "tam"
agency_type = "STA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/tam.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/tam_metadata.csv',
  row.names = FALSE)

#######################################
## 40. INYO  ----
agency = "inyo"
agency_type = "RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/inyo.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()
assignsubject()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/inyo_metadata.csv',
  row.names = FALSE)

#######################################
## 41. MCLTC  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 42. ACLTC  ----
#######################################
# I created this all by hand, and made it straight into an edited document
#######################################
## 43. AMADOR  ----
agency = "amador"
agency_type = "RTPA"
#######################################
# All manually scraped ----
df = read.csv("data/agency_webscrapes/amador.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()
assignsubject()

## Create a csv file that has the metadata
write.csv(
  metadata, 'data/agency_md_unedited/amador_metadata.csv',
  row.names = FALSE)






#######################################
## 44. CONTRA COASTA (Forgotten in the first round)  ----
agency = "ccta"
agency_type = "CMA"
#######################################
# All manually scraped ----
df = read.csv("/Users/lizawood/Box/truckee/data/agency_webscrapes/ccta.csv")
df$agency = agency
df$agency_type = agency_type

# Compiling as metadata ----
metadata = df
assigntype()
assignsubject()

## Create a csv file that has the metadata
write.csv(
  metadata, '/Users/lizawood/Box/truckee/data/agency_md_unedited/ccta_metadata.csv',
  row.names = FALSE)




