# IF I ever actually run this again, correct the filtering function to do this:
# I should say  remove anything ending with xlsx, jpeg, etc., if any url does not end in pdf, attach pdf, if there is any white space, put in % *******

# Set-up ----
library(dplyr)
library(stringr)
library(rvest) 
library(httr) 
setwd("~/Box/truckee/")
source("code/agency_scraping/functions.R")


#######################################
# STATE
#######################################

# CalTrans  ----
## Create a directory to store the downloads
metadata_dir <- '/Users/lizawood/Box/truckee/documents/state_documents'
dir.create(metadata_dir, showWarnings = TRUE)

## Upload the edited, partially raw metadata
metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/caltrans_edit.csv")

## Run the above function to filter out unwanted documents, and run the download loop
filterdocuments()

## Check in on it before going through it again
for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

## Create a csv file that has the metadata
write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/state_metadata/caltrans.csv',
  row.names = FALSE)




#######################################
# MPOs
#######################################

# AMBAG  ----
## Create a directory to store the downloads
metadata_dir <- '/Users/lizawood/Box/truckee/documents/mpo_documents/ambag'
dir.create(metadata_dir, showWarnings = TRUE)

## Upload the edited, partially raw metadata
metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/ambag_edit.csv")

## Run the above function to filter out unwanted documents, and run the download loop
filterdocuments()


## Check in on it before going through it again
for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

## Create a csv file that has the metadata
write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/mpo_metadata/ambag.csv',
  row.names = FALSE)


# BCAG  ----
metadata_dir <- 'documents/mpo_documents/bcag'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("data/agency_md_edited/bcag_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, 'metadata/mpo_metadata/bcag.csv',
  row.names = FALSE)

# FCOG ----
metadata_dir <- 'documents/mpo_documents/fcog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("data/agency_md_edited/fcog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, 'metadata/mpo_metadata/fcog.csv',
  row.names = FALSE)

# KernCOG ----

## Create a directory to store the downloads
metadata_dir <- 'documents/mpo_documents/kerncog'
dir.create(metadata_dir, showWarnings = TRUE)

## Upload the edited, partially raw metadata
metadata = read.csv("data/agency_md_edited/kerncog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, 'metadata/mpo_metadata/kerncog.csv',
  row.names = FALSE)


# KingsCOG ----

## Create a directory to store the downloads
metadata_dir <- 'documents/mpo_documents/kingscog'
dir.create(metadata_dir, showWarnings = TRUE)


#metadata = read.csv("data/agency_md_edited/kingscog_edit.csv")
#assigntype()
#write.csv(metadata, "data/agency_md_edited/kingscog_edit.csv", row.names = FALSE)

metadata = read.csv("data/agency_md_edited/kingscog_edit.csv")
filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, 'metadata/mpo_metadata/kingscog.csv',
  row.names = FALSE)

# MCAG ----

## Create a directory to store the downloads
metadata_dir <- '/Users/lizawood/Box/truckee/documents/mpo_documents/mcag'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/mcag_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  docname <- paste(i, basename(url), sep = '_')
  destfile <- file.path(metadata_dir, paste0(docname, '.pdf'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}


write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/mpo_metadata/mcag.csv',
  row.names = FALSE)

# MCTC ----

## Create a directory to store the downloads
metadata_dir <- '/Users/lizawood/Box/truckee/documents/mpo_documents/mctc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/mctc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/mpo_metadata/mctc.csv',
  row.names = FALSE)

# MTC ----

## Create a directory to store the downloads
metadata_dir <- '/Users/lizawood/Box/truckee/documents/mpo_documents/mtc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/mtc_edit.csv")

filterdocuments()

# I get "SSL peer certificate or SSH remote key was not OK" for many documents, which might mean I am being blocked. If I try in smaller sets, this may work
subset1 = md.filtered[1:61,]
for (i in 1:nrow(subset1)) {
  url <- subset1[[i, 'url']]
  doc_id <- subset1[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){subset1[[i, 'msg']] <<- conditionMessage(e)}
  )
}

subset2 = md.filtered[62:105,]
for (i in 1:nrow(subset2)) {
  url <- subset2[[i, 'url']]
  doc_id <- subset2[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){subset2[[i, 'msg']] <<- conditionMessage(e)}
  )
}

subset3 = md.filtered[106:165,]
for (i in 1:nrow(subset3)) {
  url <- subset3[[i, 'url']]
  doc_id <- subset3[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){subset3[[i, 'msg']] <<- conditionMessage(e)}
  )
}

md.filtered = rbind(subset1, subset2, subset3)
write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/mpo_metadata/mtc.csv',
  row.names = FALSE)

# SACOG ----

## Create a directory to store the downloads
metadata_dir <- 'documents/mpo_documents/sacog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("data/agency_md_edited/sacog_edit.csv")

metadata = metadata %>% filter(!(doc_subject == "ruralurbanconnection"))
                              
filterdocuments()
# take away the number after the pdf
md.filtered$url = str_remove(md.filtered$url, "(?<=pdf).*")

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, 'metadata/mpo_metadata/sacog.csv',
  row.names = FALSE)

# SANDAG  ----

## Create a directory to store the downloads
metadata_dir <- 'documents/mpo_documents/sandag'
dir.create(metadata_dir, showWarnings = TRUE)


#metadata = read.csv("data/md.unedited/sandag_metadata.csv")
#assigntype()
#assignsubject()
#
#metadata$keep = str_detect(metadata$doc_title, "CJ")
#metadata = metadata %>% filter(keep == F)
#metadata$keep = str_detect(metadata$doc_title, "Hoja")
#metadata = metadata %>% filter(keep == F)
#metadata$keep = str_detect(metadata$doc_title, "[Mm]ethamphetamine|[Dd]rug|[Cc]rime|[Vv]iolence|[Pp]olice")
#metadata = metadata %>% filter(keep == F)
#metadata$keep = str_detect(metadata$doc_title, "COVID-19")
#metadata = metadata %>% filter(keep == F)
#
#write.csv(metadata, "data/agency_md_edited/sandag_edit.csv", row.names = FALSE)

metadata = read.csv("data/agency_md_edited/sandag_edit.csv")
filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, 'metadata/mpo_metadata/sandag.csv',
  row.names = FALSE)
# SBCAG ----

## Create a directory to store the downloads
metadata_dir <- 'documents/mpo_documents/sbcag'
dir.create(metadata_dir, showWarnings = TRUE)

#metadata = read.csv("data/md.unedited/sbcag_metadata.csv")
#assignsubject()
#assigntype()
#write.csv(metadata, "data/agency_md_edited/sbcag_edit.csv", row.names = F)

metadata = read.csv("data/agency_md_edited/sbcag_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, 'metadata/mpo_metadata/sbcag.csv',
  row.names = FALSE)

# SCAG ----

## Create a directory to store the downloads
metadata_dir <- 'documents/mpo_documents/scag'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("data/agency_md_edited/scag_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, 'metadata/mpo_metadata/scag.csv',
  row.names = FALSE)

# SJCOG ----

## Create a directory to store the downloads
metadata_dir <- '/Users/lizawood/Box/truckee/documents/mpo_documents/sjcog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/sjcog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  docname <- paste(i, basename(url), sep = '_')
  destfile <- file.path(metadata_dir, paste0(docname, '.pdf'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}


write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/mpo_metadata/sjcog.csv',
  row.names = FALSE)

# SLOCOG ----

## Create a directory to store the downloads
metadata_dir <- '/Users/lizawood/Box/truckee/documents/mpo_documents/slocog'
dir.create(metadata_dir, showWarnings = TRUE)

#metadata = read.csv("data/agency_md_edited/slocog_edit.csv")
#assigntype()
#write.csv(metadata, "data/agency_md_edited/slocog_edit.csv", row.names = F)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/slocog_edit.csv")
metadata$url = str_remove(metadata$url, "(?<=pdf).*")

filterdocuments()


for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/mpo_metadata/slocog.csv',
  row.names = FALSE)


# SRTA ----

## Create a directory to store the downloads
metadata_dir <- '/Users/lizawood/Box/truckee/documents/mpo_documents/srta'
dir.create(metadata_dir, showWarnings = TRUE)

#metadata = read.csv("data/agency_md_edited/srta_edit.csv")
#assigntype()
#write.csv(metadata, "data/agency_md_edited/srta_edit.csv", row.names = F)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/srta_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  docname <- paste(i, basename(url), sep = '_')
  destfile <- file.path(metadata_dir, paste0(docname, '.pdf'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}


write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/mpo_metadata/srta.csv',
  row.names = FALSE)

# StanCOG ----

## Create a directory to store the downloads
metadata_dir <- 'documents/mpo_documents/stancog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("data/agency_md_edited/stancog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, 'metadata/mpo_metadata/stancog.csv',
  row.names = FALSE)

# TCAG ----

## Create a directory to store the downloads
metadata_dir <- '/Users/lizawood/Box/truckee/documents/mpo_documents/tcag'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/tcag_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  docname <- paste(i, basename(url), sep = '_')
  destfile <- file.path(metadata_dir, paste0(docname, '.pdf'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}


write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/mpo_metadata/tcag.csv',
  row.names = FALSE)


# TRPA ----

## Create a directory to store the downloads
metadata_dir <- 'documents/mpo_documents/trpa'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("data/agency_md_edited/trpa_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  destfile <- file.path(metadata_dir, paste(i, basename(url), sep = '_'))
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, 'metadata/mpo_metadata/trpa.csv',
  row.names = FALSE)



#######################################
# Counties
#######################################

## 1. SanBenitoCOG  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/sanbenitocog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/sanbenitocog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}


write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/sanbenitocog.csv',
  row.names = FALSE)

## 2. SCCRTC  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/sccrtc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/sccrtc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/sscrtc.csv',
  row.names = FALSE)

## 3. TAMC  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/tamc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/tamc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/tamc.csv',
  row.names = FALSE)

## 4. TCTC  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/tctc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/tctc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/tctc.csv',
  row.names = FALSE)

## 5. ACTC  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/actc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/actc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/actc.csv',
  row.names = FALSE)
## 6. CCAG_sanmateo  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/ccagsanmateo'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/ccagsanmateo_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/ccagsanmateo.csv',
  row.names = FALSE)

## 7. NVTA  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/nvta'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/nvta_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/nvta.csv',
  row.names = FALSE)

## 8. VTA  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/vta'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/vta_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/vta.csv',
  row.names = FALSE)

## 9. STA  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/sta'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/sta_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/sta.csv',
  row.names = FALSE)

## 10. SCTA  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/scta'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/scta_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/scta.csv',
  row.names = FALSE)

## 11. CalaCOG  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/calacog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/calacog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/calacog.csv',
  row.names = FALSE)

## 12. HCAOG  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/hcaog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/hcaog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/hcaog.csv',
  row.names = FALSE)

## 13. LakeAPC  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/lakeapc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/lakeapc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/lakeapc.csv',
  row.names = FALSE)

## 14. MCOG  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/mcog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/mcog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/mcog.csv',
  row.names = FALSE)

## 15. PCTPA  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/pctpa'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/pctpa_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/pctpa.csv',
  row.names = FALSE)

## 16. EDCTC  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/edctc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/edctc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/edctc.csv',
  row.names = FALSE)

## 17. CVAG  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/cvag'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/cvag_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/cvag.csv',
  row.names = FALSE)

## 18. ICTC  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/ictc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/ictc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/ictc.csv',
  row.names = FALSE)

## 19. LAMET  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/lamet'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/lamet_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/lamet.csv',
  row.names = FALSE)

## 20. OCCOG  ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/occog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/occog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/occog.csv',
  row.names = FALSE)
## 21. RCTC ----

metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/rctc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/rctc_edit.csv")
colnames(metadata)[1] <- 'url'

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/rctc.csv',
  row.names = FALSE)

## 22. VCTC ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/vctc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/vctc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/vctc.csv',
  row.names = FALSE)


## 23. SBCTA ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/sbcta'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/sbcta_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/sbcta.csv',
  row.names = FALSE)

## 24. TTD ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/ttd'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/ttd_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/ttd.csv',
  row.names = FALSE)



## 26. OCTA ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/octa'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/octa_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/octa.csv',
  row.names = FALSE)


## 27. SBCCOG ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/sbccog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/sbccog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/sbccog.csv',
  row.names = FALSE)
## 28. WRCOG ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/wrcog'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/wrcog_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/wrcog.csv',
  row.names = FALSE)

## 29. LCPW ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/lcpw'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/lcpw_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/lcpw.csv',
  row.names = FALSE)

## 30. DNLTC ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/dnltc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/dnltc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/dnltc.csv',
  row.names = FALSE)
## 31. MODOC ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/modoc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/modoc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/modoc.csv',
  row.names = FALSE)
## 32. SCLTC -- none of these documents exist ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/scltc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/scltc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/scltc.csv',
  row.names = FALSE)
## 33. TEHAMA ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/tehama'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/tehama_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/tehama.csv',
  row.names = FALSE)
## 34. NCTC ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/nctc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/nctc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/nctc.csv',
  row.names = FALSE)
## 35. YOLO ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/yolo'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/yolo_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/yolo.csv',
  row.names = FALSE)
## 36. MPW ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/mpw'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/mpw_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/mpw.csv',
  row.names = FALSE)
## 37. NCTPA ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/nctpa'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/nctpa_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/nctpa.csv',
  row.names = FALSE)

## 39. SFCTA ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/sfcta'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/sfcta_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/sfcta.csv',
  row.names = FALSE)

## 39. TAM ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/tam'
dir.create(metadata_dir, showWarnings = TRUE)
metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/tam_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/tam.csv',
  row.names = FALSE)

## 40. INYO ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/inyo'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/inyo_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/inyo.csv',
  row.names = FALSE)

## 41. MCLTC ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/mcltc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/mcltc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/mcltc.csv',
  row.names = FALSE)

## 42. ACLTC ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/acltc'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/acltc_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/acltc.csv',
  row.names = FALSE)
## 43. AMADOR ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/amador'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/amador_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}
?tryCatch

write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/amador.csv',
  row.names = FALSE)


## 44. CCTA ----
metadata_dir <- '/Users/lizawood/Box/truckee/documents/county_documents/ccta'
dir.create(metadata_dir, showWarnings = TRUE)

metadata = read.csv("/Users/lizawood/Box/truckee/data/agency_md_edited/ccta_edit.csv")

filterdocuments()

for (i in 1:nrow(md.filtered)) {
  url <- md.filtered[[i, 'url']]
  doc_id <- md.filtered[[i, 'doc_id']]
  destfile <- file.path(metadata_dir, paste(doc_id, basename(url), sep = '_'))
  destfile <- ifelse(!(str_detect(destfile, ".[Pp][Dd][Ff]$")), paste0(destfile, ".pdf"), destfile)
  tryCatch(
    download.file(url, destfile, mode = "wb"),
    error = function(e){md.filtered[[i, 'msg']] <<- conditionMessage(e)}
  )
}


write.csv(
  md.filtered, '/Users/lizawood/Box/truckee/metadata/county_metadata/ccta.csv',
  row.names = FALSE)

