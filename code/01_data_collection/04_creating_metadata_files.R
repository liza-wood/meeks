# Set-up ----
library(dplyr)
library(tidyr)
library(data.table)
library(tm)
library(pdftools)
library(stringr)

setwd("~/Documents/Davis/R-Projects/truckee")



#########################

# MASTER METADATA > File that details each document: docID, assosciated project, pdf metadata (via textmine? or pdf_info) ----

## Project metadata: Agencies ----

### MPOS: Creating a loop to run through all documents to create one list
filenames <- list.files(path="/Users/lizawood/Box/truckee/metadata/mpo_metadata/")
docs.compiled = list()

for(i in filenames){
  filepath <- file.path("/Users/lizawood/Box/truckee/metadata/mpo_metadata",paste(i))
  file <- fread(filepath, fill=FALSE, blank.lines.skip = TRUE)
  docs.compiled[[i]] <- data.frame(file)
}

# I grabbed this from online and I am confused about what these calls means but it merges all of the lists
mpodf <- do.call("rbind",docs.compiled)
setDT(mpodf, keep.rownames = TRUE)[]

### County: Creating a loop to run through all documents to create one list
filenames <- list.files(path="/Users/lizawood/Box/truckee/metadata/county_metadata")
docs.compiled = list()

for(i in filenames){
  filepath <- file.path("/Users/lizawood/Box/truckee/metadata/county_metadata",paste(i))
  file <- fread(filepath, fill=FALSE, blank.lines.skip = TRUE)
  docs.compiled[[i]] <- data.frame(file)
}

countydf <- do.call("rbind",docs.compiled)
setDT(countydf, keep.rownames = TRUE)[]

### State: Creating a loop to run through all documents to create one list
filenames <- list.files(path="/Users/lizawood/Box/truckee/metadata/state_metadata")
docs.compiled = list()

for(i in filenames){
  filepath <- file.path("/Users/lizawood/Box/truckee/metadata/state_metadata",paste(i))
  file <- fread(filepath, fill=FALSE, blank.lines.skip = TRUE)
  docs.compiled[[i]] <- data.frame(file)
}

statedf <- do.call("rbind",docs.compiled)
setDT(statedf, keep.rownames = TRUE)[]

### Combining all levels
transport_agency_documents = rbind(countydf, mpodf, statedf)
transport_agency_documents$schid = ""

transport_agency_documents$rn.id2 = paste0(transport_agency_documents$agency, "." ,transport_agency_documents$doc_id)

## Project metadata: CEQA ----
# Cleaning CEQA document list to match agencies
ceqa_documents = read.csv("/Users/lizawood/Box/truckee/metadata/ceqa_transport_metadata.csv")

ceqa_documents = ceqa_documents %>% rename("agency" = "lead.agency", "doc_title" = "doc.title", "doc_type" = "doc.type",  "doc_id" = "doc.id") %>% select(-doc.description:-keep)
ceqa_documents$url = ""
ceqa_documents$msg = ""
ceqa_documents$doc_subject = "ceqa"
ceqa_documents$agency_type = ""
ceqa_documents$rn.id2 = ""

ceqa_docs_ordered = ceqa_documents %>% select(rn, url, msg, doc_title, doc_type, doc_subject, agency, agency_type, doc_id, schid, rn.id2)


## Binding agency and CEQA documents ----
document.list = rbind(transport_agency_documents, ceqa_docs_ordered)

## Creating an id based on the rowname given
document.list$rn.id = gsub(".csv", "", document.list$rn)
# Assigning numbers here means that we cannot match documents later, but for now this is cleanest
document.list$rn.id = ifelse(str_detect(document.list$rn.id, "ceqa"), paste(document.list$schid, document.list$doc_id, sep = "."), document.list$rn.id)

# Write csv real quick ----
write.csv(document.list, "/Users/lizawood/Box/truckee/data/agency_doc_metadata/documentlist_nocleaning.csv", row.names = F)


## Cleaning document subject categories ----

document.list = read.csv("/Users/lizawood/Box/truckee/data/agency_doc_metadata/documentlist_nocleaning.csv")

document.list$doc_subject = ifelse(document.list$doc_subject == "aq", "cmaq", 
                           ifelse(document.list$doc_subject == "tranit", "transit",
                           ifelse(document.list$doc_subject == "sust", "sustainability",
                           ifelse(document.list$doc_subject %in% c("blueprint","greenprint"), "print",
                           ifelse(document.list$doc_subject %in% c("environmental","envt","ser"), "environment",
                           ifelse(document.list$doc_subject == "ppp", "outreach",
                           ifelse(document.list$doc_subject == "Nafreight", "freight",
                           ifelse(document.list$doc_subject == "mtip", "rtip",
                           ifelse(document.list$doc_subject == "rorad", "road",
                           ifelse(document.list$doc_subject == "bugdet", "remove",
                           ifelse(document.list$doc_subject == "cmaq ", "cmaq",
                           ifelse(document.list$doc_subject == "safety_community", "safety",
                           ifelse(document.list$doc_subject == "models", "model",
                           ifelse(document.list$doc_subject == "ej", "sustainability",
                           ifelse(document.list$doc_subject == "system_information", "system_info",
                           ifelse(document.list$doc_subject %in% c("budget", "port"), "remove",
                           document.list$doc_subject))))))))))))))))

# tda = transportation development act --> unsure what to do here
# ser = standard environmental reference, but lumped under environment generally

# Assigning these last few

string = which(document.list$doc_subject == "")
document.list[string]
# This is like an appendix of different kinds of zoning -- apartments, stores, etc, so I will call it an hna appendix
document.list$doc_subject[string] = "hna"
document.list$doc_type[string] = "appendix"

string = which(document.list$doc_subject == "ap")
document.list[string]
string
document.list$doc_subject[3992] = "road"
document.list$doc_subject[3993] = "sustainability"

string = which(document.list$doc_subject == "ar") # remove annual report
document.list[string]
document.list$doc_subject[string] = "remove"

string = which(document.list$doc_subject == "eir")
document.list$doc_title[string]
document.list$doc_subject[string] = "rtp"

string = which(document.list$doc_subject == "grant")
document.list[string]
document.list$doc_subject[string] = "remove"

string = which(document.list$doc_subject == "other")
document.list[string]
string
document.list$doc_subject[c(285, 286, 1765)] = "remove"
document.list$doc_subject[3435] = "climate_change"
document.list$doc_type[3435] = "report"
document.list$doc_subject[3504] = "growth"
document.list$doc_type[3504] = "study"
document.list$doc_subject[3507] = "road"
document.list$doc_subject[c(3756, 3759, 3804, 3885)] = "at"
document.list$doc_subject[4944] = "mobility"


string = which(document.list$doc_subject == "policy")
document.list[string]
document.list$doc_subject[string] = "remove"

string = which(document.list$doc_subject == "sp")
document.list[string]
document.list$doc_subject[string] = "comprehensive"

string = which(document.list$doc_subject == "stp")
document.list$doc_title[string]
document.list$doc_subject[string] = "rtp"

string = which(document.list$doc_subject == "tp")
document.list$doc_title[string]
document.list$doc_subject[string] = "stp" # State transportation plan

string = which(document.list$doc_subject == "trn")
document.list[string]
document.list$doc_subject[string] = "ctp"

string = which(document.list$doc_subject == "tda")
document.list[string]
document.list$doc_subject[string] = "mobility"

document.list = document.list %>% filter(!(doc_subject == "remove"))


# state documents of research: design, district, equipment, executive, geotech, maintenance, modal, pavement, rightofway, rural, seismic, system_info, tsm are all iterations of this
string = which(document.list$doc_subject == "design") # This looks like a bit of everything
document.list$doc_title[string]
string = which(document.list$doc_subject == "district")
document.list$doc_title[string]
string = which(document.list$doc_subject == "equipment")
document.list$doc_title[string]
string = which(document.list$doc_subject == "executive")
document.list$doc_title[string]
string = which(document.list$doc_subject == "geotech")
document.list$doc_title[string]
string = which(document.list$doc_subject == "maintenance")
document.list$doc_title[string]
string = which(document.list$doc_subject == "modal")
document.list$doc_title[string]
string = which(document.list$doc_subject == "pavement")
document.list$doc_title[string]
string = which(document.list$doc_subject == "rightofway")
document.list$doc_title[string]
string = which(document.list$doc_subject == "rural")
document.list$doc_title[string]
string = which(document.list$doc_subject == "seismic")
document.list$doc_title[string]
string = which(document.list$doc_subject == "system_info")
document.list$doc_title[string]
string = which(document.list$doc_subject == "tsm") # safety
document.list$doc_title[string]


## Cleaning document type categories ----

document.list$doc_type = ifelse(document.list$doc_type %in% c("amendmend","administration" ,"brief" ,
                                                            "certification" ,"comments" ,"committee" ,
                                                            "contract" ,"correspondence" ,"database" ,
                                                            "design" ,"fact_sheet" ,"grant" ,"grants" ,
                                                            "letters" ,"maps" ,"meeting","plan_amednment", 
                                                            "plan_amendment"), "remove", 
                        ifelse(document.list$doc_type %in% c("eia","eir_draft"), "eir", 
                        ifelse(document.list$doc_type %in% c("guidance" ,"guide" ,"guideines" ,
                                                            "guidelines_design" ,"handbook" ,"manual"), "guidelines", 
                        ifelse(document.list$doc_type == "method", "methods",
                        ifelse(document.list$doc_type == "outreach", "ppp",
                        ifelse(document.list$doc_type == "paper", "report",      
                        ifelse(document.list$doc_type %in% c("plab" ,"plan_draft" ,"plan_resolution", 
                                                            "print"), "plan",
                        ifelse(document.list$doc_type %in% c("plan_appendix" ,"program_appendix" ,
                                                            "report_appendix" ,"study_appendix"), "appendix",
                        ifelse(document.list$doc_type == "projects", "project",
                        ifelse(document.list$doc_type == "plan_methods", "methods",
                        ifelse(document.list$doc_type %in% c("plan_summary" ,"program_summary" ,
                                                           "report_summary" ,"study_summary"), "summary",
                        ifelse(document.list$doc_type == "presentaton", "presentation",
                        ifelse(document.list$doc_type == "scs", "plan",
                        ifelse(document.list$doc_type == "solicitation", "remove",
                        ifelse(document.list$doc_type %in% c("studt" ,"survey"), "study",
                        ifelse(document.list$doc_type == "survey", "study",
                        ifelse(document.list$doc_type == "toolbox", "remove",
                        document.list$doc_type)))))))))))))))))

string = which(document.list$doc_type == "")
document.list[string]
string
document.list$doc_type[c(771, 772, 951, 994, 1252)] = "remove"
document.list$doc_type[c(955, 1028, 1029)] = "report"
document.list$doc_type[c(1081, 1082)] = "guidelines"

string = which(document.list$doc_type == "other")
document.list[string]
string
document.list$doc_type[c(1591, 2522, 3166, 3167, 3573, 3574, 4849)] = "remove"
document.list$doc_type[c(3165, 3168, 3575)] = "guidelines"
document.list$doc_type[3773] = "report"

# If a ppp, I want it to be outreach and as a plan
document.list$doc_subject = ifelse(document.list$doc_type == "ppp", "outreach", document.list$doc_subject) 
document.list$doc_type = ifelse(document.list$doc_type == "ppp", "plan", document.list$doc_type) 


table(document.list$doc_type)
# Agency types: appendix, eir, guidelines, methods, model, plan, policy, presentation, program, project, report, research, research_prelim, study, summary -- 15 types
# CEQA: 24 types

document.list = document.list %>% filter(!(doc_type == "remove"))

# Final number of transport entity documents listed
document.list %>% filter(!(doc_subject == "ceqa")) #5571
document.list %>% filter(!(doc_subject == "ceqa")) %>% filter(is.na(msg)) #5325

### Create a normal order
document.list$agency_code = document.list$agency
document.list = document.list %>% select(rn.id, doc_title, doc_subject, doc_type, agency_code, url, msg, schid, doc_id, rn.id2)


## Cleaning agencies names  ----
table(document.list$agency_code)
document.list$agency_code[document.list$agency_code == "ccagsanmeteo"] <- "ccagsanmateo"
document.list$agency_code[document.list$agency_code == "hcoag"] <- "hcaog"
document.list$agency_code[document.list$agency_code == ""] <- "bcag"

transport_agencies = read.csv("/Users/lizawood/Box/truckee/data/agency_doc_metadata/masterlist.csv")

transport_agencies = separate(transport_agencies, agency, into = c("agency", "agency_code"), sep =  "\\s*\\(")
transport_agencies$agency_code = gsub("\\)","", transport_agencies$agency_code)
transport_agencies$agency_code[transport_agencies$agency_code == "srtpa"] <- "srta"
transport_agencies_short = transport_agencies %>% select(agency, agency_code, agency_type, staff, population)

ceqa_agencies = read.csv("/Users/lizawood/Box/truckee/metadata/ceqa_transport_metadata.csv")

ceqa_agencies = ceqa_agencies %>% rename("agency" = "lead.agency") %>% select(agency) %>% distinct()
ceqa_agencies$agency_code = NA
ceqa_agencies$agency_type = NA
ceqa_agencies$staff = NA
ceqa_agencies$population = NA

same = intersect(colnames(transport_agencies_short), colnames(ceqa_agencies))
# Merged the two together and combine what columns they have in common, if the values are the same
agency_list = merge(ceqa_agencies, transport_agencies_short, by = same, all = T)
#agency_list$duplicate = ifelse(duplicated(agency_list$agency) & is.na(agency_list$agency_code), T, F)
agency_list$agency_code = ifelse(is.na(agency_list$agency_code), agency_list$agency, agency_list$agency_code)
colnames(agency_list) = c("agency","agency_code","agency_type","agency_staff","agency_population")

document.list2 = left_join(document.list, agency_list, by = "agency_code")

document.list2$agency[document.list2$agency == "Alpine County Public Works"] <- "Alpine County"
document.list2$agency[document.list2$agency == "Caltrans, District 1 - Eureka"] <- "Caltrans 1 (Eureka)"
document.list2$agency[document.list2$agency == "Caltrans, District 10 - Stockton"] <- "Caltrans 10 (Stockton)"
document.list2$agency[document.list2$agency == "Caltrans, District 11 - San Diego"] <- "Caltrans 11 (San Diego)"
document.list2$agency[document.list2$agency == "Caltrans, District 12 - Orange"] <- "Caltrans 12 (Orange)"
document.list2$agency[document.list2$agency == "Caltrans, District 4 - Bay Area/Oakland"] <- "Caltrans 4 (Oakland)"
document.list2$agency[document.list2$agency == "Caltrans, District 5 - San Luis Obispo/Santa Barbara"] <- "Caltrans 5 (San Luis Obispo)"
document.list2$agency[document.list2$agency == "Caltrans, District 6 - Fresno/Bakersfield"] <- "Caltrans 6 (Fresno)"
document.list2$agency[document.list2$agency == "Caltrans, District 7 - Los Angeles"] <- "Caltrans 7 (Los Angeles)"
document.list2$agency[document.list2$agency == "Caltrans, District 9 - Bishop"] <- "Caltrans 9 (Bishop)"
document.list2$agency[document.list2$agency == "Fresno Council of Governments"] <- "Fresno County Council of Governments"
document.list2$agency[document.list2$agency == "Kern Council of Governments"] <- "Kern County Council of Governments"
document.list2$agency[document.list2$agency == "Lake County Public Works"] <- "Lake County"
document.list2$agency[document.list2$agency == "San Benito County Governments"] <- "San Benito County Council of Governments"
document.list2$agency[document.list2$agency == "San Benito Council of Governments"] <- "San Benito County Council of Governments"
document.list2$agency[document.list2$agency == "San Francisco County Transportation Authority, CMA"] <- "San Francisco County Transportation Authority"
document.list2$agency[document.list2$agency == "San Jaoquin County Council of Governments"] <- "San Jaoquin County Council of Governments"
document.list2$agency[document.list2$agency == "Tahoe Regional Planning"] <- "Tahoe Regional Planning Agency"
document.list2$agency[document.list2$agency == " Transportation, United States Department of"] <- "Transportation, United States Department of"

# Need to update any changes I make to CEQA stuff because their code in their name as well
document.list2$agency_code[document.list2$agency_code == "Caltrans, District 1 - Eureka"] <- "Caltrans 1 (Eureka)"
document.list2$agency_code[document.list2$agency_code == "Caltrans, District 10 - Stockton"] <- "Caltrans 10 (Stockton)"
document.list2$agency_code[document.list2$agency_code == "Caltrans, District 11 - San Diego"] <- "Caltrans 11 (San Diego)"
document.list2$agency_code[document.list2$agency_code == "Caltrans, District 12 - Orange"] <- "Caltrans 12 (Orange)"
document.list2$agency_code[document.list2$agency_code == "Caltrans, District 4 - Bay Area/Oakland"] <- "Caltrans 4 (Oakland)"
document.list2$agency_code[document.list2$agency_code == "Caltrans, District 5 - San Luis Obispo/Santa Barbara"] <- "Caltrans 5 (San Luis Obispo)"
document.list2$agency_code[document.list2$agency_code == "Caltrans, District 6 - Fresno/Bakersfield"] <- "Caltrans 6 (Fresno)"
document.list2$agency_code[document.list2$agency_code == "Caltrans, District 7 - Los Angeles"] <- "Caltrans 7 (Los Angeles)"
document.list2$agency_code[document.list2$agency_code == "Caltrans, District 9 - Bishop"] <- "Caltrans 9 (Bishop)"

## Assigning agency details ----
document.list2$agency_type = str_replace(document.list2$agency_type, " &", ",")
document.list2$mpo = ifelse(str_detect(document.list2$agency_type,"MPO") == TRUE, T, F)
document.list2$rtpa = ifelse(str_detect(document.list2$agency_type,"RTPA") == TRUE, T, F)
document.list2$ctc = ifelse(str_detect(document.list2$agency_type,"CTC") == TRUE, T, F)
document.list2$cma = ifelse(str_detect(document.list2$agency_type,"CMA") == TRUE, T, F)
document.list2$cog = ifelse(str_detect(document.list2$agency_type,"COG") == TRUE, T, F)
document.list2$transportation.authority = ifelse(str_detect(document.list2$agency,"Transportation Authority") == TRUE, T, F)
document.list2$transit.district = ifelse(str_detect(document.list2$agency,"Transit District") == TRUE, T, F)

local = str_detect(document.list2$agency, c("School District|University|College"))
city = str_detect(document.list2$agency, c("[Cc]ity|Port"))
county = str_detect(document.list2$agency, "[Cc]ounty|CTC|Public Works")
special_district = str_detect(document.list2$agency, "Governments")
regional = str_detect(document.list2$agency, c("Caltrans\\s[:digit:]|Caltrans,\\sDistrict|Joint Powers|Regional"))
regional2 = ifelse(document.list2$rtpa == TRUE | document.list2$mpo == TRUE, TRUE, FALSE)
state = str_detect(document.list2$agency, c("Caltrans|Caltrans, Planning|Caltrans, Statewide|State |Rail Authority|Veterans|Water Resources|Parks and Recreations"))
federal = str_detect(document.list2$agency, c("United States|Federal|Bureau|National|Parks and Recreation"))
tribal = str_detect(document.list2$agency, "Indians")

# Transit district and transportation authority; port; college district; 

document.list2 = document.list2 %>% 
  mutate(
    agency_level = case_when(
      county == T ~ "county",
      local == T ~ "local",
      city == T ~ "city",
      regional == T ~ "regional",
      regional2 == T ~ "regional",
      special_district == T ~ "special_district",
      transportation.authority == T ~ "regional",
      document.list2$agency == "Tahoe Transportation District" ~ "regional",
      state == T ~ "state",
      federal == T ~ "federal",
      tribal == T ~ "tribal",
      TRUE ~ "other"
    )
  )

other = document.list2 %>% select(agency, agency_level) %>% filter(agency_level == "other") %>% group_by(agency) %>% distinct()
# Other is Contra Costa TA, Napa Valley TA, Solano TA and TA of Marin, Taho Transportation District, Santa Clara Valley TA
# Almost all are just CMA,s except Marin and Tahoe, MTA, Metropolitan Transportation Comission, 
table(document.list2$agency_level)
## Save clean document ----
write.csv(document.list2, "/Users/lizawood/Box/truckee/data/agency_doc_metadata/documentlist.csv", row.names = F)


#########################

## PDF info for Agencies  ----
### MPOS  ----

# This is my own attempt, which did not work
#dir = list.dirs(path="/Users/lizawood/Box/truckee/documents/mpo_documents")
#names = gsub(pattern = "(.*)\\..*$", replacement = "\\1", basename(dir))
#names = data.frame(names[2:19])
#
## This is trying to do it all at once with TryCatch and does not work
#pdf.details = list()
#for(i in names){
#  filepath <- file.path("/Users/lizawood/Box/truckee/documents/mpo_documents",paste(i))
#  files <- list.files(filepath, full.names = T)
#  for(j in files){
#    tryCatch({
#      pdf <- pdftools::pdf_info(j)}, error=function(e) {data.frame("NA")})
#    pdf$doc <- j
#    pdf$doc <- gsub(".*documents/","",pdf$doc)
#    pdf.details[[j]] <- data.frame(pdf)
#  }
#}
#
## This is from a forum. I do not understand how this works, but it does
#pdf.details = Reduce(function(...) merge(..., all=T), pdf.details)

# Tyler's version
fls = list.files('/Users/lizawood/Box/truckee/documents/mpo_documents',recursive = T,full.names = T)
metafile = '/Users/lizawood/Box/truckee/metadata/document_metadata/mpo_pdf_info.csv'
# If the metadile exists, then execute the following
# read metafile and store it into file details, 
# then separately, assing values to fls if they are not already listed as files in the file_details$file
# If metafile doesnt' exist, then just assign an empy datatabel to file details

file_details1 = data.table()
#if(file.exists(metafile)){file_details1 <- fread(metafile);fls = fls[!fls %in% file_details1$file]}else{
#  file_details1 <- data.table()}

for(i in seq_along(fls)){
  dets = tryCatch(pdftools::pdf_info(fls[i]),error = function(e) NULL)
  if(is.null(dets)){next}
  dets = dets[c('pages','created','modified','layout')]
  dets = setDT(dets)
  dets$pages = as.numeric(dets$pages)
  dets$created = as.character(dets$created)
  dets$modified = as.character(dets$modified)
  dets$file = fls[i]
  file_details1 <- rbind(file_details1,dets,use.names = T,fill = T)
}
fwrite(file_details1, file = metafile)

### County  ----

fls = list.files('/Users/lizawood/Box/truckee/documents/county_documents',recursive = T,full.names = T)

metafile = '/Users/lizawood/Box/truckee/metadata/document_metadata/county_pdf_info.csv'

#if(file.exists(metafile)){file_details2 <- fread(metafile);fls = fls[!fls %in% file_details2$file]}else{
#  file_details2 <- data.table()}

file_details2 = data.table()

for(i in seq_along(fls)){
  dets = tryCatch(pdftools::pdf_info(fls[i]),error = function(e) NULL)
  if(is.null(dets)){next}
  dets = dets[c('pages','created','modified','layout')]
  dets = setDT(dets)
  dets$pages = as.numeric(dets$pages)
  dets$created = as.character(dets$created)
  dets$modified = as.character(dets$modified)
  dets$file = fls[i]
  file_details2 <- rbind(file_details2,dets,use.names = T,fill = T)
}

fwrite(file_details2, file = metafile)

### State  ----

fls = list.files('/Users/lizawood/Box/truckee/documents/state_documents',recursive = T,full.names = T)
metafile = '/Users/lizawood/Box/truckee/metadata/document_metadata/state_pdf_info.csv'

#if(file.exists(metafile)){file_details3 <- fread(metafile);fls = fls[!fls %in% file_details3$file]}else{
#  file_details3 <- data.table()}
file_details3 = data.table()

for(i in seq_along(fls)){
  dets = tryCatch(pdftools::pdf_info(fls[i]),error = function(e) NULL)
  if(is.null(dets)){next}
  dets = dets[c('pages','created','modified','layout')]
  dets = setDT(dets)
  dets$pages = as.numeric(dets$pages)
  dets$created = as.character(dets$created)
  dets$modified = as.character(dets$modified)
  dets$file = fls[i]
  file_details3 <- rbind(file_details3,dets,use.names = T,fill = T)
}

fwrite(file_details3, file = metafile)


### PDF info for CEQA  ----
#### Isolate all of the relevant PDFs  ----

# Reloading this from above -- here are my transport documents in metadata
ceqa_transport = read.csv("/Users/lizawood/Box/truckee/metadata/ceqa_transport_metadata.csv")
# Identifying their ids to match to files
r.schid = data.frame(ceqa_transport$schid)
r.schid = distinct(r.schid)
r.schid$transport = TRUE
colnames(r.schid) = c("stripped.id", "transport")
r.schid$stripped.id = as.character(r.schid$stripped.id)

# I have tried to include more unique identifiers, but I cannot figure out a way to join, merge, whatever so that it carries the unique ID over appropriately
#r.schid = ceqa_transport %>% select(schid, rn, doc.id)
#r.schid$transport = TRUE
#colnames(r.schid) = c("stripped.id", "rn.id", "doc.id","transport")
#r.schid$stripped.id = as.character(r.schid$stripped.id)



# Creating a new folder to copy these documents into specifically
metadata_dir <- '/Users/lizawood/Box/truckee/documents/ceqa_transport_documents/'
dir.create(metadata_dir, showWarnings = TRUE)

# List all the document files that are in the ceqa folders
ceqa_files <- list.files('/Users/lizawood/Box/truckee/documents/ceqa_documents', recursive = T)
# Give them their full name so that the file can be properly copied below
ceqa_files_full <- file.path('/Users/lizawood/Box/truckee/documents/ceqa_documents',paste(ceqa_files))
# Make a dataframe so that I can follow through with the following functions
ceqa_files_full = data.frame(ceqa_files_full)


# This might be just getting the one layer of files that are not in secondary folders. 
for(i in ceqa_files_full) {
    ceqa_files_full$stripped.id = str_extract(i, "(?<=\\d{4}/)\\d+") 
    ceqa_files_full2 = left_join(ceqa_files_full, r.schid, by = "stripped.id") 
    t.files = ceqa_files_full2 %>% filter(transport == T) %>% select(-transport)}
unique(t.files$stripped.id)

# Considered adding document numbers here but that doesn't work -- could consider by hand
#t.files2 = t.files %>% group_by(stripped.id) %>% mutate(doc.id = row_number())

# The next stage in the loop to copy of the files
for (j in t.files) {
  file.copy(j, metadata_dir)
}

#### Identifying what documents are missing ----
# Creating lists of the IDs so that I can create a list of mismatches 
t.files.id = distinct(data.frame(t.files$stripped.id))
colnames(t.files.id) = "stripped.id"
r.schid.id = data.frame(r.schid$stripped.id)
colnames(r.schid.id) = "stripped.id"
missing = anti_join(r.schid.id, t.files.id)

# Extracting the first four digits from the SCHIDs so I can identify year
ceqa_files_full2$year = str_extract(ceqa_files_full2$ceqa_files_full, "(?<=ceqa_documents/)\\d{4}")
all.files = ceqa_files_full2
summary(as.numeric(all.files$year))
summary(as.numeric(t.files$year))

# Create a table showing the spread of documents across years
t.docs.we.have = data.frame(table(t.files$year))
colnames(t.docs.we.have) = c("year", "number.we.have")
ceqa_transport$year = str_extract(ceqa_transport$schid, "^\\d{4}")
t.docs.that.should.exist = data.frame(table(ceqa_transport$year))
colnames(t.docs.that.should.exist) = c("year", "number.that.should.exist")

comparison = left_join(t.docs.that.should.exist, t.docs.we.have, by = "year")
comparison$number.we.have = ifelse(is.na(comparison$number.we.have), 0, comparison$number.we.have)
comparison$prop = comparison$number.we.have/comparison$number.that.should.exist

write.csv(comparison, "/Users/lizawood/Desktop/whats.happening.to.ceqa.docs.csv", row.names = F)

### CEQA documents (issue is that I don't have the perfect document list) ----

fls = list.files('/Users/lizawood/Box/truckee/documents/ceqa_transport_documents',recursive = T,full.names = T)
metafile = '/Users/lizawood/Box/truckee/metadata/document_metadata/ceqa_transport_pdf_info.csv'

#if(file.exists(metafile)){file_details4 <- fread(metafile);fls = fls[!fls %in% file_details3$file]}else{
#  file_details4 <- data.table()}

file_details4 <- data.table()
for(i in seq_along(fls)){
  dets = tryCatch(pdftools::pdf_info(fls[i]),error = function(e) NULL)
  if(is.null(dets)){next}
  dets = dets[c('pages','created','modified','layout')]
  dets = setDT(dets)
  dets$pages = as.numeric(dets$pages)
  dets$created = as.character(dets$created)
  dets$modified = as.character(dets$modified)
  dets$file = fls[i]
  file_details4 <- rbind(file_details4,dets,use.names = T,fill = T)
}

fwrite(file_details4, file = metafile)

## Merging and writing pdfinfo ----
allfiles = rbind(file_details1, file_details2, file_details3, file_details4) 
metafile = '/Users/lizawood/Box/truckee/metadata/document_metadata/combined_pdf_info.csv'
fwrite(allfiles, file = metafile)



#########################


## Combining Document and PDF info for Master Metadata ----
### Combining PDF info with document.list > when I do this the number increases by 591. Unclear why? ----
document.list = read.csv("/Users/lizawood/Box/truckee/data/agency_doc_metadata/documentlist.csv")
allfiles = read.csv('/Users/lizawood/Box/truckee/metadata/document_metadata/combined_pdf_info.csv')
# Get allfiles filename to match the code generated in document list, which is abbreviation, then number. As for ceqa, that will have to be different
allfiles$rn.id = str_extract(allfiles$file, "(?<=_documents/)\\w+/\\d+|(?<=_documents/)\\d+")
#If id starts with digits and has no more than 3 digits and is not followed by more digits, put caltans in front
allfiles$rn.id = ifelse(str_detect(allfiles$rn.id, "^\\d{1,3}(?!\\d)"), paste("caltrans", allfiles$rn.id, sep = "."), allfiles$rn.id) 
allfiles$rn.id = str_replace(allfiles$rn.id, "/", ".")
#document.list$rn.id = ifelse(str_detect(document.list$doc_subject, "ceqa"), document.list$schid, document.list$rn.id)

# Right now this binding just does not bind pdf info because we are not actually able to match it
allfiles$rn.id2 = allfiles$rn.id
document_metadata = left_join(document.list, allfiles, by = "rn.id")

document_metadata$duplicated = duplicated(document_metadata$rn.id)
string = which(document_metadata$duplicated == T) 

# These are STANCOG 27
document_metadata = document_metadata %>% slice(-c(4577, 4578))

#duplicates = document_metadata %>% filter(document_metadata$duplicated == T)
#document_metadata$duplicates2 = ifelse(document_metadata$rn.id %in% duplicates$rn.id, T, F)
#duplicates2 = document_metadata %>% filter(duplicates2 == T)


document_metadata = document_metadata %>% select(-schid)

## Writing final document metadata file with pdf info ----
write.csv(document_metadata, "/Users/lizawood/Box/truckee/data/agency_doc_metadata/documentlist_pdfinfo.csv", row.names = FALSE)





