# Agency doc descriptives `journal_and_agency_cleaning.R`, then cleaned by merging_doc_cit_metadata.R, then by agency_cleaning.
library(dplyr)
library(lubridate)
library(data.table)

df <- fread("/Users/lizawood/Box/truckee/data/journal_descriptives_data/citations_theme_docmetadata_agencyclean.csv")


# ---------------------------------------------------------------------#
# Create NAs where there are blanks, except for in the date column
# ---------------------------------------------------------------------#
## need to identify which are dates and throwing off the function below
sapply(df, class)
sapply(df, function(x) sum(is.na(x)))
replaceNA <- function(x){ifelse(x == "", NA, x)}
df[,c(1:14, 16:62)] <- data.frame(sapply(df[,c(1:14, 16:62)], replaceNA))
sapply(df, function(x) sum(is.na(x)))

# ---------------------------------------------------------------------#
# Create a variable indicating whether or not there is a potential citation here
# ---------------------------------------------------------------------#
df <- df %>% 
  mutate(potential.citation = !is.na(i))

# ---------------------------------------------------------------------#
## Remove Federal Register as a journal
# ---------------------------------------------------------------------#
df$journalpub_match <- ifelse(is.na(df$cit_journal), df$journalpub_match,
                       ifelse(df$cit_journal == "Federal Register", F,
                                     df$journalpub_match))

# ---------------------------------------------------------------------#
# Removing ridiculous one word journals
# ---------------------------------------------------------------------#
## When later inspecting documents that have just one citation, many have journals with just weird single names, and seem to be mistakes. Here I identify one word journals, then make a list to remove them from a publication list
one.word.journals <- df %>% 
  filter(journalpub_match == T) %>% 
  mutate(oneword = case_when(
    str_detect(cit_journal, "^[A-Za-z]*$") == T ~ T,
    T ~ F)) %>% 
  filter(oneword == T)

onewords <- c("History", "Information", "Notes", "Order", "Planning", "Population", "Refuge", "Stat", "South", "Source", "Signs", "Significance", "Stahlbau", "Time", "Test", "Economics", "Review")

df$journalpub_match <- ifelse(df$cit_journal %in% onewords, F,
                              df$journalpub_match)

# ---------------------------------------------------------------------#
# Correcting any off years
# ---------------------------------------------------------------------#
lubridate::as
df$cit_year <- as.numeric(df$cit_year)
df$doc_created_yr <- year(df$doc_created)
##  I have also looked at years and there are a few documents with the year 2106. I will look these up by hand to amend.
doc2106 <- df %>% 
  filter(doc_created_yr == 2106, journalpub_match == T | agency_citation == T)
unique(doc2106$doc_title)

## I am updated only those that are matches, otherwise it is a waste of time for now
df$doc_created_yr <- ifelse(#df$doc_created == "2106-02-07 06:28:15 UTC" & 
  df$doc_created_yr == 2106 &
    df$doc_title == "APPENDIX_A:_Characterization_of_Imperviousness_Study", 
  2002, 
  ifelse(#df$doc_created == "2106-02-07 06:28:15 UTC" & 
    df$doc_created_yr == 2106 &
      df$doc_title == "2018_Final_Environmental_Impact_Report_for_the_Madera_County_Regional_Transportation_Plan/Sustainable_Communities_Strategy", 
    2018,       
    ifelse(#df$doc_created == "2106-02-07 06:28:15 UTC" & 
      df$doc_created_yr == 2106 &
        df$doc_title == "Fault_Investigation_Report", 
      2011,
      ifelse(#df$doc_created == "2106-02-07 06:28:15 UTC" & 
        df$doc_created_yr == 2106 &
          df$doc_title =="San_Diego_Regional_Energy_Strategy,_2003",
        2003,
        ifelse(#df$doc_created == "2106-02-07 06:28:15 UTC" & 
          df$doc_created_yr == 2106 &
            df$doc_title =="State_Route_78_Corridor_Study,_May_2012",
          2012,
          ifelse(#df$doc_created == "2106-02-07 06:28:15 UTC" & 
            df$doc_created_yr == 2106 &
              df$doc_title =="Task_1039_-_ITS-Davis_–_Hydrogen_Pathways_Program_PDF",
            2008,
            ifelse(#df$doc_created == "2106-02-07 06:28:15 UTC" & 
              df$doc_created_yr == 2106 &
                df$doc_title =="2018_Regional_Transportation_Plan/Sustainable_Communities_Strategy_Amendment_No._1",
              2018,
              df$doc_created_yr)))))))

doc2106 <- df %>% 
  filter(doc_created_yr == 2106, journalpub_match == T | agency_citation == T)
unique(doc2106$doc_title)

## I also want to look at the documents that have a longer than 75 year lag time, to see if they are legit                  
df$lagtime <- df$doc_created_yr - df$cit_year
over75 <- df %>% filter(lagtime > 75)
summary(over75$doc_created_yr)
summary(over75$cit_year)
hist(df$cit_year)

## It looks like 1902 is a more reasonable cutoff -- the others look like legit references, but the extractor grabbed the historical reference date, not publication date, so I will just replace them with an NA
df$cit_year <- ifelse(df$cit_year > 1901,  
                      df$cit_year, NA)

## Now we can re-check these over 75 year lag times
df$lagtime <- df$doc_created_yr - df$cit_year
over75 <- df %>% filter(lagtime > 75)
summary(df$cit_year)
hist(df$cit_year)

# ---------------------------------------------------------------------#
# Defaulting to journal citations when agency and journal overlap
# ---------------------------------------------------------------------#
table(df$journalpub_match, df$agency_citation)
df$agency_citation[is.na(df$agency_citation)] <- F
df$journalpub_match[is.na(df$journalpub_match)] <- F

# If there is an overlap, then default it to a journal
df$agency_citation <- ifelse(df$journalpub_match == T & df$agency_citation == T, 
                             F, df$agency_citation)

# This should show 0 overlap
table(df$journalpub_match, df$agency_citation) 

# ---------------------------------------------------------------------#
# Create a variable indicated whether or not that citation has been identified. 
# ---------------------------------------------------------------------#
## Right now I am sticking only with journalpub and agency, but in future could include media and org
df <- df %>% 
  mutate(identified.citation = case_when(
    journalpub_match == T |
      agency_citation == T ~ T,
    TRUE ~ F))

saveRDS(df, "data/clean-df.RDS")
