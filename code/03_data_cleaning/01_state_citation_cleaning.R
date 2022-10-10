library(dplyr)
library(stringr)
library(data.table)
library(tidyr)
library(reshape2)

# Importing and merging data ----
## Import both types of extractions from anystyle -- whole and page and one that considers layout and columns
state_nolayout <- readRDS("~/Box/truckee/data/compiled_anystyle_results/state_documents_nolayout_references.RDS")
state <- readRDS("~/Box/truckee/data/compiled_anystyle_results/state_documents_references.RDS")

## Merge these two and remove original filepath started so we can recognize duplicates. Then remove exact duplicates that came up in both formatting approaches (and this would get rid if the same document mentioned a citation twice, but that is fine and probably more normals)
same <- intersect(names(state_nolayout), names(state))
combined <- full_join(state_nolayout, state, by = same) 
combined$File <- str_remove_all(combined$File, "reference_extracts_nolayout/|reference_extracts/")
combined <- distinct(combined)

# Authors, Title, Publisher ,Year, DOI,
combined <- combined %>% select(author, title, date, publisher,`container-title`, doi, url, File)
colnames(combined)[5] <- "container"

# How many of these citations have multiple titles?
#mult.titles.df <- data.table()
#for(i in seq_along(combined$title)){
#  mult.titles <- length(combined$title[[i]]) > 1
#  mult.titles.df <- rbind(mult.titles.df, mult.titles)
#}
#table(mult.titles.df)
#
## How many of these citations have multiple containers?
#mult.cont.df <- data.table()
#for(i in seq_along(combined$container)){
#  mult.cont <- length(combined$container[[i]]) > 1
#  mult.cont.df <- rbind(mult.cont.df, mult.cont)
#}
#table(mult.cont.df)

# How many of these citations have multiple dois?
#mult.doi.df <- data.table()
#for(i in seq_along(combined$doi)){
#  mult.doi <- length(combined$doi[[i]]) > 1
#  mult.doi.df <- rbind(mult.doi.df, mult.doi)
#}
#table(mult.doi.df)


# TITLE CLEANING  ----

## 1. Eliminate long titles of a single list element ----
df <- combined
df <- df %>% mutate(n.char = nchar(title))
df$too.long <- ifelse(df$n.char > 200, T, F)

mult.titles.df <- data.table()
for(i in seq_along(df$title)){
  mult.titles <- length(df$title[[i]]) > 1
  row <- i
  new.df <- data.table(mult.titles, row)
  mult.titles.df <- rbind(mult.titles.df, new.df)
}

df <- cbind(df, mult.titles.df)
df$omit <- ifelse(df$too.long == T & df$mult.titles == F, T, F)
# Only 1332 of these
df <- df %>% filter(omit == F) %>% select(-too.long, -omit, -n.char, -row)

## 2. Identify multiple titles ----
title.num.df <- data.table()
for(i in seq_along(df$title)){
  two.titles <- length(df$title[[i]]) == 2
  three.titles <- length(df$title[[i]]) == 3
  four.plus <- length(df$title[[i]]) > 3
  row <- i
  new.df <- data.table(two.titles, three.titles, four.plus, row)
  title.num.df <- rbind(title.num.df, new.df)
}

df <- cbind(df, title.num.df)

# If number of titles = number of containers = number of years

## Isolate only multiple titles and inspect 
mult.titles.only <- df %>% filter(mult.titles == T)
## There are 2848 of these

## 3. Identify titles that have agencies in the first line ----
## It looks like in many cases there titles have their first string as a department or agency name, which are better suited as their author -- used to be 910
agency.authors <- c("^\\s*([a-zA-Z]+\\s*){0,5}[Aa]genc[a-z]+\\s*([a-zA-Z]+\\s*){0,5}$", 
                    "^\\s*([a-zA-Z]+\\s*){0,2}Agricultural\\sCommissioner’s\\sOffice\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^\\s*([a-zA-Z]+\\s*){0,2}[Aa]rmy\\s[Cc]orps\\s[Oo]f\\sEngingeers$", 
                    "^\\s*([a-zA-Z]+\\s*){0,3}Association\\s[Oo]f\\sGovernments$",
                    "^\\s*([a-zA-Z]+\\s*){0,3}Administration\\s*([a-zA-Z]+\\s*){0,3}$",
                    "^\\s*([a-zA-Z]+\\s*){0,6}[Dd]epartment\\s*([a-zA-Z]+\\s*){0,7}$", 
                    "^\\s*([a-zA-Z]+\\s*){0,4}[Oo]ffice\\s*([a-zA-Z]+\\s*){0,7}$",
                    "^\\s*([a-zA-Z]+\\s*){0,5}[Dd]ept[.]*\\s*([a-zA-Z]+\\s*){0,4}$", 
                    "^\\s*([a-zA-Z]+\\s*){0,4}[Bb]oard\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^CA\\sDWR$",
                    "^California\\sAir\\sResources\\Board\\s*([a-zA-Z]+\\s*){0,1}$",
                    "^\\s*([a-zA-Z]+\\s*){0,2}[Cc]alifornia\\s[Oo]ffice\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^California\\sAssociation\\sfor\\sCoordinated\\sTransportation$",
                    "^California's\\sWildlife",
                    "^California\\sAir\\sPollution\\sControl\\sOfficers",
                    "^California\\sClimate\\sChange\\sCenter", 
                    "^California\\s\\(State\\)\\sCoastal\\Conservancy",
                    "^Caltrans\\s[Dd]istrict\\s*([a-zA-Z0-9]+\\s*){0,3}$",
                    "^Caltrans$", 
                    "^Center[s]*\\sfor\\sDisease\\sControl\\sand\\sPrevention",
                    "^Center\\sfor\\sSustainable\\sEnergy$",
                    "^Center\\sfor\\sBiological\\sDiversity\\sand\\sCenter\\sfor\\sFood\\sSafety", 
                    "^\\s*([a-zA-Z]+\\s*){1,3}\\,\\s[Cc]ity\\s\\of$",
                    "^City\\sof\\s*([a-zA-Z]+\\s*){1,3}$",
                    "^City\\sof\\s*([a-zA-Z]+\\s*){1,3}s\\[Bb]ureau",
                    "^\\s*([a-zA-Z]+\\s*){0,5}[Cc]ommission\\s*([a-zA-Z]+\\s*){0,3}$",
                    "^\\s*([a-zA-Z]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,3}$",
                    "^[Cc]oastal\\s[Cc]onservancy\\s*([a-zA-Z]+\\s*){0,2}$", 
                    "^[Cc]onservation\\s[Ss]ervice\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^\\s*([a-zA-Z]+\\s*){1,3}Council\\s[Oo]f\\sGovernments",
                    "^\\s*([a-zA-Z]+\\s*){0,3}Federal\\sEmergency\\sManagement\\sAgency",
                    "^\\s*([a-zA-Z]+\\s*){0,2}Federal\\sHighway\\sAdministration\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^\\s*([a-zA-Z]+\\s*){0,2}Federal\\sTransit\\sAdministration\\s*([a-zA-Z]+\\s*){0,2}$",  
                    "^\\s*([a-zA-Z]+\\s*){0,5}[Ff]orest\\s[Ss]ervice\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^\\s*([a-zA-Z]+\\s*){0,2}Friends\\sof\\sthe\\sLos\\sAngeles\\sRiver\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^United\\sStates\\sFish\\sand\\sWildlife\\sService[s]*",
                    "^\\s*([a-zA-Z]+\\s*){0,3}Institute\\s*([a-zA-Z]+\\s*){0,3}$", 
                    "^LSC\\sTransportation\\sConsultants",
                    "^\\s*([a-zA-Z]+\\s*){0,4}[Mm]anagement\\s[Dd]istrict\\s*([a-zA-Z]+\\s*){0,4}$", 
                    "^\\s*([a-zA-Z]+\\s*){0,4}[Mm]etropolitan\\s[Aa]uthority\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^National\\sCenter\\sfor\\sChronic\\sDisease\\sPrevention",
                    "^National\\sRenewable\\sEnergy\\sLaboratory\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^Natural\\sResources\\sConservation\\sService\\s\\(NRCS",
                    "^National\\sAssociation\\sof\\sCity\\sTransportation\\sOfficials",
                    "^\\s*([a-zA-Z]+\\s*){0,4}[Oo]ffice\\s[Oo]f\\s[Pp]lanning\\s[Aa]nd\\s[Rr]esearch\\s*([a-zA-Z]+\\s*){0,4}$", 
                    "^\\s*([a-zA-Z]+\\s*){0,4}[Rr]ail[road]*\\s[Aa]uthority\\s*([a-zA-Z]+\\s*){0,4}$",
                    "^SFRWQCB\\s\\(San\\sFrancisco\\sBay\\sRegional",
                    "^\\s*([a-zA-Z]+\\s*){0,4}Flood\\sControl\\sDistrict$",
                    "^San\\sDiego\\sGas\\sand\\sElectric$",
                    "^Santa\\sMonica\\sMountains\\sConservancy",
                    "^Southern\\sCalifornia\\sEarthquake\\sData\\sCenter",
                    "^Southern\\sCalifornia\\sGas\\sCompany",
                    "The\\sNature\\sConservancy", 
                    "^Transit\\sCooperative\\sResearch\\sProgram", 
                    "^\\s*([a-zA-Z]+\\s*){0,4}[Tt]ransit\\s[Aa]uthority\\s*([a-zA-Z]+\\s*){0,4}$",
                    "^\\s*([a-zA-Z]+\\s*){0,4}[Tt]ransit\\s[Aa]dministration\\s*([a-zA-Z]+\\s*){0,4}$",
                    "^\\s*([a-zA-Z]+\\s*){0,4}[Tt]ransportation\\s[Aa]uthority\\s*([a-zA-Z]+\\s*){0,4}$",
                    "^\\s*([a-zA-Z]+\\s*){0,4}Transportaiton\\sAuthority\\s*([a-zA-Z]+\\s*){0,4}$", 
                    "^\\s*([a-zA-Z]+\\s*){0,4}[Tt]ransportation\\s[Cc]ommission\\s*([a-zA-Z]+\\s*){0,4}$",
                    "^TRPA\\s\\(Tahoe\\sRegional\\sPlanning\\Agency$",
                    "^United\\sStates\\sCensus\\sBureau\\s*([a-zA-Z]+\\s*){0,2}$", 
                    "^United\\sStates\\sGeological\\sSurvey\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^United\\sStates\\sGeological\\sService\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^United\\sStates\\sSoil\\sConservation\\sService\\s*([a-zA-Z]+\\s*){0,2}$",
                    "^USCB",
                    "^USDA\\s*([a-zA-Z]+\\s*){0,4}$",
                    "USEPA",
                    "^\\s*([a-zA-Z]+\\s*){0,3}University\\s*([a-zA-Z]+\\s*){0,3}$", 
                    "U[.]*S[.]*\\sWater\\sResources\\sCouncil",
                    "UCLA\\sCenter\\sFor\\sClimate\\sScience", 
                    "^\\s*([a-zA-Z]+\\s*){0,4}Water\\sDistrict\\s*([a-zA-Z]+\\s*){0,4}$")
agency.authors <- paste(agency.authors, collapse = "|")

## Looping to say: if any of those patterns are in certain elements of the list, identify it.
patterns.rm <- "\\(|\\,|\\)|\\."

agency.in.title <- data.table()
for(i in seq_along(mult.titles.only$title)){
  titles <- mult.titles.only$title[i]
  line.one <- titles[[1]][1]
  line.one.clean <- str_remove_all(line.one, patterns.rm)
  line.two <- titles[[1]][2]
  line.two.clean <- str_remove_all(line.two, patterns.rm)
  agency.title.lineone <- str_detect(line.one.clean, agency.authors)
  agency.title.linetwo <- str_detect(line.two.clean, agency.authors)
  new.df <- data.table(agency.title.lineone, agency.title.linetwo)
  agency.in.title <- rbind(agency.in.title, new.df)
}

# Merge this with the rest of the data to make sure we can continue to match
title.lists.agency.tags <- cbind(mult.titles.only$row, agency.in.title)
colnames(title.lists.agency.tags)[1]<- "row"
df <- left_join(df, title.lists.agency.tags, by = "row") 
df$agency.title.lineone <- ifelse(is.na(df$agency.title.lineone), FALSE, df$agency.title.lineone)
df$agency.title.linetwo <- ifelse(is.na(df$agency.title.linetwo), FALSE, df$agency.title.linetwo)


## 4. Identify "prepared by" titles ----
prep.by.title <- data.table()
for(i in seq_along(mult.titles.only$title)){
  titles <- mult.titles.only$title[i]
  line.two <- titles[[1]][2]
  prep.by <- str_detect(line.two, "[Pp]repared\\s[Bb]y")
  prep.by.title <- rbind(prep.by.title, prep.by)
}

# Merge this with the rest of the data to make sure we can continue to match
prep.by.title.tags <- cbind(mult.titles.only$row, prep.by.title)
colnames(prep.by.title.tags)<- c("row", "prep.by.title")
df <- left_join(df, prep.by.title.tags, by = "row") 
df$prep.by.title <- ifelse(is.na(df$prep.by.title), FALSE, df$prep.by.title)

### Inspect the work I have done so far ----
check <- df %>% filter(agency.title.lineone == F)
check <- df %>% filter(agency.title.lineone == T)
check <- df %>% filter(prep.by.title== T)

holding.df <- df
## 5. Unnesting rows with the same length in major categories ----
df <- holding.df

# Set these to NA
df$doi <- ifelse(df$doi == "NULL", NA, df$doi)
df$url <- ifelse(df$url == "NULL", NA, df$url)

# How many of these citations have multiple containers?
mult.cont.df <- data.table()
for(i in seq_along(df$container)){
  mult.cont <- length(df$container[[i]]) > 1
  row <- i
  mult.cont <- data.table(mult.cont, row)
  mult.cont.df <- rbind(mult.cont.df, mult.cont, fill = T)
}
table(mult.cont.df$mult.cont)

df <- full_join(df, mult.cont.df, by = "row")

# If you have multiple containers, do you have equal number titles, containers and years?
match.df <- data.table()
for(i in seq_along(df$title)){
  match <- ifelse(df$mult.cont[[i]] == T & length(df$title[[i]]) == length(df$date[[i]]) & length(df$title[[i]])  == length(df$container[[i]]), T, F)
  row <- i
  match2 <- data.table(match, row)
  match.df <- rbind(match.df, match2, fill = T)
}
df <- full_join(df, match.df, by = "row")

# What about, multiple dois and titles?
matchdoi.df <- data.table()
for(i in seq_along(df$title)){
  matchdoi <- ifelse(length(df$title[[i]]) > 1 & length(df$title[[i]]) == length(df$doi[[i]]), T, F)
  row <- i
  matchdoi <- data.table(matchdoi, row)
  matchdoi.df <- rbind(matchdoi.df, matchdoi, fill = T)
}
df <- full_join(df, matchdoi.df, by = "row")
table(df$matchdoi, df$match)

# I tried to do this in one long condition before, but it was too much. So, let's create a binary to identify where the length of the doi and url lists match the other matches
nest.df <- data.table()
for(i in seq_along(df$title)){
  nesturl <- ifelse(df$match[[i]] == T & length(df$title[[i]]) == length(df$url[[i]]), T, F)
  nestdoi <- ifelse(df$match[[i]] == T & length(df$title[[i]]) == length(df$doi[[i]]), T, F)
  row <- i
  nests <- cbind(nesturl, nestdoi, row)
  nest.df <- rbind(nests, nest.df)
}

df <- full_join(df, nest.df, by = "row")

# Started at 36450, then 36664, so added just 210
# So now, I will unnest if these things all have matching lists
mot.df <- data.table()
for(i in seq_along(df$title)){
  if(df$match[[i]] == T && 
     df$nesturl[[i]] == 1 && 
     df$nestdoi[[i]] == 0){
    mot <- unnest(df[i,], cols = c(title, date, container, url)) 
  } else if(df$match[[i]] == T && 
            df$nesturl[[i]] == 0 && 
            df$nestdoi[[i]] == 1){
    mot <- unnest(df[i,], cols = c(title, date, container, doi))
  } else if(df$match[[i]] == T && 
            df$nesturl[[i]] == 1 && 
            df$nestdoi[[i]] == 1){
    mot <- unnest(df[i,], cols = c(title, date, container, doi, url)) 
  } else if(df$match[[i]] == T && 
            df$nesturl[[i]] == 0 && 
            df$nestdoi[[i]] == 0){
    mot <- unnest(df[i,], cols = c(title, date, container)) 
  } else if(df$matchdoi[[i]] == T &&
            df$match[[i]] == F){
    mot <- unnest(df[i,], cols = c(title, doi))
  } else if(df$match[[i]] == F){
    mot <- df[i,]
  }
  mot.df <- rbind(mot, mot.df)
}


## 6. Assign new titles based on conditions (end with clean.title.df) ----
df <- mot.df
df$row <- 1:nrow(df)

clean.title.df <- data.table()
for(i in seq_along(df$title)){
  titles <- df$title[i]
  line.one <- titles[[1]][1]
  line.two <- titles[[1]][2]
  line.three <- titles[[1]][3]
  line.four <- titles[[1]][4]
  line.five <- titles[[1]][5]
  row <- i
  # If 'prepared by' is in line 2, extract after prepared by and assign to consultant
  if(df$prep.by.title[i] == T){
    consultant <- str_extract(line.two, ".*(?=[Pp]repared\\s[Bb]y)")
    title.new <- line.one
    author.new <- consultant
  }
  # If agency title in first line is true and no author, reassign author and title is line 2
  else if(df$agency.title.lineone[i] == T && 
          df$two.titles[i] == T) {
    author.new <- line.one
    title.new <- line.two
  } 
  
  # If agency title in second line is true and no author, reassign author and title is line 2
  else if(df$agency.title.linetwo[i] == T && 
          df$two.titles[i] == T){
    author.new <- line.two
    title.new <- line.one
  } 
  
  # If agency in title line 1, but has 3 strings, merge non-agency title string
  else if(df$agency.title.lineone[i] == T && 
          df$three.titles[i] == T){
    author.new <- line.one
    title.new <- paste(line.two, line.three) 
  } 
  
  # If agency in title line 2, but has more than 3 strings, merge non-agency title string
  else if(df$agency.title.linetwo[i] == T && 
          df$three.titles[i] == T){
    author.new <- line.two
    title.new <- paste(line.one, line.three) 
  } 
  # If none of those things, but has 2 strings, combine strings 1 and 2
  else if(df$two.titles[i] == T && 
          df$agency.title.lineone[i] == F &&
          df$agency.title.linetwo[i] == F){
    title.new <- paste(line.one, line.two) 
  } 
  # If more than 2 strings, just combine the first 2. This is imperfect and not dealing with the agency element, but it is the end of my bandwidth right now
  else if(df$three.titles[i] == T || df$four.plus[i] == T){
    title.new <- paste(line.one, line.two) 
  } else{
    title.new <- NA
    author.new <- NA
    consultant <- NA
  }
  new.df <- data.table(row, title.new, author.new)
  clean.title.df <- rbind(clean.title.df, new.df, fill = T)
}
# Remove the open parentheses from titles
clean.title.df$author.new <- str_remove_all(clean.title.df$author.new, "\\(|\\)")
clean.title.df$title.new <- str_remove_all(clean.title.df$title.new, "\\(|\\)")

clean.title.df <- full_join(clean.title.df, df,  by = "row")

# AUTHOR CLEANING ----
## Re-assigning for ease
df <- clean.title.df

## 1. Identify agency author ----

agency.family <- c("Administration", "Agency", "Association", "Assessment", "Associates", "Authority",  "Board", "Bureau", "Center", "Collaborative", "Conservancy", "^Consult[a-z]+$",  "Commission", "Council", "County", "Department", "District", "Foundation", "Fisheries", "Government[s]*", "Group", "Laboratory", "Management", "Meeting", "Plan", "Planner", "Recreation", "Report", "Service", "Society", "Survey", "Study", "Tourism", "Transit", "Transportation", "Univeristy", "Works")
#agency.family <- paste(agency.family, collapse = "|")
agency.given <- c("U[.]*S[.]*", "^Federal\\sHighway\\sAdministration$")
#agency.given <- paste(agency.given, collapse = "|")
particle.pattern <- c("of", "Of", "For", "for")

## 2. Assign author on conditions (end with clean.author.df) ----
# If there is a 3rd columns and it is called particle, place in this order
# If 3rd column is "other", then get rid of it because I have not seen any worthwhile ones
# Typical author family first
# Typical author given first
#  Suffix
# If there are 2 columns with government family name, order accordingly
author.string <- data.table()
for(i in seq_along(df$author)){  
  author <- data.frame(df$author[i])
  if (ncol(author) == 2 && colnames(author)[2] == "given" && 
      FALSE %in% (author$family %in% agency.family)){
    author.clean <- paste(author$family, author$given, sep = ", ")
  } else if (ncol(author) == 2 && colnames(author)[2] == "family" && 
             FALSE %in% (author$family %in% agency.family)){
    author.clean <- paste(author$family, author$given, sep = ", ")
  } else if (ncol(author) == 2 && colnames(author)[2] == "suffix" && 
             FALSE %in% (author$family %in% agency.family)){
    author.clean <- paste(author$family, author$suffix, sep = ", ")
  } else if(ncol(author) == 2 && 
            TRUE %in% (author$family %in% agency.family)){
    #str_detect(author$family, agency.family) == T){
    author.clean <- paste(author$given, author$family)
  } else if(ncol(author) == 2 && 
            TRUE %in% (author$given %in% agency.given)){
    #str_detect(author$given, agency.given) == T){
    author.clean <- paste(author$given, author$family)
  } else if(ncol(author) == 3 && colnames(author)[3] == "particle" && 
            TRUE %in% (author[,3] %in% particle.pattern)){
    author.clean <- paste(author$given, author$particle, author$family)
  } else if(ncol(author) == 3 && colnames(author)[3] == "others"){
    author.clean <- paste(author$family, author$given, sep = ", ")
  } else if (ncol(author) == 1){
    author.clean <- author[,1]
  } else {
    author.clean <- NA_character_
  }
  
  row <- i
  authors <- cbind(author.clean, row)
  author.string <- rbind(authors, author.string, fill = T)
}

# Make df wider by nesting authors into one cell, divided by ;
author.wide <- dcast(author.string, row ~ "author.clean", value.var = "author.clean", fun.aggregate=function(x) paste(x, collapse = ";"))
author.wide$row <- as.numeric(author.wide$row)

clean.author.df <- full_join(author.wide, df, by = "row")

## Inspect the work I have done so far 
check <- clean.author.df %>% filter(!is.na(author.clean))

# CONTAINER TITLE ----
df <- clean.author.df
df$row <- 1:nrow(df)

# This is no longer relevant because it repeated the TRUE for those that we previously unnested
df <- df %>% select(-mult.cont)

# How many of these citations have multiple containers now?
mult.cont.df <- data.table()
for(i in seq_along(df$container)){
  mult.cont <- length(df$container[[i]]) > 1
  row <- i
  mult.cont.dt <- data.table(mult.cont, row)
  mult.cont.df <- rbind(mult.cont.df, mult.cont.dt, fill = T)
}
df <- full_join(df, mult.cont.df, by= "row")

# What is going on with there multiple containers?
mult.cont <- df %>% filter(mult.cont == T)

# What to do with the remaining
clean.cont.df <- data.table()
for(i in seq_along(df$container)){
  cont <- df$container[i]
  line.one <- cont[[1]][1]
  row <- i
  # If more than 1 container, merge (I have already unnested for those that have equal title date and journal lengths)
  if(length(cont[[1]]) > 1){
    cont.new <- line.one
  }
  # Everything else should just take original pub value
  else{
    cont.new <- cont
  } 
  new.df <- data.table(row, cont.new)
  clean.cont.df <- rbind(clean.cont.df, new.df, fill = T)
}

clean.cont.df <- full_join(clean.cont.df, df, by = "row")

# DATES ----
df <- clean.cont.df

## 1. Separating out multiple dates
df <- separate(df, date, into = c("date1", "date2", "date3", "date4", "date5", "date6", "date7", "date8", "date9", "date10", "date11"), sep = '\\"\\,\\s\\"')

df$date1 <- str_remove(df$date1, 'c\\(\\"')
df$date2 <- str_remove(df$date2, '\\"\\)')
df$date3 <- str_remove(df$date3, '\\"\\)')
df$date4 <- str_remove(df$date4, '\\"\\)')
df$date5 <- str_remove(df$date5, '\\"\\)')
df$date6 <- str_remove(df$date6, '\\"\\)')
df$date7 <- str_remove(df$date7, '\\"\\)')
df$date8 <- str_remove(df$date8, '\\"\\)')
df$date9 <- str_remove(df$date9, '\\"\\)')
df$date10 <- str_remove(df$date10, '\\"\\)')
df$date11 <- str_remove(df$date11, '\\"\\)')

## None of this works, but keeping it here for notes ----
# Some dates are years, they are all in diff formats -> get years
# date.new <- df %>% filter(!is.na(date)) %>% unnest(date, keep_empty = T)
# These don't work
# date.newer <- dcast(date.new, date ~ row, value.var = "date")
# date.newer <- pivot_wider(date.new, names_from = row, values_from = date)

## 1. Isolate year only for all dates ----

## Have to pull out dates if they are embedded in some other weird string of numbers
date.formats <- c("\\d{1,2}-\\d{1,2}-\\d{4}", "\\d{1,2}-\\d{1,2}-\\s?\\d{2}", "\\d{4}-\\d{1,2}-\\d{1,2}", "\\d{4}-\\d{1,2}", "\\d{1,2}\\/\\d{1,2}\\/\\d{4}", "\\d{1,2}\\/\\d{1,2}\\/\\s?\\d{2}", "\\d{1,2}\\/\\s?\\d{4}", "\\d{4}\\/\\d{1,2}\\/\\d{1,2}", "(?<=\\s)\\d{4}$", "^\\d{4}$")
date.formats <- paste(date.formats, collapse = "|")

# clean up each column. I know I could do apply functions, but I don't understand them
df$date1.clean <- df$date1 %>% str_extract(date.formats) %>% trimws()
df$date2.clean <- df$date2 %>% str_extract(date.formats) %>% trimws()
df$date3.clean <- df$date3 %>% str_extract(date.formats) %>% trimws()
df$date4.clean <- df$date4 %>% str_extract(date.formats) %>% trimws()
df$date5.clean <- df$date5 %>% str_extract(date.formats) %>% trimws()
df$date6.clean <- df$date6 %>% str_extract(date.formats) %>% trimws()
df$date7.clean <- df$date7 %>% str_extract(date.formats) %>% trimws()
df$date8.clean <- df$date8 %>% str_extract(date.formats) %>% trimws()
df$date9.clean <- df$date9 %>% str_extract(date.formats) %>% trimws()
df$date10.clean <- df$date10 %>% str_extract(date.formats) %>% trimws()
df$date11.clean <- df$date11 %>% str_extract(date.formats) %>% trimws()

# Call upon date formatting to pull out year in each format

assignyear <- function(x) {
  ifelse(str_detect(x,"^\\d{1,2}-\\d{1,2}-\\d{4}$"), 
         year(as.Date(x, format = "%m-%d-%Y")),
         ifelse(str_detect(x,"^\\d{4}-\\d{1,2}-\\d{1,2}$"),
                year(as.Date(x, format = "%Y-%m-%d")),
                ifelse(str_detect(x,"^\\d{1,2}-\\d{1,2}-\\s?\\d{2}$"),
                       year(as.Date(x, format = "%m-%d-%Y")),
                       ifelse(str_detect(x,"^\\d{4}-\\d{1,2}$"),
                              # 2 elements do not make a date, need to paste to make a day
                              year(as.Date(paste(x,1,sep="-"), format = "%Y-%m-%d")),
                              ifelse(str_detect(x,"^\\d{1,2}\\/\\d{1,2}\\/\\d{4}$"), 
                                     year(as.Date(x, format = "%m/%d/%Y")),
                                     ifelse(str_detect(x,"^\\d{4}\\/\\d{1,2}\\/\\d{1,2}$"),
                                            year(as.Date(x, format = "%Y/%m/%d")),
                                            ifelse(str_detect(x,"^\\d{1,2}\\/\\d{1,2}\\/\\s?\\d{2}$"),
                                                   year(as.Date(x, format = "%m/%d/%Y")),
                                                   ifelse(str_detect(x,"^\\d{1,2}\\/\\d{4}$"),
                                                          year(as.Date(paste(x,1,sep="/"), format = "%m/%Y/%d")),
                                                          ifelse(str_detect(x,"^\\d{4}$"),
                                                                 year(as.Date(x, format = "%Y")), "no pattern")))))))))
}

df$date1.year <- as.numeric(assignyear(df$date1.clean))
df$date2.year <- as.numeric(assignyear(df$date2.clean))
df$date3.year <- as.numeric(assignyear(df$date3.clean))
df$date4.year <- as.numeric(assignyear(df$date4.clean))
df$date5.year <- as.numeric(assignyear(df$date5.clean))
df$date6.year <- as.numeric(assignyear(df$date6.clean))
df$date7.year <- as.numeric(assignyear(df$date7.clean))
df$date8.year <- as.numeric(assignyear(df$date8.clean))
df$date9.year <- as.numeric(assignyear(df$date9.clean))
df$date10.year <- as.numeric(assignyear(df$date10.clean))
df$date11.year <- as.numeric(assignyear(df$date11.clean))

## 2. Conditional statements for dates ----

clean.year.df <- data.table()
for(i in 1:nrow(df)){
  number.of.years <- sum(!is.na(df[i,41:50]) == T)
  if(number.of.years == 1){
    year <- df$date1.year[i]
  }
  # If multiple dates don't match across columns, take the first
  else if(number.of.years > 1){
    year <- df$date1.year[i]
    # EXCLUDING: If multiple dates don't match across columns, take the most recent
    # else if(number.of.years > 1){
    #  year <- ifelse(df$date1.year[i] == df$date2.year[i], df$date1.year[i], 
    #             ifelse(df$date1.year[i] != df$date2.year[i], max(df[i,41:50], na.rm = T),
    #             "no-pattern"))
  } 
  else if(number.of.years == 0){
    year <- NA_character_
  }
  else{
    year <- "missed-something"
  }
  #year <- ifelse(year < 1850 | year > 2020 & is.null(df$title[i]), "omitrow",
  #        ifelse(year < 1800 | year > 2020 & !is.null(df$title[i]), NA_character_, year))
  row <- i
  new.df <- data.table(row, year)
  clean.year.df <- rbind(clean.year.df, new.df, fill = T)
}

clean.year.df <- full_join(clean.year.df, df,  by = "row")

## 3. Get rid of totally non-sensible years ----
clean.year.df$year <- ifelse(clean.year.df$year < 1800 | clean.year.df$year > 2020, NA_character_, 
                             clean.year.df$year)


# PUBLISHER ----
## Really it is just that for very long titles, there might also be more than one publisher, but the publishers here feel very disparate. So for multiple titles, we are just keeping the first 2, and then for container, publisher, URL, doi, we will keep the first one. 
df <- clean.year.df
clean.pub.df <- data.table()
for(i in seq_along(df$publisher)){
  pubs <- df$publisher[i]
  line.one <- pubs[[1]][1]
  row <- i
  # If more than 1 pub, just take the first
  if(length(pubs[[1]]) > 1){
    pub.new <- line.one
  }
  # Everything else should just take original pub value
  else{
    pub.new <- pubs
  } 
  new.df <- data.table(row, pub.new)
  clean.pub.df <- rbind(clean.pub.df, new.df, fill = T)
}

## Get rid of publications that are too long and probably not really publications
clean.pub.df$n.words = lengths(gregexpr("\\W+", clean.pub.df$pub.new))+1
clean.pub.df$pub.new <- ifelse(clean.pub.df$n.words > 16, NA_character_, clean.pub.df$pub.new)
# Get rid of NAs
clean.pub.df$pub.new <- ifelse(clean.pub.df$pub.new == "NULL", NA_character_, clean.pub.df$pub.new)


clean.pub.df <- full_join(clean.pub.df, df,  by = "row")

# URL ----
## In line with publishing
df <- clean.pub.df
clean.url.df <- data.table()
for(i in seq_along(df$url)){
  urls <- df$url[i]
  line.one <- urls[[1]][1]
  row <- i
  # If more than 1 pub, just take the first
  if(length(urls[[1]]) > 1){
    url.new <- line.one
  }
  # Everything else should just take original pub value
  else{
    url.new <- urls
  } 
  row <- i
  new.df <- data.table(row, url.new)
  clean.url.df <- rbind(clean.url.df, new.df, fill = T)
}

# If the url just ends, delete
clean.url.df$url.new <- as.character(clean.url.df$url.new)
false.starts <- "http[s]?\\:\\/\\/?$|http[s]?\\:\\/\\/www\\.?$|^list|^at\\:$|^here\\:$|^heading\\:$|^c\\:|^character|^file|^ftp\\:\\/\\/ftp\\.$"
clean.url.df$url.new <- ifelse(str_detect(clean.url.df$url.new, false.starts), NA_character_, 
                               ifelse(str_detect(clean.url.df$url.new, "^link\\:|^Available\\:"), str_remove(clean.url.df$url.new, "^link\\:|^Available\\:"),
                                      clean.url.df$url.new))

clean.url.df <- full_join(clean.url.df, df,  by = "row")

# DOI ----
df <- clean.url.df

# How many of these citations have multiple dois now? -- only 27, so I will take the easy road and just take the first. We lose 27, but that is okay
mult.doi.df <- data.table()
for(i in seq_along(df$doi)){
  mult.doi <- length(df$doi[[i]]) > 1
  row <- i
  mult.doi <- data.table(mult.doi, row)
  mult.doi.df <- rbind(mult.doi.df, mult.doi, fill = T)
}
table(mult.doi.df$mult.doi)

## Choose just the first one

clean.doi.df <- data.table()
for(i in seq_along(df$doi)){
  doi <- df$doi[i]
  line.one <- doi[[1]][1]
  row <- i
  # If more than 1 doi, just take the first
  if(length(doi[[1]]) > 1){
    doi.new <- line.one
  }
  # Everything else should just take original doi value
  else{
    doi.new <- doi
  } 
  new.df <- data.table(row, doi.new)
  clean.doi.df <- rbind(clean.doi.df, new.df, fill = T)
}

clean.doi.df <- full_join(clean.doi.df, df,  by = "row")
clean.doi.df$doi <- as.character(clean.doi.df$doi)
clean.doi.df$doi <- ifelse(clean.doi.df$doi == "NA", NA, clean.doi.df$doi)
clean.doi.df$doi.new <- as.character(clean.doi.df$doi.new)
clean.doi.df$doi.new <- ifelse(clean.doi.df$doi.new == "NA", NA, clean.doi.df$doi.new)

# CLEAN UP INDEX ----
new.df <- clean.doi.df
new.df <- new.df %>% select(author.clean, author.new, title.new, title, year, pub.new, cont.new, doi.new, url.new, File)
new.df$author.clean <- ifelse(new.df$author.clean == "NA", NA_character_, new.df$author.clean)

# Why do NAs keep becoming characters!?
new.df$author.clean <- ifelse(new.df$author.clean == "NA", NA, new.df$author.clean)
new.df$url.new <- ifelse(new.df$url.new == "NA", NA, new.df$url.new)
new.df$cont.new <- as.character(new.df$cont.new)
new.df$cont.new <- ifelse(new.df$cont.new == "NULL", NA, new.df$cont.new)

## 1. Author sorting ----

# It mostly looks like author.new is the clear winner, except in some cases
new.df$author.clean <- ifelse(new.df$author.clean == "", NA_character_, new.df$author.clean)
new.df$author.new <- ifelse(new.df$author.new == "", NA_character_, new.df$author.new)
# If author.new is Department or Administration and author.clean is Office, combine
new.df$author <- ifelse(str_detect(new.df$author.new, "^Office") & 
                          str_detect(new.df$author.clean, "Department|Administration"), 
                        paste(new.df$author.clean, new.df$author.new),
                        # Specific to this example, but if author.clean is Authority and author.new is Plan, go with Auth     
                        ifelse(str_detect(new.df$author.new, "Plan") & 
                                 str_detect(new.df$author.clean, "Authority"),
                               new.df$author.clean,
                               # If author.clean has multiple actual author names, make it that, and below make new publisher
                               ifelse(str_detect(new.df$author.clean, "^[:alpha:]{2,}\\,\\s[:alpha:]+\\.?[:alpha:]*.?\\;?"),
                                      new.df$author.clean,
                                      # Make punctuation only NA
                                      ifelse(str_detect(new.df$author.clean, "^-+$|^[:punct:]+|^—+||^\\<http:"), NA_character_,
                                             # If there is author.clean but no author.new, make clean
                                             ifelse(is.na(new.df$author.new), new.df$author.clean,
                                                    ifelse(is.na(new.df$author.clean), new.df$author.new,
                                                           # If there is author.clean and new, make new # For whatever is left
                                                           new.df$author.new))))))
# There is little overlap and the ones I created look better, so just overwriting
new.df$pub.new<- ifelse(str_detect(new.df$author.clean, "^[:alpha:]{2,}\\,\\s[:alpha:]+\\.?[:alpha:]*.?\\;?")
                        & !is.na(new.df$author.new),
                        new.df$author.new, new.df$pub.new)
# No overlap, overwriting
new.df$url.new <- ifelse(str_detect(new.df$author.clean, "^\\<http"),
                         new.df$author.clean, new.df$url.new)

## 2. Title sorting ----

# If there is a title new, replace title, 
new.df$title <- ifelse(is.na(new.df$title.new), new.df$title, new.df$title.new)

# Refine ----
refined.df <- new.df %>% select(author, year, title, cont.new, pub.new, doi.new, url.new, File)
colnames(refined.df) <- c("Author", "Year", "Title", "Journal", "Publisher", "DOI", "URL", "File")

class(refined.df$Year)
class(refined.df$Title)
class(refined.df$Journal)
class(refined.df$DOI)
class(refined.df$Author)
class(refined.df$Publisher)

refined.df$Year <- as.numeric(refined.df$Year)
refined.df$Title <- as.character(refined.df$Title)
refined.df$DOI <- as.character(refined.df$DOI)
refined.df$Publisher <- as.character(refined.df$Publisher)

write.csv(refined.df, "~/Box/truckee/data/compiled_anystyle_results/state_refined.csv", row.names = F)

## If there is error in writing the csv, run through to find where I have not unlisted
list.title <- data.table()
for(i in 1:nrow(refined.df)){
  list <- length(refined.df$Title[[i]]) > 1
  list.title <- rbind(list, list.title)}
table(list.title)

list.url <- data.table()
for(i in 1:nrow(refined.df)){
  list <- length(refined.df$URL[[i]]) > 1
  list.url <- rbind(list, list.url)}
table(list.url)

list.pub <- data.table()
for(i in 1:nrow(refined.df)){
  list <- length(refined.df$Publisher[[i]]) > 1
  list.pub <- rbind(list, list.pub)}
table(list.pub)

list.author <- data.table()
for(i in 1:nrow(refined.df)){
  list <- length(refined.df$Author[[i]]) > 1
  list.author <- rbind(list, list.author)}
table(list.author)

list.year <- data.table()
for(i in 1:nrow(refined.df)){
  list <- length(refined.df$Year[[i]]) > 1
  list.year <- rbind(list, list.year)}
table(list.year)
