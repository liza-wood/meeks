library(dplyr)
library(stringr)
library(tidyverse)

# I have run this through onces with county, mpo, and state, then after seeing whether or not the two anystyle citations were different with the "check" documents, I also have a check one. They look relatively similar, but I think I should use check.

county <- read.csv("~/Box/truckee/data/compiled_anystyle_results/county_refined.csv")
county_check <- read.csv("~/Box/truckee/data/compiled_anystyle_results/county_refined_check.csv")
county$level <- "County"
county_check$level <- "County"
mpo <- read.csv("~/Box/truckee/data/compiled_anystyle_results/mpo_refined.csv")
mpo_check <- read.csv("~/Box/truckee/data/compiled_anystyle_results/mpo_refined_check.csv")
mpo$level <- "MPO"
mpo_check$level <- "MPO"
state <- read.csv("~/Box/truckee/data/compiled_anystyle_results/state_refined.csv")
state_check <- read.csv("~/Box/truckee/data/compiled_anystyle_results/state_refined_check.csv")#fileEncoding="UTF-8", allowEscapes=T
state$level <- "State"
state_check$level <- "State"

df <- rbind(county_check, mpo_check, state_check) #85,325 citations or 87,376
df$agency <- ifelse(df$level == "mpo" | df$level == "county",  str_extract(df$File, "(?<=documents\\/).*(?=\\/)"), "caltrans")
df <- distinct(df) #73,232 citations or 75,059 with check

# GENERAL ----
# If NULL or blank, make NA

df$Title <- str_replace_all(df$Title, "NULL", NA_character_)
df$Title <- ifelse(df$Title == "", NA_character_, df$Title)
df$DOI <- str_replace_all(df$DOI, "NULL", NA_character_)
df$URL <- str_replace_all(df$URL, "NULL", NA_character_)
df$Publisher <- ifelse(df$Publisher == "", NA_character_, df$Publisher)

# TITLE CLEANING ----
# Remove leading whitespace
df$Title <- str_remove_all(df$Title, "^\\s+") 

# Certain titles begin with items, or have items, that are reason to remove entirely -- this is from reviewing the data
remove.row <- c("^\\¿", "°", "^\\@", "^\\§", "^\\#", "^\\%", "^\\$", "^✓", "^❖","^♦", "^\\♦", "^◦", "BRUSSEL\\sSPROUT", "\\d,\\d{3},\\d{3}", "\\d{1,2}:\\d{2}", "\\/\\sRules\\sand\\sRegulations", ">>> Abhijit Bagde", "^\\>\\sAccessed", "^\\d{3,}", "^\\=\\s\\$*\\d{3}", "ND\\s\\d\\.*\\d{1,6}\\sNA\\s", "B3J0\\d{3}", "^Proposed\\sAM", "^Proposed\\sPM", "^Surrogate\\:", "^Year\\s\\d$", "Name\\:\\sYear\\:\\sAddress\\:", "Ave\\s@\\s|Rd\\s@\\s|Dr\\s@\\s|Blvd\\s@\\s|Ln\\s@\\s", "^\\*{8}", "^\\*{5}", "\\d{2}\\-\\d{2}\\-\\d{2}", "\\d{1,2}\\syears\\sof", "^[[:punct:]]$", "^\\?\\s1$", "^\\*+\\sPer\\sthe", "^\\*\\w", "\\\\+", "^\\˃", "^\\++", "^\\<$", "^\\=", "^\\¢", "^_+\\s", "^_+[[:alpha:]]", "^_+$", "^\\*{1}", "^∙", "^0.", "^Contact\\sfor\\sMore", "^FlowName\\:", "^Experience\\:", "^\\d{1,2}[[:punct:]]\\d{1,2}", "^\\d{1,2}nd|^\\d{1,2}th|^\\d{1,2}rd|^\\d{1,2}st","^14\\sSan\\sDiego")
remove.row <- paste(remove.row, collapse="|")
df$remove <- str_detect(df$Title, remove.row) 
table(df$remove) # Remove 6110 rows or 6438 with check
df <- df %>% filter(remove == F | is.na(remove))

## If an address, remove eg. 1515WestKingmanStreet,ResidentialIntermodalFullYesSanBernardino,CA92411Yard	
st.names <- c("[Rr]oad|Rd\\.*|[St]reet|St\\.*|[Aa]venue|Ave\\.*|[Bb]oulevard|[Bb]lvd\\.*")
adress.pattern <- "^\\d{1,}\\s\\d*\\s[[A-Za-z]+\\s]{1,2}"

# For remaining titles, Remove all special characters
# Remove these from the string
remove <-  c("�", "\\","", "\\s$", "»", "^\\_+", "^\\_+\\.", "^\\___", "^\\-+", "^\\—+", "^\\--\\s", "\\'", "^\\&", "\\•", "\\|", "^\\.", "\"", "\\‘", "\\’", "\\“", "\\”", "\\„", "\\", "·", "\\·", "^\\)", "^\\/+", "^\\,", "^\\*{2}", "^\\<\\s")
remove <- paste(remove, collapse="|")
df$Title <- str_remove_all(df$Title, remove)

# Follows up based on challenges
df$Title <- str_remove_all(df$Title, "^\\s+") 
df$Title <- str_remove_all(df$Title, "--\\s|^\\.|^\\‐+|^\\–+|^\\―+")
df$Title <- str_remove_all(df$Title, "^\\s+") 
df$Title <- str_remove_all(df$Title, "^\\−\\s")

# Remove if title is just one word, or one set of numbers or punctuation
df$Title <- gsub("^[A-Za-z]+$|^\\d+$", NA_character_, df$Title)

df.hold <- df
# JOURNAL CLEANING ----
df <- df.hold
df$Journal <- str_remove_all(df$Journal, "^\\s+") 

df$Journal <- trimws(df$Journal)
remove.row <- c("^\\¿", 
                "^\\@", 
                "^\\§", 
                "^\\#", 
                "^\\%", 
                "^\\$", 
                "^✓", 
                "^❖",
                "^♦", 
                "^\\♦", 
                "^◦",
                "BRUSSEL\\sSPROUT", 
                "\\d,\\d{3},\\d{3}", 
                "\\d{1,2}:\\d{2}", 
                "\\/\\sRules\\sand\\sRegulations", 
                ">>> Abhijit Bagde", 
                "^\\>\\sAccessed", 
                "^\\d{3,}", 
                "^\\=\\s\\$*\\d{3}", 
                "ND\\s\\d\\.*\\d{1,6}\\sNA\\s", 
                "B3J0\\d{3}", 
                "^Proposed\\sAM", 
                "^Proposed\\sPM", 
                "^Surrogate", 
                "^Year\\s\\d$", 
                "Name\\:\\sYear\\:\\sAddress\\:", 
                "Ave\\s@\\s|Rd\\s@\\s|Dr\\s@\\s|Blvd\\s@\\s|Ln\\s@\\s", 
                "^\\*{8}",
                "^\\*{5}", 
                "\\d{2}\\-\\d{2}\\-\\d{2}", 
                "\\d{1,2}\\syears\\sof", 
                "^[[:punct:]]$", 
                "^\\?\\s1$", 
                "^\\*+\\sPer\\sthe", 
                "^\\*\\w", 
                "\\\\+", 
                "^\\˃", 
                "^\\++", 
                "^\\<$", 
                "^\\=", 
                "^\\¢", 
                "^_+\\s", 
                "^_+[[:alpha:]]", 
                "^_+$", 
                "ND\\s20\\sNA", 
                "^Photo", 
                "^Image", 
                "^\\:\\s", 
                "^\\&", 
                "^\\*", 
                "^\\`", 
                "^\\^", 
                "^ACTION", 
                "^Additionally", 
                "^Adoption\\sHearing", 
                "\\!", 
                "^Also", 
                "^Alternative$",  
                "^All\\sAges", 
                "^AM$", 
                "^Avenue$", 
                "^BE IT FURTHER RESOLVED", 
                "^BEN LOMOND", 
                "^Between$", 
                "^Bike Lane", 
                "^Bike Logo", 
                "^Box\\s?", 
                "PUBLIC COMMENT MEETING", 
                "^Quarterly$", 
                "^RareFind$", 
                "RATING", 
                "^Rd$", 
                "^Real$", 
                "Responses to comment", 
                "^Ring$", 
                "^[Ss]$", 
                "^[S]\\-", 
                "EL REFUGIO VINEYARD", 
                "^Assembly Bill$", 
                "^Associates?", 
                "B\\.A\\.\\sin\\sCommunications", 
                "^BEFORE THE", 
                "^Building\\s[Pp]ermits", 
                "^Attachment$", 
                "^CA\\s\\d{5}", 
                "^Condition I\\‐10",
                "^Contribution\\:", 
                "^COV$", 
                "^Dam Ped\\/Bike$", 
                "^Date of Government Version\\:", 
                "^Dual Bore", 
                "^E\\-mail", 
                "^EcoLocalizer Np$", 
                "^Element Type County",
                "^E$", 
                "^EW$", 
                "^Email [Cc]ommunication$", 
                "^Examples of County Special", 
                "^Executive Director", 
                "^Experience$", 
                "^Express$", 
                "^Expwy$", 
                "^Figueroa Street", 
                "^F E$", 
                "^FAMBRINI JOHN", 
                "^Fax$", 
                "^FAX", 
                "^FC  EASEMENT", 
                "^Fifteen Years of Experience", 
                "^Fifth Street San Bernardino CA$", 
                "^Fire Station$", 
                "^Fire Service Areas$", 
                "^First St$", 
                "^FlowName\\:$", 
                "^FOCUS", 
                "^Folson from$", 
                "^Formal is Everything Except", 
                "^Forms and Procedures  Form", 
                "^Fourth St 6th Floor San Bernardino CA 92401", 
                "^FR 53 Selma Four", 
                "^Franklin Elementary School", 
                "^FRE00106 Install Traffic",
                "^Free\\s?[Free]?$", 
                "^Fifteen Years",  
                "^FROM\\:", 
                "^Further Information", 
                "^GEARY CORRIDOR", 
                "^Gilroy Transit Center$", 
                "^Gingko Gi k$", 
                "^Given the adequacy", 
                "^GOCO ENTERPRISES", 
                "^Grading  S31", 
                "^Guidance$", 
                "^Gxuen State", 
                "^Homepage$", 
                "^Homestead Rd$", 
                "^However", 
                "^I M Idriss$", 
                "^I‐\\d\\d", 
                "^I\\‐\\d\\d", 
                "^I\\d\\d", 
                "^Interchange North Termini$", "^IMPACT 312", "^Improvements$", "^In addition", "^In compliance", "^In other words", "^Indian laurel fig", "^Informational Meeting", "^Intersection and Approach LOS$", "^Is 1994", "^Lemoore PE", "^Leningrad$", "^MEDER STREET FARMS",  "^Municipal Code §", "ARTICHOKE", "^Local Highway$", "^LOCAL HIGHWAY", "^LOS ANGELES", "^Los Angeles International Airport", "^M$", "^Maintenance$", "^Mandatory or Probability", "^Matrix of 2015", "^MAX Madera", "^Market St$", "^Maximum", "^Mgmt Program$", "^METRO Council$", "^Miller Rd OC$", "^Millions$", "^Mins Kg", "^Mitsubishi the 2007", "^Mgmt Program$", "^Mine Safety and Health Administration", "Mins Kg\\/Hr Bhp", "^Mitsubishi", "^MODEL \\#", "^Modeled using custom", "^MP STA Tr", "^Mr Rainka Qualifie", "^N New Z l d Zealand", "^New N Zealand Z l d$", "^N\\/A Dat", "^NA\\b", "^NANCYS TAILORING", "^National Pollutant Discharge Elimination System", "^National Endowment for the Arts$", "^NavigateLA$", "^Network [Aa]ll [Aa]ges", "^New Directions for Program Evaluation$", "^New Freedom", "Nitrosod", "^No\\b", "^Nominatim", "^None None", "^NORTH [Ss]ources\\:", "^NOTICE|^Notice", "^NOW THEREFORE", "^NO MOTOR VEHICLES", "^Nonconforming Use$", "^Obstruction$", "^Objectives$", "^Oct\\.?-\\d\\d", "^OF", "^OF", "^Ongoing$", "^Operations & Maintenance", "^Overview", "^Parsons Staff on$", "^[Pp]ersonal [Cc]ommunication", "^P$", "^PDF and MS Word format$", "^Peak", "^Ped\\. Path Phase$", "^[Pp]ersonal [Ii]nterview", "^PERFORMING ORGANIZATION", "^Phone|^PHONE", "^Por Teléfono$", "^[Pp][Mm]\\b", "^Posted$", "^Pla Pla Pla", "^Please", "^PM TRAVEL SPEED", "^Program 13220", "^Project", "^Proposed", "^PS24153420", "^Public [Mm]eeting", "^Purchase PM10", "^QIAshredder", "^Quarterly INDIAN", "^RANCHO LAS PALMAS", "^Rec$", "^Recieved$", "^Redlands", "^References", "^Regular Meeting", "^Release$", "^Remarks", "^Response$", "^Responses to Comment$", "^Research [Nn]etwork$", "^Research [Nn]otes", "^Research [Pp]aper", "^Research [Pp]roject", "^Research [Rr]eport$", "^Residential Research Record$", "^Response", "^Restoration Ecologist$", "^Retrieved$", "^Review$", "^Review Period$", "^Reviewed February$", "^Revenue %", "^Robomow It Mows You Dont$", "^Routes?$", "^Routes and Travel Times$",  "^RPA Ontario Phone$", "^Rule$", "^RW  Version", "^RY CO S", "^SAN FRANCISCO COUNTY", "^Saturday$", "^See Master Response", "^St$", "SR\\s\\d", "^SUBJECT\\:|^Subject\\:", "^Sunday$", "^SUNSET FARMS", "^Surface Year$", "^Stats$", "^S1131", "^SAFETY ROADSIDE", "^San Fernando Gold Line", "^ScanAvailableDot", "^SD805", "^See", "^Senate [Bb]ill$", "^Senior Planner$", "^Sept\\/Oct$", "^Shingle Rd Realignment", "^Shoreline Blvd", "^Sideline Zone$", "^Sidewalk and Bike Path$", "^Sidewalk Phase$", "^Signal [Ii]nstallations$", "^Signal Streets and", "^Slaff is", "^Smith [Ss]mith [Ll]ived", "^Soil \\d\\d", "^Sources", "^South$", "^South \\d", "^Southbound HOV Lane", "^Specific$", "^Street San Bernardino CA$", "^Study$", "^SunSentinel$", "^Table [Dd]4b", "^Take", "^Task", "^Taylor", "^TCM2 Kings Public Transit", "^Tel$", "^Telephone$", "^Ten Years of Experience", "^The$", "^The \\d\\d\\d\\d", "^The [Cc]ircular also", "^The [Cc]ity [Ww]as not", "^The [Cc]onstruction", "^The [Cc]ounty", "^The [Cc]orresponding", "^The [Rr]esources [aA]gency [pP]rimary #", "^Therefore", "^These", "^This", "^Thus", "^To\\s", "^Total \\% [Cc]hange", "^Times for Tuesday$", "^True Travel Times", "^UCPRCRR201605", "^Undated", "^US 101", "^V\\=", "^Vers$",  "^Wrigley Creek Improvement Project Year", "^WOA", "^Wkdy$", "^Westbound$", "^WHEREAS", "^W\\s", "^Wants to [Ii]ncrease [Mm]ultiuse$", "CASFS FARM", "^Written [Cc]oi", "^Xing$",  "^XXXX$", "^Year", "^YEAR", "^Yes [Cc]hapter$", "^Zone$", "^日", "^年", "^Mond?a?y?$","^Tuesd?a?y?$","^Wedn?e?s?d?a?y?$","^Thurs?d?a?y?$","^Frid?a?y?$","^Satu?r?d?a?y?$", "^Sund?a?y?$", "^Janu?a?r?y?$", "^Febr?u?a?r?y?$", "^Marc?h?$", "^Apri?l?$", "^May$", "^June?$", "^July?$", "^Augu?s?t?$", "^Sept?e?m?b?e?r?$", "^Octo?b?e?r?$", "Nove?m?b?e?r?$", "^Dece?m?b?e?r?$")

remove.row <- paste(remove.row, collapse="|")
df$remove <- str_detect(df$Journal, remove.row) 
table(df$remove) # Remove 1232 rows or 1230 in check
df <- df %>% filter(remove == F | is.na(remove))

# Remove these characters
remove <- c("�", "\\", "", "\\s$", "»", "^\\_+", "^\\_+\\.", "^\\___", "^\\-+", "^\\—+", "^\\--\\s", "\\'", "^\\&", "\\•", "\\|", "^\\.", "\"", "\\‘", "\\’", "\\“", "\\”", "\\„", "\\", "·", "\\·", "^\\)", "^\\/+", "^\\,", "^January$", "^February$", "^March$", "^April$", "^May$", "^June$", "^July$", "^August$", "^September$", "^October$", "^November$", "^December$", "^Accessed$", "^Access$", "^Accessed\\s[Oo]nline$", "Accessible\\sat$", "^Year$", "^Jan$", "^Feb$", "^Mar$", "^Apr$", "^Jun$", "^Oct$", "^Aug$", "^Sept$", "^Dec$", "^CD\\-?\\s?ROM\\s[a-z]?\\s?", "^Literature [Rr]eview\\s?o?n?", "^Miscellaneous Paper")
remove <- paste(remove, collapse="|")
df$Journal <- str_remove_all(df$Journal, remove)

# Remove the whole contents of this cell with NA
remove.cell <- c("^Adopted", "^ADOPTION", "^Amend", "^[a-z]", "^Annual\\sMeeting$", "^APN", "^Appendix", "^APPENDIX", "^App$", "^App\\.", "^Appendices", "^Approved", "^APPROV", "^Area\\sPlan[s]*$", "^Available", "Ave$", "AVENUE", "^Based", "^Bldg$", "^Blvd$", "^Calkins$", "^Journal$", "^Proc$", "^Project$", "^Project\\sManager$", "^Publication$", "^Rep$", "^Report$", "^Rept$", "^Resolution$", "^Revised$", "^Revision$", "^Assoc$", "^Bulletin$", "^Chapter$", "Chatsworth\\, CA", "^CA$", "^Contract$", "^Data$", "^Contribution$", "^Dated$", "^District$", "^Draft", "^DRAFT", "^Effective$", "^Exhibit$", "^Ibid$", "^N.d$", "^N.p$","^Final$", "^Figure", "^Final Report$", "^Final Approval$", "^Final Rule$", "^Finalized", "^Fourth Year Report$", "^FY", "^Governing$", "^High$", "^IIE Transactions$", "^In$", "^In preparation", "^In press", "In progress", "In submission", "^In this", "^Inc$", "^Include", "^ISBN", "^Issued$", "^ISO", "^It\\s", "^Journal$", "^J$", "^Nov$", "^Journal of the$","^Mond?a?y?$","^Tuesd?a?y?$","^Wedn?e?s?d?a?y?$","^Thurs?d?a?y?$","^Frid?a?y?$","^Satu?r?d?a?y?$", "^Sund?a?y?$", "^Janu?a?r?y?$", "^Febr?u?a?r?y?$", "^Marc?h?$", "^Apri?l?$", "^May$", "^June?$", "^July?$", "^Augu?s?t?$", "^Sept?e?m?b?e?r?$", "^Octo?b?e?r?$", "Nove?m?b?e?r?$", "^Dece?m?b?e?r?$", "^Last Accessed$", "^Last Accessed\\: May$", "^Last [Aa]mended$", "^Last [Mm]odified$", "^Last [Mm]odified December$", "^Last [Mm]odified Mach$", "^Last [Mm]odified\\:? October$", "^Last [Rr]eviewed$", "^Last [Rr]evised$", "^Last [Uu]pdated$", "Last [Uu]pdated February 21$", "^Leningrad$", "^Lett", "^LightRailNow$", "^Location", "^M\\+ PMVM", "^Map", "^Memo", "^Meeting", "^Middle Archaic", "^Minutes", "^Mitigation [Mm]easure", "^MS", "^MY 2011","^NA", "^Na~arian", "^Name",  "^Natural Environment Study$", "^News", "^New York N\\.p$", "^Next$", "^Note$", "^N\\.p", "^[Nn]\\.[Dd]\\.", "^Number", "^Nov$", "^On$", "^Other", "^P0503", "^Page","^PAGE", "^Paper$", "^Parking$", "^Parts?$", "^PDF and MS Word Format$", "^Passenger$", "^Patent$", "^Presentation$", "^Press Corrected Proof$", "^Proceedings$", "^Produced$", "^Project$", "^Pub$", "^Publication$","^Quantification-Report$", "^Recieved", "^Received", "Rep$", "^Reports?$", "^Reporting Form$", "^Rept$", "^Research$", "^Resolution$", "^Review$", "^Revised", "^Revision$", "^Roads$",  "^Scale$", "^Section$", "^Sheets?$", "^Signed on", "^Site [Nn]ame", "^Site [Vv]isited", "^Site [Nn]umber", "^Suite$", "^Stakeholder Interview", "^Summary for Policymakers$", "^Summary of [Ff]ieldwork", "^Summer$", "^Table", "^Two$", "^TX$", "^Steven$", "^Technical Memorandum$", "^Technical [Nn]ote$", "^Technical [Rr]eport$", "^UCDITS", "^UCPRCR", "^Updated?$", "^Unpublished$", "^Update", "^URL", "^V$", "^Viewed", "^VOLUME$", "^Version$", "^Website$", "^Working Paper$", "^PCI", "^PCF", "^Your", "^location", "^WestConshohocken", "West Conshohocken")
remove.cell <- paste(remove.cell, collapse="|")
df$Journal <- ifelse(str_detect(df$Journal, remove.cell), NA_character_, df$Journal)

df$Journal <- ifelse(df$Journal == "", NA_character_, df$Journal)


# Follows up based on challenges
df$Journal <- str_remove_all(df$Journal, "^\\s+") 
df$Journal <- str_remove_all(df$Journal, "^--|^\\.|^\\‐+|^\\–+")
df$Journal <- str_remove_all(df$Journal, "^―|^-|^\\.---~")
df$Journal <- str_remove_all(df$Journal, "^\\s+") 

###*** IF THERE IS A PREPARED BY SECTION HERE, BUT NOT AN AUTHOR, MOVE IT


df.hold <- df

# AUTHOR CLEANING ----
df <- df.hold
# Remove leading whitespace
df$Author <- str_remove_all(df$Author, "^\\s+") 

# Inspected specifically A-C O-R rows at random
remove.row <- c( "^\\@","Ave\\s@\\s|Rd\\s@\\s|Dr\\s@\\s|Blvd\\s@\\s|Ln\\s@\\s", "RODONI", "^Accessibility$", "^accordingly$", "^Access\\sRoute", "^Acenaphth", "^Acknowledgment", "addition", "^After$", "^Again$", "^Aged$", "^Agency$", "^Aimsun$", "^Airnavcom$", "Alert\\,\\sBe\\sWork\\sZone", "^Also", "^Ampe$", "And", "^Anicic", "^answers\\,\\sYahoo", "^Area$", "^Asph$", "^Ave\\.", "^Ave\\,", "^Avenue\\,", "Awards\\,\\sFederal", "^Aye$", "^Background$", "^Bar\\,\\sDiamond", "^Base\\sLine\\s@", "^Begin\\sTrip\\s\\-\\sVeh\\.$", "^Ben\\,\\sPage", "^between!", "^beyond\\,", "^Bibliography$", "^bld$", "^Blvd\\,", "^Break$", "^Bridge\\,\\sS", "^Bridge\\,\\sW", "^Buijze$", "^BURBANK", "^buses$", "^Butylbenzylphthalate", "^noted,\\sComment", "^Checklist", "^CHECKLIST", "^Cities\\,\\sBig$", "^Citilog$", "^Claribel\\!$", "^Class", "^Collomb$", "^Comment$", "^COMMENT", "^Completed", "^COMPLETION", "^Conc$", "^Conceição$", "^Construct\\!", "^Construction\\!", "^Contact[s]*", "^Cont\\'d$", "^Dr\\s", "^Dr\\,", "^Drive\\,", "^Livability$", "^Location$", "^[Oo]n,*\\s", "^Ongoing", "^Outreach", "^OUTREACH", "^Overcrossing", "^P,\\sCaltrans\\sD12", "^P.O", "^Page,\\sTC", "^Phone$", "^Photo-documentation", "^Photograph", "^Proposed\\sPlan", "^ramp!", "^Respondent", "^Streets\\,\\sFresno", "^Stats$", "above\\,\\sSee", "^Street\\,", "STREET\\,")
  
remove.row <- paste(remove.row, collapse="|")
df$remove <- str_detect(df$Author, remove.row) 
table(df$remove) # Remove 1048 rows or 1078 with check
df <- df %>% filter(remove == F | is.na(remove))

# Remove if months, only one letter
remove <- c("^Year[s]*$", "^\\<-*-*$", "^\\d+$", "^[A-Za-z]{1}\\.*$", "^\\d+\\.*$",	"©", "^January$", "^February$", "^March$", "^April$", "^May$", "^June$", "^July$", "^August$", "^September$", "^October$", "^November$", "^December$", "Nov", "^Page$", "Surrogate$", "^Anonymous$", "^bbb$", "^Chairman$", "\\!")
remove <- paste(remove, collapse="|")
df$Author <- str_remove_all(df$Author, remove)
df$Author <- ifelse(df$Author == "", NA_character_, df$Author)
#Remove NAs are that in character form
df$Author <- str_remove_all(df$Author, "NA\\s")

## Reversing some order of things not initially captured well in the author formatting
# Detect where this is true for America
df$front <- str_extract(df$Author, "^America\\,\\s(?=.*)")
df$front <- str_remove(df$front, "\\,\\s")
df$front <- ifelse(is.na(df$front), FALSE, df$front)
df$Author <- str_remove(df$Author, "^America\\,\\s(?=.*)")
df$Author <- ifelse(df$front == "America", paste(df$Author, df$front), df$Author)

# Detect where this is true for Area
df$front <- str_extract(df$Author, "^Area\\,\\s(?=.*)")
df$front <- str_remove(df$front, "\\,\\s")
df$front <- ifelse(is.na(df$front), FALSE, df$front)
df$Author <- str_remove(df$Author, "^Area\\,\\s(?=.*)")
df$Author <- ifelse(df$front == "Area", paste(df$Author, df$front), df$Author)

# Detect where this is true for Bill
df$front <- str_extract(df$Author, "^Bill\\,\\s(?=.*)")
df$front <- str_remove(df$front, "\\,\\s")
df$front <- ifelse(is.na(df$front), FALSE, df$front)
df$Author <- str_remove(df$Author, "^Bill\\,\\s(?=.*)")
df$Author <- ifelse(df$front == "Bill", paste(df$Author, df$front), df$Author)

# Detect where this is true for Code
df$front <- str_extract(df$Author, "^Code\\,\\s(?=.*)")
df$front <- str_remove(df$front, "\\,\\s")
df$front <- ifelse(is.na(df$front), FALSE, df$front)
df$Author <- str_remove(df$Author, "^Code\\,\\s(?=.*)")
df$Author <- ifelse(df$front == "Code", paste(df$Author, df$front), df$Author)

# Detect where this is true for City of
df$front <- str_extract(df$Author, "(?<=^.{1,30})\\,\\sCity\\sof$")
df$front <- str_remove(df$front, "\\,\\s")
df$front <- ifelse(is.na(df$front), FALSE, df$front)
df$Author <- str_remove(df$Author, "(?<=^.{1,30})\\,\\sCity\\sof$")
df$Author <- ifelse(df$front == "City of", paste(df$front, df$Author), df$Author)

# Get rid of "^Available\\:" before a URL "\\<http"
df$Author <- str_remove(df$Author, "^Available\\:")

# Remove 	e.g. CCEER 15-06 from start of author because I think we have a lot of repeats
df$Author <- str_remove(df$Author, "^CCEER\\s\\d{2}\\-\\d{1,2}\\s")
# This got rid of 152 rows and 312-161
df <- distinct(df) 

# Remove month if [month],...
# What do to with multiply abbreviated/periods?

df.hold <- df
# PUBLISHER CLEANING ----
df <- df.hold
df$Publisher <- str_remove_all(df$Publisher, "^\\s+") 

# Standardize all of the names
name <- "Center for Civil Engineering Earthquake Research, University of Nevada"
df$Publisher <- ifelse(str_detect(df$Publisher, "^Center\\sfor\\sEarthquake\\sResearch|Center\\sfor\\sCivil\\sEngineering\\sEarthquake\\sResearch"), name, df$Publisher)

name <- "Center for Civil Engineering Earthquake Research, University of Nevada"
df$Publisher <- ifelse(str_detect(df$Publisher, "^University\\sof\\sNevada") & str_detect(df$Journal, "^Center\\sfor\\sCivil\\sEngineering\\sEarthquake\\sResearch|^Civil\\sEngineering\\sDepartment|^Department\\sof\\sCivil\\sEngineering"), name, df$Publisher)
df$Journal <- ifelse(str_detect(df$Publisher, name) & str_detect(df$Journal, "^Center\\sfor\\sCivil\\sEngineering\\sEarthquake\\sResearch"), NA_character_, df$Journal)

name <- "American Association of State Highway and Transportation Officials"
df$Publisher <- ifelse(str_detect(df$Publisher, "^American\\sAssociation\\sof\\sState\\sand\\sHighway|^American\\sAssociation\\sof\\sState\\sHighway"), name, df$Publisher)

# Could continue: World Bank, World Health Organization, McGraw Hill, Wiley, Universities / UC, US Department of, Fish...

# Got A-C and Z-
remove.row <- c("^\\¿", "^\\@", "^\\§", "^\\#", "^\\%", "^\\$", "^✓", "^❖","^♦", "^\\♦", "^◦", "BRUSSEL\\sSPROUT", "\\d,\\d{3},\\d{3}", "\\d{1,2}:\\d{2}", "\\/\\sRules\\sand\\sRegulations", ">>> Abhijit Bagde", "^\\>\\sAccessed", "^\\d{3,}", "^\\=\\s\\$*\\d{3}", "ND\\s\\d\\.*\\d{1,6}\\sNA\\s", "B3J0\\d{3}", "^Proposed\\sAM", "^Proposed\\sPM", "^Surrogate", "^Year\\s\\d$", "Name\\:\\sYear\\:\\sAddress\\:", "Ave\\s@\\s|Rd\\s@\\s|Dr\\s@\\s|Blvd\\s@\\s|Ln\\s@\\s", "^\\*{8}", "^\\*{5}", "\\d{2}\\-\\d{2}\\-\\d{2}", "\\d{1,2}\\syears\\sof", "^[[:punct:]]$", "^\\?\\s1$", "^\\*+\\sPer\\sthe", "^\\*\\w", "\\\\+", "^\\˃", "^\\++", "^\\<$", "^\\=", "^\\¢", "^_+\\s", "^_+[[:alpha:]]", "^_+$", "ND\\s20\\sNA", "^Photo", "^Image", "^\\:\\s", "^\\&", "^\\*", "^\\`", "^\\^", "^Comment\\sNoted", "^\\!Brigsmore", "^[Aa]ction$", "^AM\\s", "^APN$", "^Broadway$", "^Center$", "^[Cc]hange$", "^Choices\\.Community$", "^Clerk-*Recorders\\sOffice", "^CLIENT", "^Climate\\sChange\\sand\\sGreenhouse", "^Climate\\sChange$", "\\bAve\\b", "Avenue", "^Zones", "^YouGov", "^Wrong-Way", "^Workable", "^West\\sWest", "Specific\\sPlan$", "^UPDATE$", "^Updatable$")
remove.row <- paste(remove.row, collapse="|")
df$remove <- str_detect(df$Publisher, remove.row) 
table(df$remove) # Remove 113 rows or 136
df <- df %>% filter(remove == F | is.na(remove))

# Remove these bits
remove <-  c("�", "\\","", "\\s$", "©", "»", "^\\_+", "^\\_+\\.", "^\\___", "^\\-+", "^\\—+", "^\\--\\s", "\\'", "^\\&", "\\•", "\\|", "^\\.", "\"", "\\‘", "\\’", "\\“", "\\”", "\\„", "\\", "·", "\\·", "^\\)", "^\\/+", "^\\,", "^January$", "^February$", "^March$", "^April$", "^May$", "^June$", "^July$", "^August$", "^September$", "^October$", "^November$", "^December$", "^Accessed$", "^Year$", "^[A-Za-z]{1}\\.*$", "^\\d{1,2}\\s|^\\d{1,2}\\-|^\\d{1,2}$","^Address$", "^Adopted$", "^age\\,*", "^and\\s", "^Association$", "^Attachments$", "^author$", "^CA$", "^California$", "^City\\sof$", "^Ibid$", "^Conf", "^Year\\sFour", "WORK\\sUNIT\\sNUMBER\\s", "Work\\sUnit\\sNo\\.\\s", "^West$", "^University$", "^U\\.S\\.$", "^Monday$", "^Tuesday$", "^Wednesday$", "^Thursday$", "^Friday$")
remove <- paste(remove, collapse="|")
df$Publisher <- str_remove_all(df$Publisher, remove)
df$Publisher <- ifelse(df$Publisher == "", NA_character_, df$Publisher)

# Follows up based on challenges
df$Publisher <- str_remove_all(df$Publisher, "^\\s+") 
df$Publisher <- str_remove_all(df$Publisher, "^--|^\\.|^\\‐+|^\\–+")
df$Publisher <- str_remove_all(df$Publisher, "^―|^-|^\\.---~")
df$Publisher <- str_remove_all(df$Publisher, "^\\s+") 

# Make NA if they have these patterns
pattern <- c("^Yes")
pattern <- paste(pattern, collapse="|")
df$Publisher <- ifelse(str_detect(df$Publisher, pattern), NA_character_, df$Publisher)

df <-distinct(df)

# An exploratory check
#cceer <- df %>% filter(Publisher == "Center for Civil Engineering Earthquake Research, University of Nevada") #968

df$Publisher <- ifelse(df$Publisher == "", NA_character_, df$Publisher)                      
df.hold <- df

# URL CLEANING ----
df <- df.hold
# For all columns (Author-Publisher) If it starts with this: <http://www put into url
df$extract <- str_extract(df$Author, "^<http.*|www\\..*")
df$extract <- str_extract(df$Title, "^<http.*|www\\..*")
df$extract <- str_extract(df$Journal, "^<http.*|www\\..*")
# If there is no URL, use extracted url, but if there is a URL, use that
df$url.new <- ifelse(is.na(df$URL), df$extract, df$URL)

df$URL <- df$url.new
remove <- c("\\<", "\\>.*", "^doi\\:", "^from\\:", "^e-mail\\:", "gintAutoCreateInput\\:", "gintBlobType\\:", "gintCaption\\:", "gintCaptionVertical\\:", "^htp\\:\\/\\/trid\\.$", "^http\\:$" )
remove <- paste(remove, collapse="|")
df$URL <- str_remove_all(df$URL, remove)
df$URL <- ifelse(df$URL == "", NA_character_, df$URL) 


#df$Author <- ifelse(str_detect(df$Author, "^<http|www\\."), NA_character_, df$Author)
#df$Title <- ifelse(str_detect(df$Title, "^<http|www\\."), NA_character_, df$Title)
#df$Journal <- ifelse(str_detect(df$Journal, "^<http|www\\."), NA_character_, df$Journal )

df.hold <- df
# COMBINED CLEANING ----
df <- df.hold

# WHAT TO DO WITH NUMBER TITLES?   
# If Journal starts with a number then a space or punctuation, and there is no title or publisher, remove               
# If Title starts with a number, and does not have anything in the title, journal or publihser, get rid of it
df$remove <- ifelse(str_detect(df$Title, "^\\d+") == T &
                      is.na(df$Journal) == T &
                      is.na(df$Publisher) == T &
                      is.na(df$DOI) == T &
                      is.na(df$URL) == T, T, NA)
table(df$remove)
remove <- c("^\\d\\,\\sissued", "^\\d{1,2}\\.\\d{2}%", "^1\\s*St", "^3D", "%", "^79\\sGreen", "^18\\sThe", "^73\\sMoore", "^14\\sFWY", "^9\\smiles", "^8\\srequesting", "^32\\sBeyond", "^1\\svideo", "^10\\scharge", "^29\\sjunction", "^10\\sFeet", "\\$", "^20\\sminute", "^7\\sand", "^4\\sworkshops")
remove <- paste(remove, collapse="|")
df$remove.all <- ifelse(df$remove == T | str_detect(df$Title, remove), TRUE, FALSE)
table(df$remove.all)
df <- df %>% filter(is.na(remove.all))

df$Title <- str_remove_all(df$Title, "10-measuring-the-street\\.pdf\\s\\s")
df$Title <- ifelse(df$Title == "", NA_character_, df$Title) 

# WHAT TO DO WITH NUMBER JOURNALS?   

# If JOURNAL starts with the following patterns, replace it with an NA
remove <- c("^\\d{1,2}$", "^\\d{1,2}\\.\\d{1,2}%*", "^\\d{1,2}[[:punct:]]", "^\\d{1,2}\\s", "^\\d{1,2}am", "^\\d{1,2}pm")
remove <- paste(remove, collapse="|")
df$Journal <- ifelse(str_detect(df$Journal, remove), NA,df$Journal)


# Omit if there is no author, title, url or year
df$remove <- ifelse(is.na(df$Author) & is.na(df$Title) & is.na(df$Journal) & is.na(df$Publisher) & is.na(df$DOI) & is.na(df$URL), T, F)
table(df$remove)
df <- df %>% filter(remove == F)

df <- df %>% select(Author, Year, Title, Journal, Publisher, DOI, URL, File, level, agency)

# Make sure I have distinct entries
df <- distinct(df)
# was #73,232 citations, now 62,522 = 10,710 removed.

write.csv(df, "~/Box/truckee/data/compiled_anystyle_results/combined_anystyle_citations_clean_check.csv", row.names = F)



