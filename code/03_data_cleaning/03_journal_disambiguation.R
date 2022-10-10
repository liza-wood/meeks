#devtools::install_github("ikashnitsky/sjrdata")
library(rvest)
library(sjrdata)
library(dplyr)
library(stringr)
library(data.table)
library(tools)
library(tidyverse)
source("code/anystyle_citation_cleaning/functions.R")

# 1. SCIMAGO DATA ----

## sjr_journals package and cross checked with my own downloads
scimago <- data.frame(sjr_journals)
scimago <- data.frame(sjr_journals) %>% filter(year == 2015)

scimago$title <- str_remove_all(scimago$title, "\\.|\\,|\\;|\\*|\\-")
scimago$title <- base::trimws(scimago$title)
scimago$title <- toTitleCase(scimago$title)
colnames(scimago)[4] <- "Journal"


# 2. SEMI-CLEAN COMBINED DATA ----
df.notclean <- read.csv("~/Box/truckee/data/compiled_anystyle_results/combined_anystyle_citations_clean_check.csv")
df <- read.csv("~/Box/truckee/data/compiled_anystyle_results/combined_anystyle_citations_clean_check.csv")
df <- select(df, -agency)
# One more bit of cleaning ----
df$DOI <- ifelse(str_detect(df$Journal, "^:"), df$Journal, df$DOI)
df$DOI <- ifelse(str_detect(df$DOI, "^:"), str_extract(df$DOI, "(?<=^:\\/\\/doi\\s\\.org\\/).*"), df$DOI)
df$DOI <- ifelse(str_detect(df$DOI, "SFMTA"), str_extract(df$DOI, ".*(?=\\sSFMTA)"), df$DOI)
df$Journal <- ifelse(str_detect(df$Journal, "^<http|^:"), NA, df$Journal)

# Remove some things
df$Journal <- str_remove_all(df$Journal, "\\.|\\,|\\;|\\*|\\-")
df$Journal <- base::trimws(df$Journal)
df$Journal <- str_replace_all(df$Journal, "\\&", "and")
df$Journal <- toTitleCase(df$Journal)

df$Publisher <- str_remove_all(df$Publisher, "\\.|\\,|\\;|\\*|\\-")
df$Publisher <- base::trimws(df$Publisher)
df$Publisher <- str_replace_all(df$Publisher, "\\&", "and")
df$Publisher <- toTitleCase(df$Publisher)

# Need to get rid of journals that are longer than 200 characters, which is where most of my issues are coming in the later choosing of authors
df <- df %>% 
  mutate(n.char = nchar(Journal)) 
df$Journal.new <- ifelse(df$n.char > 200, NA, df$Journal)

df$Journal <- df$Journal.new

# 3. CLEANED ANYSTYLE CITATIONS DATA ----

df$Journal <- toTitleCase(df$Journal)
df$Publisher <- toTitleCase(df$Publisher)

## CLEAN JOURNAL ABBREVIATIONS ----

## STOP HERE TO THINK ABOUT WHAT WE KNOW ABOUT AGENCY ET AL AUTHORS !!

# Adv[.]? should be Advances -- see Adv for some inspiration
df$Journal <- str_replace(df$Journal, "Adv\\b|Advn\\b", "Advances in")
# Agric[.]? for Agricultur
df$Journal <- str_replace(df$Journal, "Agric\\b", "Agriculture")
# Anim. = Animal
df$Journal <- str_replace(df$Journal, "Anim\\b", "Animal")
# Am J = American Journal of
df$Journal <- str_replace(df$Journal, "Am\\sJ\\b", "American Journal of")
# Am = America at end
df$Journal <- str_replace(df$Journal, "Am$|Amer$", "America")
# Am = American 
df$Journal <- str_replace(df$Journal, "Am\\b|Amer\\b", "American")
# Ann. is Annals of
df$Journal <- str_replace(df$Journal, "Ann\\b", "Annals")
# Annu. is annual
df$Journal <- str_replace(df$Journal, "Annu\\b", "Annual")
# Atmos.is Atmospheric
df$Journal <- str_replace(df$Journal, "Atmos\\b", "Atmospheric")
# Assoc.= Association
df$Journal <- str_replace(df$Journal, "Assoc\\b", "Association")
# Appl. is Applied
df$Journal <- str_replace(df$Journal, "Appl\\b", "Applied")
# Biol. = Biology
df$Journal <- str_replace(df$Journal, "Biol\\b", "Biology")
#Behav = Behavior
df$Journal <- str_replace(df$Journal, "Behav\\b", "Behavior")
#Bull = Bulletin
df$Journal <- str_replace(df$Journal, "Bull\\b", "Bulletin of")
# Cem Bas Mat
df$Journal <- str_replace(df$Journal, "Cem\\sBas\\sMat[a-z]?\\b", "Cement-Based Materials")
# Cem Bas Mat
df$Journal <- str_replace(df$Journal, "Cem\\-Bas\\sMat[a-z]?\\b", "Cement-Based Materials")
# Civ = Civil
df$Journal <- str_replace(df$Journal, "Civ\\b", "Civil")
# Climatol = Climatology
df$Journal <- str_replace(df$Journal, "Climatol\\b", "Climatology")
# Conf = Consference
df$Journal <- str_replace(df$Journal, "Conf\\b", "Conference")
# Conserv = Conservation
df$Journal <- str_replace(df$Journal, "Conserv\\b", "Conservation")
# Comput = Computing
df$Journal <- str_replace(df$Journal, "Comput\\b", "Computing")
# Constr = Constructions
df$Journal <- str_replace(df$Journal, "Constr\\b", "Construction")
# Corro = Corrosion
df$Journal <- str_replace(df$Journal, "Corros?\\b", "Corrosion")
# Croat == Croation
df$Journal <- str_replace(df$Journal, "Croat?\\b", "Croatian")
# Earthq.= Earthquake
df$Journal <- str_replace(df$Journal, "Earthq\\b", "Earthquake")
# Ecol[.]? should be Ecology
df$Journal <- str_replace(df$Journal, "Ecol\\b", "Ecology")
# Eng[.]? should be Engineering
df$Journal <- str_replace(df$Journal, "Eng\\b", "Engineering")
# Environ. = Environment at end
df$Journal <- str_replace(df$Journal, "Environ$", "Environment")
# Environ. = Environmtnal
df$Journal <- str_replace(df$Journal, "Environ\\b|Env\\b", "Environmental")
# Ergon Ergonomics
df$Journal <- str_replace(df$Journal, "Ergon\\b", "Ergonomics")
# Epidemiol
df$Journal <- str_replace(df$Journal, "Epidemiol\\b", "Epidemiology")
# European Euro
df$Journal <- str_replace(df$Journal, "Euro?\\b", "European")
# Genet
df$Journal <- str_replace(df$Journal, "enet\\b", "Genetics")
# Geophys
df$Journal <- str_replace(df$Journal, "Geophys\\b", "Geophysics")
# Geol. = Geology
df$Journal <- str_replace(df$Journal, "Geol\\b", "Geology")
# Geoenv Geoenvi.
df$Journal <- str_replace(df$Journal, "Geo[Ee]nvi?r?o?n?\\b", "Geoenvironmental")
# Geotech
df$Journal <- str_replace(df$Journal, "Geotech\\b", "Geotechnical")
# Hous
df$Journal <- str_replace(df$Journal, "Hous\\b", "Housing")
# Hydrogeol
df$Journal <- str_replace(df$Journal, "Hydrogeol\\b", "Hydrogeology")
# Hydrol
df$Journal <- str_replace(df$Journal, "Hydrol\\b", "Hydrology")
# Ieee
df$Journal <- str_replace(df$Journal, "^Ieee\\b", "IEEE")
# Int = International
df$Journal <- str_replace(df$Journal, "Int\\b", "International")
# J[.]? should be Journal, if at end
df$Journal <- str_replace(df$Journal, "J$", "Journal")
# J[.]? should be Journal of, if at start
df$Journal <- str_replace(df$Journal, "J\\,?\\b", "Journal of")
# Mater = Materials
df$Journal <- str_replace(df$Journal, "Mat\\b|Mater\\b", "Materials")
# Mech = Mechanical
df$Journal <- str_replace(df$Journal, "Mech\\b", "Mechanical")
# Psychol = Psychology
df$Journal <- str_replace(df$Journal, "Psychol\\b", "Psychology")
# Sci = Science
df$Journal <- str_replace(df$Journal, "Sci\\b", "Science")
# Seis.= Siesmic
df$Journal <- str_replace(df$Journal, "Seism?\\b", "Seismological")
# Soc = Society
df$Journal <- str_replace(df$Journal, "Soc\\b", "Society")
# Sociol = Sociology
df$Journal <- str_replace(df$Journal, "Sociol\\b", "Sociology")
# Softw = Software
df$Journal <- str_replace(df$Journal, "Softw\\b", "Software")
# Stud
df$Journal <- str_replace(df$Journal, "Stud\\b", "Studies")
# Struct = Structural
df$Journal <- str_replace(df$Journal, "Struct\\b", "Structural")
# Resour. = Resources
df$Journal <- str_replace(df$Journal, "Resour\\b", "Resources")
# Rev. = Review at end
df$Journal <- str_replace(df$Journal, "Rev$", "Review")
# Rev. = Review of
df$Journal <- str_replace(df$Journal, "Rev\\b", "Review of")
# Zool = Zoology
df$Journal <- str_replace(df$Journal, "Zool\\b", "Zoology")

## CLEAN SPECIFIC JOURNAL NAMES THAT ARE POPULAR BUT OFF FROM SCIMAGO LIST ----

# Should be "Accident Analysis and Prevention"
pattern <- c("^Accid\\sAnal$", "^Accid\\sAnal\\sPrev$", "^Accident\\sAnal\\sPrev[a-z]*", "^AccidAnalPrev$", "^Accident\\sAnaly[Ss][Ii][Ss]\\sand\\sPrevention.*$", "^Spatial Patterns Accident Analysis and Prevention$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "Accident Analysis and Prevention")

# Should be ACI Structural Journal
pattern <- c("^ACI\\sStructural\\sJournal\\sv$", "^ACI\\sStructural\\sJournal\\sMarchApril$", "^ACI\\sStructural\\sJournal\\sSP$", "ACI\\sStructural\\sJournal\\sTitle$", "^ACI\\sStructures\\sJournal$", "^American\\sConcrete\\sInstitute\\sACI\\sStructural\\sJournal$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "ACI Structural Journal")

# Shoudl be "ACI Materials Journal"
pattern <- c("ACI Material Journal")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "ACI Materials Journal")

# Should be "Concrete International"
pattern <- c("^ACI Concrete International$", "^ACI Concrete Journal$", "^Concr Int$", "^Concrete International\\: Design and Construction$", "^Journal of American Concr Inst Proceedings$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "Concrete International")

# Should just be ACI
pattern <- c("^ACI SP$", "^ACI Special Publication$", "^ACI Monograph$", "^ACI Journal Proceedings$", "^ACI Journal$", "^ACIJournal$", "^ACI Committee$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "ACI")

# Anything that starts with AHMCT Should be AHMCT Report
df$Journal <- str_replace(df$Journal, "^AHMCT.*", "AHMCT Report")

# Should be Engineering Journal
df$Journal <- str_replace(df$Journal, "^ASHI Engineering Journal$", "Engineering Journal")

# American Journal of Preventive Medicine
df$Journal <- str_replace(df$Journal, "^American Journal of Prev.*", "American Journal of Preventive Medicine")

# Remove ASCE or ASME before Journal or AISC
df$Journal <- str_remove(df$Journal, "^ASCE\\s(?=J)|^ASME\\s(?=J)|\\sASCE$|\\sAISC$")

# Journal of Geotechnical Engineering should add in envt
df$Journal <- str_replace(df$Journal, "^Journal of Geotechnical Engineering$", "Journal of Geotechnical and Geoenvironmental Engineering")

# BMJ
df$Journal <- str_replace(df$Journal, "^Bmj$|^BMJournal$|^Systematic Review Bmj$|^Systematic Review British Medical Journal$", "BMJ Open")

# Bulletin of the Seismological Society of America	
pattern <- c("^Bulletin of Seismological  Society America$", "^Bulletin of Seismological Society America", "^Bulletin of Seismological Society American Ti$", "^Bulletin of Seismological Society of America$", "^Bulletin of the Seismological Soceity of America$", "^Bulletin of the Seismological Society of American$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "Bulletin of the Seismological Society of America")

#Bulletin of Earthquake Engineering
df$Journal <- str_replace(df$Journal, "^Bulletin of Earthquake Engineering DOI.*$", "Bulletin of Earthquake Engineering")

#Bulletin of Engineering Geology and the Environment
df$Journal <- str_replace(df$Journal, "^Bulletin of Engineering Geology Environment$", "Bulletin of Engineering Geology and the Environment")

#Can Journal = Canadian Journal
df$Journal <- str_replace(df$Journal, "^Can\\sJournal", "Canadian Journal")
df$Journal <- str_replace(df$Journal, "^Canadian Journal of For Res$", "Canadian Journal of Forest Research")
df$Journal <- str_replace(df$Journal, "^Canandian Journal of Public Health	$", "Canadian Journal of Public Health	")

# Canadian Geotechnical Journal
df$Journal <- str_replace(df$Journal, "^Can Geotechnical Journal$|^Can Geotechnical Journal of  Ottawa$|^Canadian Geotechnical Journal Journal$", "Canadian Geotechnical Journal")

# Cement and Concrete Composites
pattern <- c("^Cem Conc?r? Compo?s?$", "^Cement \\& Concrete Composites$", "^Cement and Concrete Composits$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "Cement and Concrete Composites")

# Cement and Concrete Research
pattern <- c("^Cem Conc?r? Res$", "^Cement Concrete Research$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "Cement and Concrete Research")

# Climatic Change
df$Journal <- str_replace(df$Journal, "^Climactic\\sChange$|^Climate\\sChange$", "Climatic Change")

# Composites Part B: Engineering
pattern <- c("^Composites\\: Part B", "Composites B$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "Composites Part B\\: Engineering")

# Composites Part A: Applied Science and Manufacturing
df$Journal <- str_replace(df$Journal, "^Composites: Part A$", "Composites Part A: Applied Science and Manufacturing")

# Comptes Rendus Geoscience
df$Journal <- str_replace(df$Journal, "^Comptes Rendus Geoscience$", "Comptes Rendus  Geoscience
")

# Computational Materials Science
df$Journal <- str_replace(df$Journal, "^Computational Material Science$", "Computational Materials Science")

# Remove Elsevier Ltd
df$Journal <- str_remove(df$Journal, "\\sElsevier Ltd$")

# Conservation Biology Pp
df$Journal <- str_replace(df$Journal, "^Conservation Biology Pp$", "Conservation Biology")

# Construction and Building Materials
c("^Const Build Materials$", "^Construct Build Materials$", "^Construction and Building Material$", "^Construction Bldg Materials$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "Construction and Building Materials")

# Corro Review
df$Journal <- str_replace(df$Journal, "Corrosion Review", "Corrosion Reviews")

# Critical Reviews In Environmental Science and Technology
df$Journal <- str_replace(df$Journal, "^Critical Reviews In Environmental Science and Toxicology$", "Critical Reviews In Environmental Science and Technology")

#Earthquake Engineering and Structural Dynamics
df$Journal <- str_replace(df$Journal, "^Earhquake Engineering Structural Dyn$|^Earthquake Engin$|^Earthquake Engineering and Structural Dynamics\\(40$|^Earthquake Engineering and Structural DynamicsVol$|^Earthquake Engineering Structural Dyn$|^Earthquake Engng Structural Dyn$|^Engineering Structural Dyn$|^Engineering Structural Dyn$", "Earthquake Engineering and Structural Dynamics")

#Earth and Planetary Science Letters
df$Journal <- str_replace(df$Journal, "^Earth Planet Science Lett$", "Earth and Planetary Science Letters")

#Earth Surface Processes and Landforms
df$Journal <- str_replace(df$Journal, "^Earth Surf Process Landforms$", "Earth Surface Processes and Landforms")

# Ecological Applications (this is an issue from previous code)
df$Journal <- str_replace(df$Journal, "^Ecology Applications$", "Ecological Applications")
df$Journal <- str_replace(df$Journal, "^Ecology Econ$", "Ecological Economics")
df$Journal <- str_replace(df$Journal, "^Ecology Modell$", "Ecological Modelling")
df$Journal <- str_replace(df$Journal, "^Ecology Monogr$", "Ecological Monographs")
df$Journal <- str_replace(df$Journal, "^Ecology Res$", "Ecological Research")
df$Journal <- str_replace(df$Journal, "^Ecology Society$", "Ecology and Society")

# Econometrica
df$Journal <- str_replace(df$Journal, "^Econometrica\\: journal of the Econometric Society", "Econometrica")

# EconPapers
df$Journal <- str_replace(df$Journal, "^EconPapers$", "Economic Papers")

# Ecotoxicology and Environmental Safety
df$Journal <- str_replace(df$Journal, "^Ecotox\\/cologyand En vironmental Safety$|^Ecotoxlcology and Environmental Safety$", "Ecotoxicology and Environmental Safety")

# Energy and Fuels
df$Journal <- str_replace(df$Journal, "^Energy and Fuels$", "Energy \\& Fuels")

# Energy Policy 33
df$Journal <- str_replace(df$Journal, "^Energy Policy 33$", "Energy Policy")

# Energy Research & Social Science
df$Journal <- str_replace(df$Journal, "^Energy Research \\& Social Science$", "Energy Research and Social Science")

# Energy Strategy Review
df$Journal <- str_replace(df$Journal, "^Energy Strategy Review$", "Energy Strategy Reviews")

# Engineering Failure Analysis
df$Journal <- str_replace(df$Journal, "^Engineering Fail Anal$", "Engineering Failure Analysis")

# Engineering Structures
df$Journal <- str_replace(df$Journal, "^Engineering Structural$|^Engineering Structures.*", "Engineering Structures")

# Environment 119 and Behavior
df$Journal <- str_replace(df$Journal, "^Environment 119 and Behavior$", "Environment and Behavior")

# Environment and Planning Part A
df$Journal <- str_replace(df$Journal, "^Environment and Planning Part A$", "Environment and Planning A")

# Environment and Planning B\\: Planning and Design
pattern <- c("^Environment and Planning B$", "^Environment and Planning B\\: Planning and Design 25$", "^Environment and Planning\\: Part B$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "Environment and Planning B\\: Planning and Design")

# Environment and Planning C: Government and Policy
df$Journal <- str_replace(df$Journal, "^Environment and Planning C\\: Government and Policy$", "Environment and Planning C\\: Government and Policy")

# Environmental Health Perspectives
df$Journal <- str_replace(df$Journal, "^Environmental Health Persp?e?c?t?$|^Environmental Health Perspective$", "Environmental Health Perspectives")

# Environmental Modeling and Assessment
df$Journal <- str_replace(df$Journal, "^Environmental Modeling and Assessment 8$", "Environmental Modeling and Assessment")

# Environmental Research Letters
df$Journal <- str_replace(df$Journal, "^Environmental Research Letter$|^Environmental Res Lett$", "Environmental Research Letters")

# Environmental Science and Technoiogy
df$Journal <- str_replace(df$Journal, "^Environmental Science and Technoiogy$|^Environmental Science and Techn?ology$|^Environmental Science Tech$|^Environmental Science Technology$", "Environmental Science & Technology")

#Environmental Technology
df$Journal <- str_replace(df$Journal, "^Environmental Technology$", "Environmental Technology (United Kingdom)")

# Epidemiology (Cambridge
df$Journal <- str_replace(df$Journal, "^Epidemiology \\(Cambridge$", "Epidemiology")

# Estuaries Coasts
df$Journal <- str_replace(df$Journal, "^Estuaries Coasts$", "Estuaries and Coasts")

# European Journal of Operational Research
pattern <- c("^European Journal of of Operational Research$", "^European Journal of Opera-Tional Research$", "^European Journal of Operation Research$|^European Journal of Operational$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "European Journal of Operational Research")

# European Transport
df$Journal <- str_replace(df$Journal, "^European Transport$", "European Transport  Trasporti Europei")

# Experiment Smart Materials and Structures
df$Journal <- str_replace(df$Journal, "^Experiment Smart Materials and Structures$", "Smart Materials and Structures")

# Experimental Techniques Structural Testing Series: Part
df$Journal <- str_replace(df$Journal, "^Experimental Techniques Structural Testing Series: Part$", "Experimental Techniques")

# Expert Syst Applied
df$Journal <- str_replace(df$Journal, "^Expert Systems with Applications$", "Expert Syst Applied")

# Explor Geophysics
df$Journal <- str_replace(df$Journal, "^Explor Geophysics$", "Exploration Geophysics")

# Federal register
df$Journal <- str_replace(df$Journal, "^Federal Register.*", "Federal Register")

# Forthcoming Transportation Research Record: Journal of the Transportation Research Board
df$Journal <- str_replace(df$Journal, "^Forthcoming Transportation Research Record: Journal of the Transportation Research Board$|^Transportation Research Rec.*|^Metropolitan Area Transportation Research Record Journal of the Transportation Research Board$|^Research Rec.*|^The Journal of Transportation Research Board$", "Transportation Research Record")

# Foundations and Trends R in Machine Learning
df$Journal <- str_replace(df$Journal, "^Foundations and Trends R in Machine Learning$", "Foundations and Trends in Machine Learning")

# Generation? Journal of the American Planning Association
df$Journal <- str_replace(df$Journal, "^Generation\\? Journal of the American Planning Association$|^Longer View Journal of the American Planning Association$", "Journal of the American Planning Association")

# Geophysics Journal of Int
df$Journal <- str_replace(df$Journal, "^Geophysics Journal of Int.*$|^Geophysics Journal of R Astron Society$", "Geophysical Journal International")

# Geophysical Prospecting
df$Journal <- str_replace(df$Journal, "^Geophysics Prosp$", "Geophysical Prospecting")

# Geophysical Research Letters
df$Journal <- str_replace(df$Journal, "^Geophysics Res L?e?t?t?e?r?s?$", "Geophysical Research Letters")

# Geoscience Cananda
df$Journal <- str_replace(df$Journal, "^Geosciences$", "Geoscience Canada")

# Geosynthet Int
df$Journal <- str_replace(df$Journal, "^Geosynthet International$", "Geosynthetics International")

# Geotechnical Geology Engineering
df$Journal <- str_replace(df$Journal, "^Geotechnical Geology Engineering$", "Geotechnical and Geological Engineering")

# Geotechnical Test Journal
df$Journal <- str_replace(df$Journal, "^Geotechnical Test Journal$|^Geotechnical Testing Journal American Society for Testing and Materials$|^Geotechnical Testing Journal ASTM$|^Geotechnical Testing Journal GTJODJournal$", "Geotechnical Testing Journal")

# Géotechnique
df$Journal <- str_replace(df$Journal, "^Géotechnique$", "Geotechnique")

# Global Ecology and Biogeography Letters
df$Journal <- str_replace(df$Journal, "^Global Ecology and Biogeography Letters$", "Global Ecology and Biogeography")

# Highway Res Rec
df$Journal <- str_replace(df$Journal, "^Highway Res Rec$|^Highway Research Record", "Highway Research Record")

# Human Factors: The Journal of the Human Factors and Ergonomics Society
df$Journal <- str_replace(df$Journal, "^Human Factors: The Journal of the Human Factors and Ergonomics Society$", "Human Factors")

# Hydrogeology Journal
df$Journal <- str_replace(df$Journal, "^Hydrogeol Journal$", "Hydrogeology Journal")

# Hydrol Process
df$Journal <- str_replace(df$Journal, "^Hydrology Process$", "Hydrology Processes")

# IEEE Conference on Intelligent Transportation System
df$Journal <- str_replace(df$Journal, "^IEEE Conference on Intelligent Transportation System$|^IEEE Intelligent Transportation Systems|^IEEE ITS Conference$", "IEEE Conference on Intelligent Transportation Systems Proceedings ITSC")

# IEEE 66th Vehicular Technology Conference (VTC
df$Journal <- str_replace(df$Journal, "^IEEE \\d\\dth Vehicular Technology Conference|^IEEE \\d\\dnd Vehicular Technology Conference", "IEEE Vehicular Technology Conference")

#IEEE Intelligent Vehicles Symposium Proceedings
df$Journal <- str_replace(df$Journal, "^IEEE Intelligent Vehicles Symposium|^IEEE on Intelligent Vehicles Symposium", "IEEE Intelligent Vehicles Symposium Proceedings")

# IEEE Journal of Select Topics Applied Earth Obs Remote Sens
df$Journal <- str_replace(df$Journal, "^IEEE Journal of Select Topics Applied Earth Obs Remote Sens$", "IEEE Journal of Selected Topics in Applied Earth Observations and Remote Sensing")

# IEEE Journal on Selected Areas in Communications/Supplement
df$Journal <- str_replace(df$Journal, "^IEEE Journal on Selected Areas in Communications/Supplement$", "IEEE Journal on Selected Areas in Communications")

# IEEE Trans Geosci Remote Sens
df$Journal <- str_replace(df$Journal, "^IEEE Trans Geosci Remote Sens$|^IEEE Transact Geosci Remote Sens$|^Journal of IEEE Transact Geosci Remote Sensing$", "IEEE Transactions on Geoscience and Remote Sensing")

# IEEE Trans Instrum Meas
df$Journal <- str_replace(df$Journal, "^IEEE Trans Instrum Meas$", "IEEE Transactions on Instrumentation and Measurement")

# IEEE Trans Intell Transp Syst
df$Journal <- str_replace(df$Journal, "^IEEE Trans Intell Transp Syst$|^IEEE Trans on Intel Trans Syst?
$|^IEEE Trans on Intelligent Transportation Systems$|^IEEE Transactions in ITS$|^IEEE Transaction on Intelligent Transportation Systems$|^IEEE Transportation on Intelligent Transportation Systems$|^Intelligent Transportation Systems IEEE Transactions on$", "IEEE Transactions on Intelligent Transportation Systems")

# IEEE Trans on Education
df$Journal <- str_replace(df$Journal, "^IEEE Trans on Education$", "IEEE Transactions on Education")

# IEEE Trans on Inform Theory
df$Journal <- str_replace(df$Journal, "^IEEE Trans on Inform Theory$", "IEEE Transactions on Information Theory")

# IEEE Trans on Signal Processing
df$Journal <- str_replace(df$Journal, "^IEEE Trans on Signal Processing$", "IEEE Transactions on Signal Processing")

# IEEE Trans on Veh Tech
df$Journal <- str_replace(df$Journal, "^IEEE Trans on Veh Tech$|^IEEE Transactions on v Ehicular Technology$", "IEEE Transactions on Vehicular Technology")

# IEEE Transactions Automatic Control to Appear
df$Journal <- str_replace(df$Journal, "^IEEE Transactions Automatic Control to Appear$", "IEEE Transactions on Automatic Control")

# IEEE Transactions on Control System Technology
df$Journal <- str_replace(df$Journal, "^IEEE Transactions on Control System Technology$|^IEEE Transactions on Control Systems$|^IEEE TRANSACTIONS on CONTROL SYSTEMS TECHNOLOGY$", "IEEE Transactions on Control Systems Technology")

# IEEE Transactions on Systems Man and Cybernetics
df$Journal <- str_replace(df$Journal, "^IEEE Transactions on Systems Man and Cybernetics$", "IEEE Transactions on Systems Man and Cybernetics\\: Systems")

# IEEE Vehicular Networking Conference (VNC) Amsterdam (the Netherlands
df$Journal <- str_replace(df$Journal, "^IEEE Vehicular Networking Conference \\(VNC\\) Amsterdam \\(the Netherlands$", "IEEE Vehicular Networking Conference VNC")

# IEEE Vehicular Technol Mag
df$Journal <- str_replace(df$Journal, "^Vehicular Technol Mag$", "Vehicular Technology Magazine")

# IEEE Wireess Communications
df$Journal <- str_replace(df$Journal, "^IEEE Wireess Communications$|^IEEEE Wireless Telecommunications Symposium$", "IEEE Wireless Communications")

# IEEE/ASME Transactions on Mechanics
df$Journal <- str_replace(df$Journal, "^IEEE\\/ASME Transactions on Mechanics$|^IEEE/ASME TRANSACTIONS on MECHATRONICS$", "IEEE\\/ASME Transactions on Mechatronics")

# IEEJ Transactions on Electrical and Electronic Engineering
df$Journal <- str_replace(df$Journal, "^IEEJournal of Transactions on Electric and Environmental Engineering$", "IEEJ Transactions on Electrical and Electronic Engineering")

# IET Journal of Intelligent Transportation Systems
df$Journal <- str_replace(df$Journal, "^IET Journal of Intelligent Transportation Systems$", "IET Intelligent Transportation Systems")

#  Industrial & Engineering Chemistry Research
df$Journal <- str_replace(df$Journal, "^Indust Engineering Chem$|^Industrial and Engineering Chemistry Research$", "Industrial & Engineering Chemistry Research")

# Industrial Electronics IEEE Transactions on
df$Journal <- str_replace(df$Journal, "^Industrial Electronics IEEE Transactions on$", "IEEE Transactions on Industrial Electronics")

# Inhalation Toxicology
df$Journal <- str_replace(df$Journal, "^Inhal Toxicol$", "Inhalation Toxicology")

# Inj Prev
df$Journal <- str_replace(df$Journal, "^Inj Prev$|^Injury Prevention \\[serial Online$", "Injury Prevention")

# Internation Journal of Project Management
df$Journal <- str_replace(df$Journal, "^Internation Journal of Project Management$", "International Journal of Project Management")

# International Journal of Solids and Structures
df$Journal <- str_replace(df$Journal, "^Internation Journals of Solids and Structures$", "International Journal of Solids and Structures")

# ITE Journal (Institute of Transportation Engineers)
pattern <- c("^Institute of Transportation Engineering Journal$", "^Institute of Transportation Engineers ITE Journal$", "^Institute of Transportation Engineers Journal$")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "ITE Journal \\(Institute of Transportation Engineers\\)")

# International Archives of the Photogrammetry Remote Sensing and Spatial Information Sciences  ISPRS Archives
df$Journal <- str_replace(df$Journal, "^International Archives of Photogrammetry and Remote Sensing$|^International Archives of Photogrammetry Remote Sensing and Spatial Information Sciences$", "International Archives of the Photogrammetry Remote Sensing and Spatial Information Sciences  ISPRS Archives")

# International Journal of Autonomous and Adaptive Communications Systems
df$Journal <- str_replace(df$Journal, "^International Journal of Autonomous and Adaptive Communications Systems \\(IJAACS$", "International Journal of Autonomous and Adaptive Communications Systems")

# International Journal of Environmental Research and Public Health
df$Journal <- str_replace(df$Journal, "^International Journal of Environmental Res Public Health$", "International Journal of Environmental Research and Public Health")

# International Journal of Health Geographics
df$Journal <- str_replace(df$Journal, "^International Journal of Health Geogr$|^International Journal of Health Geographic Associations Between Street Governments Office of Planning and Research$|^International Journal of Health Geography$", "International Journal of Health Geographics")

# International Journal of Industrial Engineering : Theory Applications and Practice
df$Journal <- str_replace(df$Journal, "^International Journal of Industrial Engineering$", "International Journal of Industrial Engineering \\: Theory Applications and Practice")

# International Journal of ITS
df$Journal <- str_replace(df$Journal, "^International Journal of ITS$|^International Journal of ITS Research$|^International Journal of of Intelligent Transportation Systems$", "International Journal of Intelligent Transportation Systems Research")

# International Journal of Life Cycle Assess
df$Journal <- str_replace(df$Journal, "^International Journal of Life Cycle Assess$", "International Journal of Life Cycle Assessment")

# International Journal of Logistics: Research and Applications
df$Journal <- str_replace(df$Journal, "^International Journal of Logistics\\: Research and Applications$", "International Journal of Logistics Research and Applications")

# International Journal of of Civil Engineering
df$Journal <- str_replace(df$Journal, "^International Journal of of Civil Engineering$", "International Journal of Civil Engineering")

# International Journal of Hydrogen Energy
df$Journal <- str_replace(df$Journal, "^International Journal of of Hydrogen Energy$", "International Journal of Hydrogen Energy")

# International Journal of of Steel Structures
df$Journal <- str_replace(df$Journal, "^International Journal of of Steel Structures$|^International Journal of Steel Structural$|^International Journal of Steel Structure$|^International Journal of Steel Structures KSSC$", "International Journal of Steel Structures")

# International Journal of Plant Science
df$Journal <- str_replace(df$Journal, "^International Journal of Plant Science$", "International Journal of Plant Sciences")

# International Journal of Remote Sens
df$Journal <- str_replace(df$Journal, "^International Journal of Remote Sens$|^International Journal of Remote Sensing  in Review$", "International Journal of Remote Sensing")

# International Journal of Rock Mechanics and Minings Sciences
df$Journal <- str_replace(df$Journal, "^International Journal of Rock Mechanical Min Science$|^International Journal of Rock Mechanical Mining Science$|^International Journal of Rock Mechanical Mining Science Geomech Abs$|^International Journal of Rock Mechanics and Mining Sciences and Geomechanics$", "International Journal of Rock Mechanics and Minings Sciences")

# International Journal of Sustain Transp
df$Journal <- str_replace(df$Journal, "^International Journal of Sustain Transp$", "International Journal of Sustainable Transportation")

# Intl
df$Journal <- str_replace(df$Journal, "Intl\\b", "International")

# ITE Journal (Institute of Transportation Engineers)
df$Journal <- str_replace(df$Journal, "^ITE Journal.*", "ITE Journal \\(Institute of Transportation Engineers\\)")

# Journal of Environmental and Engineering Geophysics
df$Journal <- str_replace(df$Journal, "^JEEG$|^JGeotechnical and Geoenvironmental Engineering$", "Journal of Environmental and Engineering Geophysics")

# Journal of Materials in Civil Engineering
df$Journal <- str_replace(df$Journal, "^Jorunal of Material in Civil Engineering$", "Journal of Materials in Civil Engineering")

# Journal
df$Journal <- str_replace(df$Journal, "Jour$", "Journal")
df$Journal <- str_replace(df$Journal, "Jour\\b", "Journal of")

# Wild Mgmt
df$Journal <- str_replace(df$Journal, "^Journal of Wild Mgmt$", "Journal of Wildlife Management")

# Journal of Engineering Mechanics  ASCE
df$Journal <- str_replace(df$Journal, "^Journal ASCE Engineering Mechanics Div$|^Theory Journal of Engineering MechanicsAsce$", "Journal of Engineering Mechanics  ASCE")

# Journal of the Acoustical Society of America
df$Journal <- str_replace(df$Journal, "^Journal of Acoust Society America$|^Journal of Acoustical Society of America$|^The Journal of the Acoustical Society of America$", "Journal of the Acoustical Society of America")

# Journal of Advanced Concrete Technology
df$Journal <- str_replace(df$Journal, "^Journal of Advances in Concr Tech$", "Journal of Advanced Concrete Technology")

# Journal of Agricultural Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Agricult Engineering Res$|^Journal of Agricultural Engineering Research$|^Journal of Agriculture Engng Res$", "Journal of Agricultural Engineering")

# JAMA  Journal of the American Medical Association
df$Journal <- str_replace(df$Journal, "^Journal of American Med Association$|^Journal of the American Medical Association$", "JAMA  Journal of the American Medical Association")

# JAMA  Journal of the American Medical Association
df$Journal <- str_replace(df$Journal, "^Journal of American Plan Association$|^Journal of American Planning Association$", "Journal of the American Planning Association")

# Journal of the American Society for Horticultural Science
df$Journal <- str_replace(df$Journal, "^Journal of American Society Hort Science$|^Journal of American Society Hortic Science$", "Journal of the American Society for Horticultural Science")

# App
df$Journal <- str_replace(df$Journal, "App\\b", "Applied")
# Electrochem
df$Journal <- str_replace(df$Journal, "Electrochem\\b", "Electrochemistry")

# Journal of Applied Mechanics Transactions ASME
df$Journal <- str_replace(df$Journal, "^Journal of Applied Mechanics$|^Journal of Applied Mechanics ASME$|^Journal of Applied MechanicsTransactions of the Asme$", "Journal of Applied Mechanics Transactions ASME")

# Journal of Applied Meteor
df$Journal <- str_replace(df$Journal, "^Journal of Applied Meteor.*", "Journal of Applied Meteorology and Climatology")

# Phys
df$Journal <- str_replace(df$Journal, "Phys\\b$", "Physics")
# Sens
df$Journal <- str_replace(df$Journal, "Sens\\b$", "Sensing")
# Archaeol
df$Journal <- str_replace(df$Journal, "Archaeol\\b$", "Archaeological")

# Journal of Arid Environment
df$Journal <- str_replace(df$Journal, "^Journal of Arid Environment.*", "Journal of Arid Environments")

# Journal of Atmospheric and Oceanic Technology
df$Journal <- str_replace(df$Journal, "^Journal of Atmospheric Oceanic Technol$", "Journal of Atmospheric and Oceanic Technology")

# Journal of Bridge Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Bridge Engineeing$|^Journal of Bridge Engineering ©$", "Journal of Bridge Engineering")

# Journal of Can Pet Technol
df$Journal <- str_replace(df$Journal, "^Journal of Can Pet Technol$", "Journal of Canadian Petroleum Technology")

# Journal of Central South University
df$Journal <- str_replace(df$Journal, "^Journal of Cent South Univ$", "Journal of Central South University")

# KSCE Journal of Civil Engineering 
df$Journal <- str_replace(df$Journal, "^Journal of Civil Engineering$", "KSCE Journal of Civil Engineering")

# Journal of Clean Prod
df$Journal <- str_replace(df$Journal, "^Journal of Clean Prod$", "Journal of Cleaner Production")

# Journal of Clim
df$Journal <- str_replace(df$Journal, "^Journal of Clim$", "Journal of Climate")

# Journal of Clinical Epidemihttp://Wwwnhtsadotgov/Portal/Site/Nhtsa/Menuitemdfedd570f698cabbbf Ology
df$Journal <- str_replace(df$Journal, "^Journal of Clinical Epid.*", "Journal of Clinical Epidemiology")

# Journal of Cold Regions Engineering  ASCE
df$Journal <- str_replace(df$Journal, "^Journal of Cold Regions Engineering$", "Journal of Cold Regions Engineering  ASCE")

# Journal of Combinatorial Theory Series B 
df$Journal <- str_replace(df$Journal, "^Journal of Combinatorial Theory B$", "Journal of Combinatorial Theory Series B")

# Journal of Comp Electro
df$Journal <- str_replace(df$Journal, "^Journal of Comp Electro$", "Journal of Computational Electronics")

# Journal of Compos Construction
df$Journal <- str_replace(df$Journal, "^Journal of Compos Construction$|^Journal of Composite for Construction$|^StateoftheArt Review Journal of Composites for Construction$", "Journal of Composites for Construction")

# Journal of Computers and Structures
df$Journal <- str_replace(df$Journal, "^Journal of Computers and Structures$", "Computers and Structures")

# Journal of Computing Civil Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Computing Civil Engineering$", "Journal of Computing in Civil Engineering")

# Journal of Computational and Graphical Statistics
df$Journal <- str_replace(df$Journal, "^Journal of Computational and Graphical Statistics$", "Journal of Computational and Graphical Statistics")

# Journal of Construction Engineering and Management  ASCE
df$Journal <- str_replace(df$Journal, "^Journal of Construction Engi$|^Journal of Construction Engineering and Management$|^Journal of Construction Engineering Manage$|^Journalof Construction Engineering and Management$", "Journal of Construction Engineering and Management  ASCE")

# Journal of Structural Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Constructional Steel$", "Journal of Structural Engineering")

# Journal of Contaminant Hydrology
df$Journal <- str_replace(df$Journal, "^Journal of Conta?m? Hydrology$", "Journal of Contaminant Hydrology")

# Journal of Dynamic Systems Measurement and Control Transactions of the ASME
df$Journal <- str_replace(df$Journal, "^Journal of Dynamics? Systems Measurement and Control$", "Journal of Dynamic Systems Measurement and Control Transactions of the ASME")

# Journal of Earthquake Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Earthquake$|^Journal of Earthquake Engineering Taylor and Francis UK$", "Journal of Earthquake Engineering")

# Econ
df$Journal <- str_replace(df$Journal, "Econ\\b", "Economic")

# Journal of Emerg Nurs
df$Journal <- str_replace(df$Journal, "^Journal of Emerg Nurs$", "Journal of Emergency Nursing")

# Journal of Engineering Gas Turbines Power
df$Journal <- str_replace(df$Journal, "^Journal of Engineering Gas Turbines Power$", "Journal of Engineering for Gas Turbines and Power")

# Journal of Engineering Mechanics  ASCE
df$Journal <- str_replace(df$Journal, "^Journal of Engineering Mechanical$|^Journal of Engineering Mechanics$|^Journal of Engineering MechanicsAsce$", "Journal of Engineering Mechanics  ASCE")

# Journal of Engineering and Technology Management  JETM
df$Journal <- str_replace(df$Journal, "^Journal of Engineering Tech Manage$", "Journal of Engineering and Technology Management  JETM")

# Journal of Environmental Engineering ASCE
df$Journal <- str_replace(df$Journal, "^Journal of Environmental Engineering$", "Journal of Environmental Engineering ASCE")

# Journal of Environmental and Engineering Geophysics
df$Journal <- str_replace(df$Journal, "^Journal of Environmental Engineering Geophysics$", "Journal of Environmental and Engineering Geophysics")

# Hort
df$Journal <- str_replace(df$Journal, "Hort\\b", "Horticulture")

#  Journal of Environmental Plann Manage
df$Journal <- str_replace(df$Journal, "^Journal of Environmental Plann Manage$", "Journal of Environmental Planning and Management")

# Journal of Epidemiology and Community Health
df$Journal <- str_replace(df$Journal, "^Journal of Epidemiology Commun$|^Journal of Epidemiology Community$", "Journal of Epidemiology and Community Health")

# Journal of Fluid Mechanical
df$Journal <- str_replace(df$Journal, "^Journal of Fluid Mechanical$", "Journal of Fluid Mechanics")

# Journal of Fluids Engineering Transactions of the ASME
df$Journal <- str_replace(df$Journal, "^Journal of Fluids Engineering$", "Journal of Fluids Engineering Transactions of the ASME")

# Journal of Forestry
df$Journal <- str_replace(df$Journal, "^Journal of Forestry.*", "Journal of Forestry")

# Journal of General Internal Medicine
df$Journal <- str_replace(df$Journal, "^Journal of Gen Intern Med$", "Journal of General Internal Medicine")

# Journal of Geophysical Resear
df$Journal <- str_replace(df$Journal, "^Journal of Geophysical Resear$|^Journal of Geophysical Research\\: Earth Surface$|^Journal of Geophysics Res$|^Journal of Geophysics Res: Solid Earth$", "Journal of General Internal Medicine")

# Journal of Geotechnical and Geoenvironmental Engineering  ASCE
df$Journal <- str_replace(df$Journal, "^Journal of Geotechnical and Geo Environmental Engineering Division$|^Journal of Geotechnical and Geo[Ee]nvironmental Engineering$|^Journal of Geotechnical and Geoenvironmental Engrg$|^Journal of Geotechnical Geoen Engineering$|^Journal of Geotechnical Geoenvironmental$|^Journal of Geotechnical Geoenvironmental Engineering$|^Journal of Geot Engineering Div$|^Journal of Geotechnical Engineering Div$|^Journal of Geotechnical Engrg\\s?D?i?v?$|^Journal of Geotechnical Geoen Engineering$|^Journal of Geotechnical Geoenvironmental$|^Journal of Geotechnical Geoenvironmental Engineering$|^Journal of Geotechnical and Geoenvironmental Engineering JOUR American Society of Civil Engineers$|^Journal of Geotechnical Engg$|^Journal of Geotechnical Geoenvironmental Engineering$", "Journal of Geotechnical and Geoenvironmental Engineering  ASCE")

# Journal of Geotechnical Engineering , I am calling the one abocve

# Journal of Health and Social Behavior
df$Journal <- str_replace(df$Journal, "^Journal of Health Society$", "Journal of Health and Social Behavior")

# Journal of Housing and the Built Environment
df$Journal <- str_replace(df$Journal, "^Journal of Housing Built Environment$", "Journal of Housing and the Built Environment")

# Journal of Housing Economic
df$Journal <- str_replace(df$Journal, "^Journal of Housing Economic$", "Journal of Housing Economics")

# Journal of Housing Economic
df$Journal <- str_replace(df$Journal, "^Journal of Hydraulic Research$", "Journal of Hydraulic Research\\/De Recherches Hydrauliques")

# Hydro
df$Journal <- str_replace(df$Journal, "Hydro\\b", "Hydrology")

# Journal of Hydrologic Engineering  ASCE
df$Journal <- str_replace(df$Journal, "^Journal of Hydrologic Engineering$", "Journal of Hydrologic Engineering  ASCE")

# Hydrometeorology
df$Journal <- str_replace(df$Journal, "Hydrometeor\\b", "Hydrometeorology")

# Journal of Industrial and Engineering Chemistry
df$Journal <- str_replace(df$Journal, "^Journal of Ind Engineering Chem$", "Journal of Industrial and Engineering Chemistry")

# Ind
df$Journal <- str_replace(df$Journal, "\\bInd\\b", "Industrial")

# Journal of Insect
df$Journal <- str_replace(df$Journal, "^Journal of Insect$", "Journal of Insect Conservation")

# Journal of Intell Transp Syst: Technol Planning and Operations
df$Journal <- str_replace(df$Journal, "^Journal of Intell Transp Syst\\: Technol Planning and Operations$|^Journal of Intelligent Transportation System$", "Journal of Intelligent Transportation Systems")

# Journal of Intelligent and Robotic Systems: Theory and Applications
df$Journal <- str_replace(df$Journal, "^Journal of Intell Transp Syst: Technol Planning and Operations$|^Journal of Intelligent and Robotic Systems$", "Journal of Intelligent and Robotic Systems\\: Theory and Applications")

# Journal of Irrigation and Drainage Engineering  ASCE
df$Journal <- str_replace(df$Journal, "^Journal of Irrig and Drain Engrg$|^Journal of Irrigation and Drainage Engineering$", "Journal of Irrigation and Drainage Engineering  ASCE")

# Journal of Mach Learn Res
df$Journal <- str_replace(df$Journal, "^Journal of Mach Learn Res$", "Journal of Machine Learning Research")

# Journal of Management in Engineering  ASCE
df$Journal <- str_replace(df$Journal, "^Journal of Management in Engineering$", "Journal of Management in Engineering  ASCE")

# Journal of Materials in Civil Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Materials Civil Engineering$|^Journal of Materials Civil Engrg$|^Journal of Materials in Civil Engrg$", "Journal of Materials in Civil Engineering")

# Mathematics and Computers in Simulation
df$Journal <- str_replace(df$Journal, "^Journal of Mathematics and Computers in Simulation$", "Mathematics and Computers in Simulation")

# Journal of Mechanical Design Transactions of the ASME
df$Journal <- str_replace(df$Journal, "^Journal of Mechanical Design$", "Journal of Mechanical Design Transactions of the ASME")

# Journal of Mechanical Engineering and Sciences
df$Journal <- str_replace(df$Journal, "^Journal of Mechanical Engineering Science$", "Journal of Mechanical Engineering and Sciences")

# Journal of Modern Transportation
df$Journal <- str_replace(df$Journal, "^Journal of Mod Transp$", "Journal of Modern Transportation")

# Journal of Modern Transportation
df$Journal <- str_replace(df$Journal, "^Journal of of", "Journal of")

# Journal of Multivariate Analysis
df$Journal <- str_replace(df$Journal, "^Journal of Multivar Anal$", "Journal of Multivariate Analysis")

# Journal of Petroleum Science and Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Pet Science Engineering$", "Journal of Petroleum Science and Engineering")

# Journal of Petroleum Technology -- not a journal but worth standarizing
df$Journal <- str_replace(df$Journal, "^Journal of Petr? Techn?o?l?$|^Journal of Pet Tech Transactions AIME$", "Journal of Petroleum Technology")

# Journal of Petroleum Science and Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Petrol Science Engineering$", "Journal of Petroleum Science and Engineering")

# Journal of Pharmaceutical Sciences
df$Journal <- str_replace(df$Journal, "^Journal of Pharm Science$", "Journal of Pharmaceutical Sciences")

# Phycol
df$Journal <- str_replace(df$Journal, "Phycol\\b", "Phycology")

# Journal of Phys Act Health
df$Journal <- str_replace(df$Journal, "^Journal of Phys Act Health$|^Journal of Physical Activity and Health$|^Journal of Physical Activity$", "Journal of Physical Activity & Health")

# Journal of Plan Educ
df$Journal <- str_replace(df$Journal, "^Journal of Plan Educ$|^Journal of Plan Educ Res$", "Journal of Planning Education and Research")

# Journal of Planning Literature
df$Journal <- str_replace(df$Journal, "^Journal of Plan Lit$", "Journal of Planning Literature")

# Journal of Policy Analysis and Managment
df$Journal <- str_replace(df$Journal, "^Journal of Policy Analysis and Managment$", "Journal of Policy Analysis and Management")

# Journal of Public Economic
df$Journal <- str_replace(df$Journal, "^Journal of Public Economic$", "Journal of Public Economics")

# Journal of Public Health Management and Practice : JPHMP
df$Journal <- str_replace(df$Journal, "^Journal of Public Health Management and Practice$", "Journal of Public Health Management and Practice \\: JPHMP")

# Journal of Public Transportation
df$Journal <- str_replace(df$Journal, "^Journal of Public Transp$|^Journal of Public Transportation Research$", "Journal of Public Transportation")

#Journal of the Royal Statistical Society Series a (Statistics in Society)
df$Journal <- str_replace(df$Journal, "^Journal of R Statist Society a$", "Journal of the Royal Statistical Society Series a \\(Statistics in Society\\)")

# Journal of R Stat Society Ser B
df$Journal <- str_replace(df$Journal, "^Journal of R Stat Society Ser B$|^Journal of the Royal Statistical Society Series B.*|^Journal of the Royal Statistical Society$", "Journal of the Royal Statistical Society Series B\\: Statistical Methodology")

# Journal of Real Estate Economics
df$Journal <- str_replace(df$Journal, "^Journal of Real Estate Economics$|^Journal of Real Estate Financ Economic$|^The Journal of Real Estate Finance and Economics$|^The Journal of Real Estate Finance Economics$", "Journal of Real Estate Finance and Economics")

# Journal of Real Estate Research
df$Journal <- str_replace(df$Journal, "^Journal of Real Estate Res$|^The Journal of Real Estate Research$", "Journal of Real Estate Research")

# Journal of Rock Mechanics and Geotechnical Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Rock Mechanical Geotechnical Engineering$", "Journal of Rock Mechanics and Geotechnical Engineering")

# Res
df$Journal <- str_replace(df$Journal, "Res\\b", "Research")

# Journal of Soils and Water Conservation
df$Journal <- str_replace(df$Journal, "^Journal of Soil and Water Conservation.*", "Journal of Soils and Water Conservation")

# Journal of the Soil Mechanics and Foundations Division -- not a journal in scimago but really popular
df$Journal <- str_replace(df$Journal, "^Journal of Soil$|^Journal of Soil Mechanical Fdns Div$|^Journal of Soil Mechanical Found$|^Journal of Soil Mechanical Found Div$|^Journal of Soil Mechanical Found Engin Div American Society Civil Engin$|^Journal of Soil Mechanical Nc Fdns Div$|^Journal of Soil Mechanics and Foundation.*|^Journal of the Soil Mechanics and Foundation$|^Journal of the Soil Mechanics and Foundations Division.*|^JSoil Mechanical and Foundations Div$", "Journal of the Soil Mechanics and Foundations Division")

# Journal of Sound and Vibration
df$Journal <- str_replace(df$Journal, "^Journal of Sound Vib$", "Journal of Sound and Vibration")

# Journal of Southeast University (English Edition)
df$Journal <- str_replace(df$Journal, "^Journal of Southeast University \\(English Edition$", "Journal of Southeast University \\(English Edition\\)")

# Journal of Strain Analysis for Engineering Design
df$Journal <- str_replace(df$Journal, "^Journal of Strain Analysis$", "Journal of Strain Analysis for Engineering Design")

# Journal of Structural and Construction Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Structural Construction Engineering$", "Journal of Structural and Construction Engineering")

# Journal of Structural Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Structural Div$|^Journal of Structural Engineering.*|^Journal of Structural Engrg$|^Journal of the Structural Engineering$|^Journal of the Structural Division Proceedings of the American Society of Civil Engineers$", "Journal of Structural Engineering")

# Journal of Studies on Alcohol and Drugs 
df$Journal <- str_replace(df$Journal, "^Journal of Studies Alcohol$", "Journal of Studies on Alcohol and Drugs")

# Journal of the American Concrete Institute
df$Journal <- str_replace(df$Journal, "^Journal of American Concrete Institute$|^Journal of the American Concrete Institute$|^Materials Journal$", "ACI Materials Journal")

# Journal of the American Water Resources Association
df$Journal <- str_replace(df$Journal, "^Journal of the American Water Resources Association \\(JAWRA
$", "Journal of the American Water Resources Association")

# Asphalt Paving Technology: Association of Asphalt Paving TechnologistsProceedings of the Technical Sessions
df$Journal <- str_replace(df$Journal, "^Journal of the Association of Asphalt Pavement Technologists
$|^Journal of the Association of Asphalt Paving Technologists$", "Asphalt Paving Technology\\: Association of Asphalt Paving TechnologistsProceedings of the Technical Sessions")

# Journal of the Chinese Institute of Engineers Transactions of the Chinese Institute of EngineersSeries a/Chungkuo Kung Ch'eng Hsuch K'an
df$Journal <- str_replace(df$Journal, "^Journal of the Chinese Institute of Engineers$", "Journal of the Chinese Institute of Engineers Transactions of the Chinese Institute of EngineersSeries a\\/Chungkuo Kung Ch\\'eng Hsuch K\\'an")

# Journal of the South African Institution of Civil Engineers
df$Journal <- str_replace(df$Journal, "^Journal of the South African Institution of Civil Engineers$", "Journal of the South African Institution of Civil Engineering")

# Transportation Research Record
df$Journal <- str_replace(df$Journal, "^Journal of the Transportation Research Board$|^Journal of the Transportation Research Forum$|^Journal of Transportation Research$|^Journal of Transportation Research Record$", "Transportation Research Record")

# Journal of Toxicology and Environmental Health
df$Journal <- str_replace(df$Journal, "^Journal of Toxicology and Environmental HealthPart aCurrent Issues$|^Journal of Toxicology and Environmental Health$", "Journal of Toxicology and Environmental Health  Part a")

# Trans
df$Journal <- str_replace(df$Journal, "\\bTransp?\\b", "Transportation")
# Geog
df$Journal <- str_replace(df$Journal, "\\bGeog\\b", "Geography")

# Transportation Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Transportation$|^Journal of Transportation Enginee1ing$|^Journal of Transportation Engineering American Society of Civil Engineers$|^Journal of Transportation EngineeringAsce$", "Journal of Transportation Engineering")

# Journal of Transport and Land Use
df$Journal <- str_replace(df$Journal, "^Journal of Transpotation L Use$|^Journal of Transport and Land Use \\(JTLU", "Journal of Transport and Land Use")

# Journal of Transp Stat -- this is a government joirnal
df$Journal <- str_replace(df$Journal, "^Journal of Transp Stat$", "Journal of Transportation and Statistics")

# Journal of Transport Economics Policy
df$Journal <- str_replace(df$Journal, "^Journal of Transport Economics Policy$", "Journal of Transport Economics and Policy")

# Journal of Transportation Geography
df$Journal <- str_replace(df$Journal, "^Journal of Transportation Geography$", "Journal of Transport Geography")

# Journal of Trauma and Acute Care Surgery
df$Journal <- str_replace(df$Journal, "^Journal of Trauma$", "Journal of Trauma and Acute Care Surgery")

# Journal of Urban Technology
df$Journal <- str_replace(df$Journal, "^Journal of Urban$", "Journal of Urban Technology")

# Journal of Urban Economic
df$Journal <- str_replace(df$Journal, "^Journal of Urban Economic.*", "Journal of Urban Economics")

# Journal of Urban Planning and
df$Journal <- str_replace(df$Journal, "^Journal of Urban Planning and$|^Journal of Urban Planning and DevelopmentAsce$", "Journal of Urban Planning and Development")

# Journal of Vibration and Acoustics Transactions of the ASME
df$Journal <- str_replace(df$Journal, "^Journal of Vibration and Acoustics.*", "Journal of Vibration and Acoustics Transactions of the ASME")

# Journal of Water Resources Planning and Management  ASCE
df$Journal <- str_replace(df$Journal, "^Journal of Water Resources Planning and ManagementAsce", "Journal of Water Resources Planning and Management  ASCE")

# Journal of Waterway Port Coastal and Ocean Engineering
df$Journal <- str_replace(df$Journal, "^Journal of Waterway Port Coastal and Ocean Engineering American Society of Civil Engineering$", "Journal of Waterway Port Coastal and Ocean Engineering")

# Journal of Wil
df$Journal <- str_replace(df$Journal, "^Journal of Wil.*|^JWidl Mgmt$", "Journal of Wildlife Management")

# Journal of Wind Engineering and Industrial Aerodynamics
df$Journal <- str_replace(df$Journal, "^Journal of Wind Engineering Industrial Aerodyn$", "Journal of Wind Engineering and Industrial Aerodynamics")

# Journal of Zoology (London
df$Journal <- str_replace(df$Journal, "^Journal of Zoology \\(London$", "Journal of Zoology")

# Land Economics
df$Journal <- str_replace(df$Journal, "^Land Economic$", "Land Economics")

# Landscape and Urban Planning
df$Journal <- str_replace(df$Journal, "^Landsc Urban Plan$|^Landscale and Urban Planning$|^Landscape and Planning$|^Landscaping and Urban Planning$", "Landscape and Urban Planning")

# Lect Notes in Control and Inf
df$Journal <- str_replace(df$Journal, "^Lect Notes in Control and Inf$", "Lecture Notes in Control and Information Sciences")

# New England Journal of Medicine
df$Journal <- str_replace(df$Journal, "^Life Expectancy the New England Journal of Medicine$|^NEJM
$", "New England Journal of Medicine")

# Lighting Design and Application: LD and a
df$Journal <- str_replace(df$Journal, "^Lighting Design and Application$", "Lighting Design and Application: LD and a")

# unclear why Lighting Research and Technology is not a match

# Limnol Oceanogr Methods
df$Journal <- str_replace(df$Journal, "^Limnol Oceanogr Methods$", "Limnology and Oceanography\\: Methods")

# Mag Concr Research
df$Journal <- str_replace(df$Journal, "^Mag Concr Research$", "Magazine of Concrete Research")

# Mar = Marine (I hope this doesn't mess up future March tags)
df$Journal <- str_replace(df$Journal, "Mar\\b", "Marine")

# Marine Ecology  Progress Series
df$Journal <- str_replace(df$Journal, "^Marine Ecology Prog Ser$|^Marine Ecology Progress Series$", "Marine Ecology  Progress Series")

# Marine and Petroleum Geology
df$Journal <- str_replace(df$Journal, "^Marine Pet Geology", "Marine and Petroleum Geology")

# Materials and Structures
df$Journal <- str_replace(df$Journal, "^Materials and Structures$|^Materials Structural$|^Materiaux et Constructions$", "Materials and Structures\\/Materiaux et Constructions")

# Materials Chemistry and Physics
df$Journal <- str_replace(df$Journal, "^Materials Chem Physics$", "Materials Chemistry and Physics")

# Materials and Corrosion  Werkstoffe Und Korrosion
df$Journal <- str_replace(df$Journal, "^Materials Corrosion$", "Materials and Corrosion  Werkstoffe Und Korrosion")

# Materials Evaluation
df$Journal <- str_replace(df$Journal, "^Materials Evaluation Journal American Society for Nondestructive Testing$", "Materials Evaluation")

# Materials Perform
df$Journal <- str_replace(df$Journal, "^Materials Perform$", "Materials Performance")

# Materials Phys Chem
df$Journal <- str_replace(df$Journal, "^Materials Phys Chem$", "Materials Chemistry and Physics")

# Materials Science and Engineering
df$Journal <- str_replace(df$Journal, "^Materials Science and Engineering$", "Materials Science & Engineering A\\: Structural Materials\\: Properties Microstructure and Processing")

# Mathematical Methods in the Applied Sciences
df$Journal <- str_replace(df$Journal, "^Math Meth Applied Science$", "Mathematical Methods in the Applied Sciences")

# Mathematical Programming Series B
df$Journal <- str_replace(df$Journal, "^Mathematical Programming$|^Mathematical Programming SerB$", "Mathematical Programming Series B")

# Measurement Science and Technology
df$Journal <- str_replace(df$Journal, "^Meas Science Technol$", "Measurement Science and Technology")

# Mechanical Systems and Signal Processing
df$Journal <- str_replace(df$Journal, "^Mechanical Syst Signal Process$", "Mechanical Systems and Signal Processing")

# Mechatronics IEEE/ASME Transactions on
df$Journal <- str_replace(df$Journal, "^Mechatronics IEEE/ASME Transactions on$", "Mechatronics")

# Medicine and Science in Sports and Exercise
df$Journal <- str_replace(df$Journal, "^Medicine and Science in Sports Exercise$", "Medicine and Science in Sports and Exercise")

# Metallurgical and Materials Transactions a
df$Journal <- str_replace(df$Journal, "^Metallurgical and Materials Transactions a$|^Metallurgical Transactions a$", "Metallurgical and Materials Transactions A\\: Physical Metallurgy and Materials Science")

# Mol
df$Journal <- str_replace(df$Journal, "Mol\\b", "Molecular")

# Molecular Biology Evol
df$Journal <- str_replace(df$Journal, "^Molecular Biology Evol$", "Molecular Biology and Evolution")

# Nat Clim Chang
df$Journal <- str_replace(df$Journal, "^Nat Clim Chang$", "Nature Climate Change")

#Nat Research
df$Journal <- str_replace(df$Journal, "^Nat Research$", "Natural Resources Forum")

# Natural Hazards and Earth System Sciences
df$Journal <- str_replace(df$Journal, "^Natural Hazards and Earth System Science$", "Natural Hazards and Earth System Sciences")

# Natural Resource Journal
df$Journal <- str_replace(df$Journal, "^Natural Resource Journal$", "Natural Resources Journal")

# NCHRP Synthesis
df$Journal <- str_replace(df$Journal, "^NCHRP.*", "National Cooperative Highway Research Program")

# Near Surf Geophysics
df$Journal <- str_replace(df$Journal, "^Near Surf Geophysics$", "Near Surface Geophysics")

# Networks and Heterogeneous Media
df$Journal <- str_replace(df$Journal, "^Netw Heterog Media$", "Networks and Heterogeneous Media")

# Netw Spat Economic
df$Journal <- str_replace(df$Journal, "^Netw Spat Economic$|^Network and Spatial Economics$", "Networks and Spatial Economics")

# Noise Health
df$Journal <- str_replace(df$Journal, "^Noise Health$", "Noise and Health")

# Nonlinear Analysis Theory Methods and Applications
df$Journal <- str_replace(df$Journal, "^Nonlinear Analysis$", "Nonlinear Analysis Theory Methods and Applications")

# Numerishe Mathematik
df$Journal <- str_replace(df$Journal, "^Numerishe Mathematik$", "Numerische Mathematik")

# Operation Research
df$Journal <- str_replace(df$Journal, "^Operation Research$|^Operations Reseaich$", "Operation Researchs")

# Not a journal but an org Pacific Coast Archaeological Society Quarterly
df$Journal <- str_replace(df$Journal, "^Pacific Coast Archaeological Society Quarterly$", "Pacific Coast Archaeological Society")

# Pacific Earthquake Engineering Research
df$Journal <- str_replace(df$Journal, "^Pacific Earthquake Engineering Research.*|^PEER.*", "Pacific Earthquake Engineering Research")
df$Journal <- ifelse(str_detect(df$Journal, "PEER"), "Pacific Earthquake Engineering Research", df$Journal)

# Packaging Technology and Science
df$Journal <- str_replace(df$Journal, "^Packaging and Technology Science$", "Packaging Technology and Science")

# Pervasive and Mobile Computing
df$Journal <- str_replace(df$Journal, "^Pervasive Computing$", "Pervasive and Mobile Computing")

# Petroleum Engineer International
df$Journal <- str_replace(df$Journal, "^Pet Engineering$", "Petroleum Engineer International")

# Philosophical Transactions of the Royal Society A: Mathematical Physical and Engineering Sciences
df$Journal <- str_replace(df$Journal, "^Philosophical Transactions of the Royal Society a$|^Phil Transportation R Society a$|^Philosophical Transactions of the Royal Society of London$|^Philosophical Transactions of the Royal Society of London Series [Aa]Mathematical Physical and Engineering Sciences$|^Philosophical Transactions: Mathematical Physical and Engineering Sciences$", "Philosophical Transactions of the Royal Society A\\: Mathematical Physical and Engineering Sciences")

# Philosophical Transactions of the Royal Society B: Biological Sciences
df$Journal <- str_replace(df$Journal, "^Philosophical Transactions of the Royal Society of London Series BBiological Sciences$", "Philosophical Transactions of the Royal Society B: Biological Sciences")

# Phys D
df$Journal <- str_replace(df$Journal, "^Phys D$", "Physica D: Nonlinear Phenomena")

# Physics of the Earth and Planetary Interiors
df$Journal <- str_replace(df$Journal, "^Phys Earth Planet International$", "Physics of the Earth and Planetary Interiors")

# Physical Review Letters
df$Journal <- str_replace(df$Journal, "^Phys Review of Lett$", "Physical Review Letters")

# Physics and Chemistry of the Earth
df$Journal <- str_replace(df$Journal, "^Physics and Chemistry of the Earth Parts.*$", "Physics and Chemistry of the Earth")

# PLoS Medicine
df$Journal <- str_replace(df$Journal, "^PLOS Med$", "PLoS Medicine")

# PLoS ONE
df$Journal <- str_replace(df$Journal, "^P[Ll][Oo][Ss] [Oo][Nn][Ee]$", "PLoS ONE")


#Prepared by sections -- moving these over to author or publisher
new.df <- data.table()
for (i in 1:nrow(df)){
  prep.by <- str_detect(df$Journal[i], "^Prepared by [a-z+]?")
  # Need to look up this anything followed by)
  Author <- ifelse(prep.by == T & is.na(df$Author[i]), 
                   str_extract(df$Journal[i], "(?<=[Pp]repared\\s[Bb]y\\st?h?e?).*"), 
                   df$Author[i])
  Publisher <- ifelse(prep.by == T & !is.na(df$Author[i]) & is.na(df$Publisher[i]), 
                      str_extract(df$Journal[i], "(?<=[Pp]repared\\s[Bb]y\\s[a-z+]?).*"), 
                      df$Publisher[i])
  Journal <- ifelse(prep.by == T, 
                    str_extract(df$Journal[i], "(?<=[Pp]repared\\s[Bb]y\\s[a-z+]?).*"), 
                    df$Journal[i])
  temp.df <- cbind(Author, Publisher, Journal, i)
  new.df <- rbind(temp.df, new.df)
}

df$i = row.names(df)

df2 <- left_join(new.df, df, by = "i")
df <- df2 %>% select(-Author.y, -Journal.y, -Publisher.y)
df <- df %>% rename("Author" = "Author.x", "Journal" = "Journal.x", "Publisher"= "Publisher.x")

# Prepared for all look like they have authors, so I should just remove the prepared for
df$Journal <- ifelse(str_detect(df$Journal, "^Prepared for t?h?e?"), str_remove(df$Journal, "^Prepared for t?h?e?\\s?"), df$Journal)

# Preventative Medicine
df$Journal <- str_replace(df$Journal, "^Preventative Medicine$", "Preventive Medicine")

# Probabilistic Engineering Mechanics
df$Journal <- str_replace(df$Journal, "^Probability Engineering Mechanics$", "Probabilistic Engineering Mechanics")

# Proc
df$Journal <- str_replace(df$Journal, "^Proc\\b", "Proceedings ")

scimago$Journal <- str_replace(scimago$Journal, "^Proc\\b", "Proceedings")

# Symp
df$Journal <- str_replace(df$Journal, "Symp\\b", "Symposium")

# Procedia  Social and Behavioral Sciences
df$Journal <- str_replace(df$Journal, "^Procedia‐Social and Behavioral Sciences$|^ProcediaSocial and Behavioral Sciences$", "Procedia  Social and Behavioral Sciences")

# Looking at the proceedings that are journals and matching those before throwing them into conference bucket later

# Proceedings of the National Academy of Sciences of the United States of America
df$Journal <- str_replace(df$Journal, "^PNAS.*|^Proceedings of the National Academy of Sciences$", "Proceedings of the National Academy of Sciences of the United States of America")

# Proceedings of the IEEE Special Issue on Vehicular Communications
df$Journal <- str_replace(df$Journal, "^Proceedings of the IEEE Special Issue on Vehicular Communications$|^Proceedings  IEEE$|^Proceedings  of the IEEE Journal", "Proceedings of the IEEE")

# Proceedings of the Royal Society A: Mathematical Physical and Engineering Sciences
# Proceedings  Royal Society
df$Journal <- str_replace(df$Journal, "^Proceedings  Royal Society$|^Proceedings  Royal Society of London Series a$|^Proceedings of the Royal Society of London Series a Mathematical and Physical Sciences$|^Royal Society: Math Phys Engineering Science$|^Ser a Math Phys Engineering Science$|^Series a Mathematical and Physical Sciences$", "Proceedings of the Royal Society A\\: Mathematical Physical and Engineering Sciences")

# Proceedings of the Royal Society B: Biological Sciences
df$Journal <- str_replace(df$Journal, "^Proceedings of the Royal Society Biological Sciences$", "Proceedings of the Royal Society B\\: Biological Sciences")

# Progress in Energy and Combustion Science
df$Journal <- str_replace(df$Journal, "^Prog Energy Combust Science$", "Progress in Energy and Combustion Science")

# Prog Structural Engng Materials
df$Journal <- str_replace(df$Journal, "^Prog Energy Combust Science$", "Progress in Energy and Combustion Science")

# Psychosomatic Medicine
df$Journal <- str_replace(df$Journal, "^Psychosom Med$", "Psychosomatic Medicine")

# Published by is shorter -- do tis and Report to and Submitted for
df$Journal <- ifelse(str_detect(df$Journal, "^Published by"), str_remove(df$Journal, "^Published by "), df$Journal)
df$Journal <- ifelse(str_detect(df$Journal, "^Published in"), str_remove(df$Journal, "^Published in "), df$Journal)

# Pure and Applied Geophysics
df$Journal <- str_replace(df$Journal, "^Pure Applied Geophysics$", "Pure and Applied Geophysics")

# Pure and Applied Mathematics Quarterly
df$Journal <- str_replace(df$Journal, "^Pure Applied Math$", "Pure and Applied Mathematics Quarterly")

# Q Journal of Engineering Geology
df$Journal <- str_replace(df$Journal, "^Q Journal of Engineering Geology$|^Quart Journal of Engineering Geology Hydrogeology$", "Quarterly Journal of Engineering Geology and Hydrogeology")

# RAND Journal of Economics
df$Journal <- str_replace(df$Journal, "^Rand Journal of Economics$|^The RAND Journal of Economics$", "RAND Journal of Economics")

# Real Estate Economic
df$Journal <- str_replace(df$Journal, "^Real Estate Economic$", "Real Estate Economics")

# Regional Science and Urban Economics
df$Journal <- str_replace(df$Journal, "^Reg Science Urban Economic$", "Regional Science and Urban Economics")

# Reg Studies
df$Journal <- str_replace(df$Journal, "^Reg Studies$", "Regional Studies")

# Renewable and Sustainable Energy Reviews
df$Journal <- str_replace(df$Journal, "^Renew Sustain Energy Review$|^Renewable and Sustainable Energy.*", "Renewable and Sustainable Energy Reviews")

# Report by, for and to should all just be removed
df$Journal <- ifelse(str_detect(df$Journal, "^Report by"), str_remove(df$Journal, "^Report by "), df$Journal)
df$Journal <- ifelse(str_detect(df$Journal, "^Report for"), str_remove(df$Journal, "^Report for "), df$Journal)
df$Journal <- ifelse(str_detect(df$Journal, "^Report to"), str_remove(df$Journal, "^Report to "), df$Journal)

# Road Materials and Pavement Design
df$Journal <- str_replace(df$Journal, "^Road Materials and Pavement Design \\(Online$", "Road Materials and Pavement Design")

# Rock Mechanics and Rock Engineering
df$Journal <- str_replace(df$Journal, "^Rock Mechanical Engineering Geology$|^Rock Mechanical Rock Engineering$", "Rock Mechanics and Rock Engineering")

# Sun: STUVW to Within the Transportation

# SAE Technical Paper
df$Journal <- str_replace(df$Journal, "^SAE.*", "SAE Technical Papers")

# Science Magazine
df$Journal <- str_replace(df$Journal, "^Science Magazine$", "Science")

# Environmental Science & Technology
df$Journal <- str_replace(df$Journal, "^Science Technol$", "Environmental Science & Technology")

# Science of the Total Environment
df$Journal <- str_replace(df$Journal, "^Science Total Environment$", "Science of the Total Environment")

# Seismological Research Letters
df$Journal <- str_replace(df$Journal, "^Seismol Research Lett$|^Seismological Research Let.*", "Seismological Research Letters")

# Shock and Vibration
df$Journal <- str_replace(df$Journal, "^Shock Vib$", "Shock and Vibration")

# SIAM Journal of Applied Math
df$Journal <- str_replace(df$Journal, "^SIAM Journal of Applied Math$", "SIAM Journal on Applied Mathematics")

# SIAM Journal of Control and Optimization
df$Journal <- str_replace(df$Journal, "^SIAM Journal of Control and Optimization$|^SIAM Journal of Control Optim$", "SIAM Journal on Control and Optimization")

# SIAM Journal of Matrix Anal Applied
df$Journal <- str_replace(df$Journal, "^SIAM Journal of Matrix Anal Applied$", "SIAM Journal on Matrix Analysis and Applications")

# SIAM Journal of Scientific Computing
df$Journal <- str_replace(df$Journal, "^SIAM Journal on Scientific Computing$", "SIAM Journal of Scientific Computing")

# Signal Processing IEEE Transactions on
df$Journal <- str_replace(df$Journal, "^Signal Processing IEEE Transactions on$|^Signal Processing Magazine IEEE$", "Signal Processing")

# Smart Materials and Structures
df$Journal <- str_replace(df$Journal, "^Smart Materials Structural$|^Smart Structures and Materials$", "Smart Materials and Structures")

# Society Science
df$Journal <- str_replace(df$Journal, "^Society Science$|^Society Science Med$", "Social Science and Medicine")

# Social Studies of Science
df$Journal <- str_replace(df$Journal, "^Society Studies Science$", "Social Studies of Science")

# Soils and Foundations
df$Journal <- str_replace(df$Journal, "^Soil and Foundations$", "Soils and Foundations")

# Soil Dynamics and Earthquake Engineering
df$Journal <- str_replace(df$Journal, "^Soil Dyn Earthquake Engineering$", "Soil Dynamics and Earthquake Engineering")

# Communications in Soil Science and Plant Analysis
df$Journal <- str_replace(df$Journal, "^Soil Science Plant Anal$", "Communications in Soil Science and Plant Analysis")

# Soil Science Society of America Journal
df$Journal <- str_replace(df$Journal, "^Soil Science Society American Journal.*$|^Soil Science Society of American Journal of$", "Soil Science Society of America Journal")

# Soil and Tillage Research
df$Journal <- str_replace(df$Journal, "^Soil Till Research$", "Soil and Tillage Research")

# Soils and Foundations
df$Journal <- str_replace(df$Journal, "^Soils and Foundations.*$|^Soils Found$", "Soils and Foundations")

# Spectra
df$Journal <- str_replace(df$Journal, "^Spectra$", "Earthquake Spectra")

# Standard and Poors PPP Credit Survey
df$Journal <- str_replace(df$Journal, "^Standard and Poors PPP Credit Survey$", "SandP")

# Stanford Law Review
df$Journal <- str_replace(df$Journal, "^Stanford Law Policy Review$", "Stanford Law Review")

# Statistics and Computing
df$Journal <- str_replace(df$Journal, "^Stat Computing$", "Statistics and Computing")

# Statistical Methods in Medical Research
df$Journal <- str_replace(df$Journal, "^Stat Methods Med Research$|^Statistical Methods in Medical$", "Statistical Methods in Medical Research")

# Structural Engineering/Earthquake Engineering
df$Journal <- str_replace(df$Journal, "^Structural and Earthquake Engineering Proc JSCE$", "Structural Engineering\\/Earthquake Engineering")

# Structural Concrete
df$Journal <- str_replace(df$Journal, "^Structural Concr Journal of FIB$", "Structural Concrete")

# Structural Control and Health Monitoring
df$Journal <- str_replace(df$Journal, "^Structural Control and Health Monitoring Inpress$|^Structural Control Heal Monit$|^Structural Heal Monit$", "Structural Control and Health Monitoring")

# Structural Design of Tall and Special Buildings
df$Journal <- str_replace(df$Journal, "^Structural Design of Tall Buildings$", "Structural Design of Tall and Special Buildings")

# Structural Dynamics @ 2000: Current Status and Future Directions Research Studies Press Ltd
df$Journal <- str_replace(df$Journal, "^Structural Dynamics @ 2000: Current Status and Future Directions Research Studies Press Ltd$", "Structural Dynamics")

# Structural Engineering
df$Journal <- str_replace(df$Journal, "^Structural Engineering$|^Structural Engineering International$", "Structural Engineering International: Journal of the International Association for Bridge and Structural Engineering (IABSE)")

# Structure and Infrastructure Engineering
df$Journal <- str_replace(df$Journal, "^Structural Infrastruct Engineering$", "Structure and Infrastructure Engineering")

# Studies Avian Biology -- I don't know why this is not being detected, I think it ended in 2008
df$Journal <- str_replace(df$Journal, "^Studies Avian Biology$", "Studies in Avian Biology")

# Submitted to 
df$Journal <- ifelse(str_detect(df$Journal, "^Submitted to"), str_remove(df$Journal, "^Submitted to "), df$Journal)

# Studies in Symbolic Interaction
df$Journal <- str_replace(df$Journal, "^Studies Symb Interact$", "Studies in Symbolic Interaction")

# Supply Chain Management
df$Journal <- str_replace(df$Journal, "^Supply Chain Management\\: An International Journal$", "Supply Chain Management")

# Technometrics
df$Journal <- str_replace(df$Journal, "^Technometric$", "Technometrics")

# American Economic Review
df$Journal <- str_replace(df$Journal, "^The American Economic Review$", "American Economic Review")

# American Journal of Clinical Nutrition
df$Journal <- str_replace(df$Journal, "^The American Journal of Clinical Nutrition$", "American Journal of Clinical Nutrition")

# American Naturalist
df$Journal <- str_replace(df$Journal, "^The American Naturalist$", "American Naturalist")

# the
df$Journal <- ifelse(str_detect(df$Journal, "^the"), str_remove(df$Journal, "^the "), df$Journal)

# Annals of Porb
df$Journal <- str_replace(df$Journal, "^The Annals of Probability$", "Annals of Probability")

# Annals of Regional Science
df$Journal <- str_replace(df$Journal, "^The Annals of Regional Science$", "Annals of Regional Science")

# Annals of Regional Statistics
df$Journal <- str_replace(df$Journal, "^The Annals of Statistics$", "Annals of Statistics")

# Auk
df$Journal <- str_replace(df$Journal, "^The Auk$", "Auk")

# International Journal of Life Cycle Assessment
df$Journal <- str_replace(df$Journal, "^The International Journal of Life Cycle Assessment$", "International Journal of Life Cycle Assessment")

# International Journal of Logistics Management
df$Journal <- str_replace(df$Journal, "^The International Journal of Logistics Management$", "International Journal of Logistics Management")

# Robotics Research
df$Journal <- str_replace(df$Journal, "^The International Journal of Robotics Research$", "International Journal of Robotics Research")

# Journal of Economic Inequality
df$Journal <- str_replace(df$Journal, "^The Journal of Economic Inequality$", "Journal of Economic Inequality")

# The Journal of Economic Perspectives
df$Journal <- str_replace(df$Journal, "^The Journal of Economic Perspectives$", "Journal of Economic Perspectives")

# The Journal of Industrial Economics
df$Journal <- str_replace(df$Journal, "^The Journal of Industrial Economics$", "Journal of Industrial Economics")

# The Journal of Machine Learning Research
df$Journal <- str_replace(df$Journal, "^The Journal of Machine Learning Research$", "Journal of Machine Learning Research")

# The Journal of Supercomputing
df$Journal <- str_replace(df$Journal, "^The Journal of Supercomputing$", "Journal of Supercomputing")

# The Quarterly Journal of Economics
df$Journal <- str_replace(df$Journal, "^The Quarterly Journal of Economics$", "Quarterly Journal of Economics")

# Quarterly Review of Economics and Finance
df$Journal <- str_replace(df$Journal, "^The Quarterly Review of Economics and Finance$", "Quarterly Review of Economics and Finance")

# The Senses and Society
df$Journal <- str_replace(df$Journal, "^The Senses and Society$", "Senses and Society")

# The Social Science Journal
df$Journal <- str_replace(df$Journal, "^ The Social Science Journal$", "Social Science Journal")

# Traffic Engineering and Control
df$Journal <- str_replace(df$Journal, "^Traffic Engineering Control$|^Traffic Engineeiing and Control$", "Traffic Engineering and Control")


# SKIPPED MORE

# Transportation Research Part A: Policy and Practice
df$Journal <- str_replace(df$Journal, "^Transport Research APol$|^Transport Research\\:? Part a$|^Transportation Research a$|^Transportation Research A: Policy and Practice$|^Transportation Research Part [Aa].*|^Transportation Research\\: Part a", "Transportation Research Part A\\: Policy and Practice")

# Transportation Research Part B: Methodological
pattern <- c("^Transp01tation Research Part B$", "^Transpmtation Research Pait B: Methodological$", "^Transpn Research$", "^Transporation Research B$", "Transportation Re Search Part B\\: Methodological$", "^Transportation Reseaich Part B.*$", "^Transportation Research B$", "^Transportation Research Pait B.*|^Transportation Research\\:? Part\\s?B.*")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "Transportation Research Part B\\: Methodological")

# Transportation Research Part C: Emerging Technologies
pattern <- c("^Transpo1tation Research Part C\\: Emerging Technologies$", "^Transport Research Part C: Emerging Technol$", "Transportation Reseaich Pait C\\: Emerging Technologies$", "^Transportation Research C$|^Transportation Research C Emerging Technol$|^Transportation Research C: Emerging Technologies$|^Transportation Research\\:? Part C.*|^Transportation ResearchPart C")
pattern <- paste(pattern, collapse = "|")
df$Journal <- str_replace(df$Journal, pattern, "Transportation Research Part C\\: Emerging Technologies")

# Transportation Research Part D: Transport and Environment -- I am including Transportation Research Part in here, even though not all are D, most seem to be
df$Journal <- str_replace(df$Journal, "^Transportation Research D.*$|^Transportation Research Part$|^Transportation Research\\:? Part D.*", "Transportation Research Part D\\: Transport and Environment")

# Transportation Research Part E: Logistics and Transportation Review
df$Journal <- str_replace(df$Journal, "^Transport Research ELog$|^Transportation Research  Part E$|^Transportation Resear\\:? Part E.*|^Transportation Research Part E$|^Transportation Research Part ELogistics and Transportation Review$", "Transportation Research Part E\\: Logistics and Transportation Review")

# Transportation Research Part F: Traffic Psychology and Behaviour
df$Journal <- str_replace(df$Journal, "^Transportat Research F\\: Traff Psychology Behavior$|^Transportation Research Part F.*|^Transportation Research\\: Part F$", "Transportation Research Part F\\: Traffic Psychology and Behaviour")

# Transportation Research Record
df$Journal <- str_replace(df$Journal, "^Transp01tation Research Record: Journal of the Transportation Research Board$|^Transpn Research Rec$|^Transport Research Rec$|^Transport Research Rec: Journal of Transportation Res Board$|^Transportation Reasearch Record: Journal of the Transportation Research Board$|^Transportation Research Rec$|^Transportation Research Rec\\:? Journal of Transp Res Board$|^TRANSPORTATION RESEARCH RECORD|^Transportation Research Repord.*|^Transptn Research Rec$|^Transrortation Research Record64and$|^TRR\\b|^Transportation Research Record.*", "Transportation Research Record")

# Transport Pol
df$Journal <- str_replace(df$Journal, "^Transport Pol$|^Transport Policy17\\(2\\):7284$|^Transportation Policy$|^Transportation Transport Policy$", "Transport Policy")

# Transport Science
df$Journal <- str_replace(df$Journal, "^Transport Science$", "Transportation Science")

# Transport Reviews
df$Journal <- str_replace(df$Journal, "^Transport Reviews\\: A Transnational Transdisciplinary Journal$", "Transport Reviews")

# Transportation (Amst
df$Journal <- str_replace(df$Journal, "^Transportation \\(Amst$", "Transportation")

# Transportmetrica A: Transport Science
df$Journal <- str_replace(df$Journal, "^Transportation A\\: Transp Science$", "Transportmetrica A\\: Transport Science")

#ASAE is the ASABE in Scimago!!!
# Transactions of the ASABE
df$Journal <- str_replace(df$Journal, "^Transportation ASAE$|^^Transportation of the ASAE$", "Transactions of the ASABE")

# Transportation Letters: The International Journal of Transportation Research
df$Journal <- str_replace(df$Journal, "^Transportation Letters\\: The International Journal of Transportation Research$", "Transportation Letters")

# Transportation LJournal
df$Journal <- str_replace(df$Journal, "^Transportation LJournal$", "Transportation Journal")

# Transport in Porous Media
df$Journal <- str_replace(df$Journal, "^Transportation Porous Media$", "Transport in Porous Media")

# On its own "Transportation Research" cannot be connected to one journal. The titles link to multiple of the A-F above and Research Record

# It looks like this Board runs into the research program, so I can assign this so it can be detected as an org later
df$Journal <- str_replace(df$Journal, "^Transportation Research Board.*", "National Cooperative Highway Research Program")

# Tunnelling and Underground Space Technology
df$Journal <- str_replace(df$Journal, "^Tunnel Undergr Space Tech$|^Tunneling and Underground Space Technology$", "Tunnelling and Underground Space Technology")

# URBAN STUDIES
df$Journal <- str_replace(df$Journal, "^URBAN STUDIES", "Urban Studies")

# World Applied Sciences Journal
df$Journal <- str_replace(df$Journal, "^World Applied Science Journal$", "World Applied Sciences Journal")

# World Dredging Mining and Constructions
df$Journal <- str_replace(df$Journal, "^World Dredging Mining and Construction$", "World Dredging Mining and Constructions")

# ZAMM Zeitschrift Für Angewandte Mathematik Und Mechanik
df$Journal <- str_replace(df$Journal, "^ZAMM‐Journal of Applied Mathematics and Mechanics/Zeitschrift Fuf Grund Der Plastizitätsbedingung Fü$", "ZAMM Zeitschrift Für Angewandte Mathematik Und Mechanik")



# I was a little sloppy in the I's. Lots to review there...
# Could not find journal of infastructure
# Need to maybe recheck National section
# Prepared by, prepared for: Take whatever is after and put in author column, if empty, and if not the publisher column, an dif not, then leave in journal column
# A lot of shit left in the The section but I didn't want to delte alle
## Also: "FINAL REPORT TO THE" or "FINAL REPORT PREPARED FOR THE"

# Check on Journals that have a lit of titles, but havent matched.Journal of urban planning and development
# Journal of Transportation and Statistics is in Scimago but not in this scimago list


# In combined cleaning: There is a long prepared for section... should I remove the prepared for and call it a jounral for now?




# 4. IDENTIFY NON-ACADEMIC JOURNAL SOURCES ----
## AGENCY LIST ----

# Get federal list later...
local.agency.abbr <- unique(toupper(str_extract(df$File, "(?<=documents\\/).*(?=\\/\\d)")))
local.agency.abbr <- local.agency.abbr[-1]
local.agency.abbr <- paste0("\\b", local.agency.abbr, "\\b")
local.agency.abbr.c <- paste(local.agency.abbr, collapse = "|")

other.agency <- read.csv("/Users/lizawood/Box/truckee/data/ca_fed_agencies_short.csv")
other.agency.name <- trimws(other.agency$agency)
#other.agency.name <- ifelse(str_detect(other.agency.name, "^US "), str_replace(other.agency.name, "^US ", "U?S?\\s?"), other.agency.name)
other.agency.name.c <- paste0(other.agency.name, collapse = "|")
other.agency.abbr <- trimws(other.agency$abbr)
other.agency.abbr <- other.agency.abbr[c(1:267)]
#other.agency.abbr <- ifelse(!str_detect(other.agency.abbr, "^US"), paste0("U?S?", other.agency.abbr) , other.agency.abbr)
other.agency.abbr <- paste0("\\b", other.agency.abbr, "\\b")
other.agency.abbr <- unique(other.agency.abbr)
other.agency.abbr.c <- paste0(other.agency.abbr, collapse = "|")


agency.misc <- c( #"^\\s*([a-zA-Z0-9:punct:]+\\s*){0,5}[Aa]genc[a-z]+\\s*([a-zA-Z0-9:punct:]+\\s*){0,5}$", 
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,2}Agricultural\\sCommissioner’s\\sOffice\\s*([a-zA-Z]+\\s*){0,2}$",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,3}Association\\s[Oo]f\\sGovernments$",
                  #"^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}Administration\\s*([a-zA-Z0-9:punct:]+\\s*){0,3}$",
                  #"^\\s*([a-zA-Z0-9:punct:]+\\s*){0,6}[Dd]epartment\\s*([a-zA-Z0-9:punct:]+\\s*){0,10}$",
                  #"^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Dd]ivision\\s*([a-zA-Z0-9:punct:]+\\s*){0,7}$",
                  #"^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Oo]ffice\\s*([a-zA-Z0-9:punct:]+\\s*){0,10}$",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,5}[Dd]ept[.]*\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", 
                  #"^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Bb]oard\\s*([a-zA-Z0-9:punct:]+\\s*){0,2}$",
                  #"^\\s*([a-zA-Z0-9:punct:]+\\s*){0,2}[Cc]alifornia\\s[Oo]ffice\\s*([a-zA-Z0-9:punct:]+\\s*){0,2}$",
                  "^California\\sAssociation\\sfor\\sCoordinated\\sTransportation$",
                  "^California\\sAir\\sPollution\\sControl\\sOfficers",
                  "^California\\s\\(State\\)\\sCoastal\\Conservancy",
                  "^Caltrans\\s[Dd]istrict\\s*([a-zA-Z0-9]+\\s*){0,3}$",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,4}\\,\\s[Cc]ity\\s\\of$",
                  #"^City\\sof\\s*([a-zA-Z0-9:punct:]+\\s*){1,4}$",
                  #"^\\s*([a-zA-Z0-9:punct:]+\\s*){0,5}[Bb]ureau\\s*([a-zA-Z]+\\s*){0,5}$",
                  #"^\\s*([a-zA-Z0-9:punct:]+\\s*){0,5}[Cc]ommission\\s*([a-zA-Z]+\\s*){0,3}$",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,4}$",
                  #"^[Cc]onservation\\s[Ss]ervice\\s*([a-zA-Z]+\\s*){0,2}$",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,3}Council\\s[Oo]f\\sGovernments\\s*([a-zA-Z0-9:punct:]+\\s*){0,3}$",
                  "^Journal of Transportation and Statistics$", # federal journal
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Mm]anagement\\s[Dd]istrict\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", 
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Mm]etropolitan\\s[Aa]uthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,2}$",
                  "^Monthly Energy Review$", # EIA Report
                  "^National\\sRenewable\\sEnergy\\sLaboratory\\s*([a-zA-Z0-9:punct:]+\\s*){0,2}$",
                  "^National Register Bulletin$",
                  #"^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Oo]ffice\\s[Oo]f\\s[Pp]lanning\\s[Aa]nd\\s[Rr]esearch\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", 
                  "^Planning Assistance and Standards 23", # This is FHWA but I'm not catching it now
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Rr]ail[road]*\\s[Aa]uthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
                  "^SFRWQCB\\s\\(San\\sFrancisco\\sBay\\sRegional",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}Flood\\sControl\\sDistrict$",
                  #"^State of California",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransit\\s[Aa]uthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransit\\s[Aa]dministration\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransportation\\s[Aa]uthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}Transportaiton\\sAuthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", 
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransportation\\s[Cc]ommission\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
                  #"^United\\sStates\\sGeological\\sService\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
                  #"^United\\sStates\\sSoil\\sConservation\\sService\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
                  #"^United States",
                  #"^US\\b",
                  #"^U S ",
                  #"^USCB",
                  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}Water\\sDistrict\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$")
agency.misc.c <- paste(agency.misc, collapse = "|")

agency.authors <- c(local.agency.abbr, other.agency.name, other.agency.abbr, agency.misc)
agency.authors.c <- paste(local.agency.abbr.c, other.agency.name.c, other.agency.abbr.c, agency.misc.c, sep = "|")


df$agency_journal <- ifelse(grepl(agency.authors.c, df$Journal, ignore.case = T), T, F) 
df$agency_author <- ifelse(grepl(agency.authors.c, df$Author, ignore.case = T), T, F)
df$agency_pub <- ifelse(grepl(agency.authors.c, df$Publisher, ignore.case = T), T, F)

## DEAL WITH NAs
df$agency_journal <- ifelse(is.na(df$agency_journal), F, df$agency_journal)
df$agency_author <- ifelse(is.na(df$agency_author), F, df$agency_author)
df$agency_pub <- ifelse(is.na(df$agency_pub), F, df$agency_pub)

table(df$agency_journal)
table(df$agency_author)
table(df$agency_pub)

# Create a single column to identify -- yes, this is an agency
df$agency_citation <- ifelse(df$agency_author == T | df$agency_journal == T | df$agency_pub == T, T, F)
table(df$agency_citation)


## ORGANIZATION LIST ----

# Include those journals that are not listed
org.authors <- c("^ACI$",
                 "AHMCT",
                 "^American Planning Association$",
                 "^([:graph:]+\\s*){0,3}Associates+\\s*([:graph:]+\\s*){0,3}$",
                 "^Center\\sfor\\sSustainable\\sEnergy$",
                 "^Center\\sfor\\sBiological\\sDiversity\\sand\\sCenter\\sfor\\sFood\\sSafety", 
                 "^California\\sClimate\\sChange\\sCenter", 
                 "^\\s*([a-zA-Z]+\\s*){0,3}Center+\\s*([a-zA-Z]+\\s*){0,3}$",
                 "^California's\\sWildlife",
                 "^Center\\sfor\\sUrban\\sTransportation\\sResearch$",
                 "^Concrete International$",
                 "^([a-zA-Z]+\\s*){0,4}Consortium\\s*([a-zA-Z]+\\s*){0,4}$",
                 "^ECONorthwest$",
                 "^Fitch$", # a credit company?
                 "^Intergovernmental Panel on Climate Change",
                 "^IPCC",
                 "^Journal of Commerce", # A website
                 "^\\s*([a-zA-Z]+\\s*){0,2}Friends\\sof\\sthe\\sLos\\sAngeles\\sRiver\\s*([a-zA-Z]+\\s*){0,2}$",
                 "^MATES", # an AQMD study 
                 "Mayo Clinic|Mayoclinic",
                 "^McKinsey and Company$", # Some kind of newsletter
                 "^\\s*([a-zA-Z]+\\s*){0,4}Museum\\s*([a-zA-Z]+\\s*){0,4}$",
                 "^National\\sAssociation\\sof\\sCity\\sTransportation\\sOfficials",
                 "^National Center for Earthquake Engineering Research$",
                 "^National Earthquake Hazards Reduction Program",
                 "^National Cooperative [a-zA-Z+] Research Program",
                 "^National Research Council",
                 "^Natural Resources Defense Council",
                 "^Nature Conservancy Council",
                 "Pew Research Center",
                 "Pacific Coast Archaeological Society",
                 "Pacific Earthquake Engineering Research", # A Berkeley Group
                 "PEER", # The same Berkeley Group
                 "Rasmussen Reports", # Some cpolling company
                 "SandP", # a group
                 "^San\\sDiego\\sGas\\sand\\sElectric$",
                 "^Santa\\sMonica\\sMountains\\sConservancy",
                 "Seismological Society America",
                 "^Southern\\sCalifornia\\sEarthquake\\sData\\sCenter",
                 "^Southern\\sCalifornia\\sGas\\sCompany",
                 "The\\sNature\\sConservancy", 
                 "^Transit\\sCooperative\\sResearch\\sProgram", 
                 "Transportation Sustainability Research Center",
                 "Transportation Research Laboratories United Kingdom",
                 "UCLA", 
                 "UCDAVIS",
                 "UC Davis",
                 "UC Berkeley",
                 "^University",
                 "^\\s*([a-zA-Z]+\\s*){0,3}University\\s*([a-zA-Z]+\\s*){0,3}$",
                 "World Bank",
                 "World Health Organization")
org.authors <- paste(org.authors, collapse = "|")

df$org_journal <- ifelse(grepl(org.authors, df$Journal), T, F)
df$org_author <- ifelse(grepl(org.authors, df$Author), T, F)
df$org_pub <- ifelse(grepl(org.authors, df$Publisher), T, F)

df$org_journal <- ifelse(is.na(df$org_journal), F, df$org_journal)
df$org_author <- ifelse(is.na(df$org_author), F, df$org_author)
df$org_pub <- ifelse(is.na(df$org_pub), F, df$org_pub)

table(df$org_journal)
table(df$org_pub)
table(df$org_author)

df$org_citation <- ifelse(df$org_author == T | df$org_journal == T | df$org_pub == T, T, F)


## MEDIA LIST ----

# https://www.agilitypr.com/resources/top-media-outlets/top-10-california-daily-newspapers-by-circulation/

# Include those journals that are not listed
media.authors <- c("ACCESS Magazine",
                   "Baltimore Sun",
                   "Brentwood News",	
                   "Business Insider",	
                   "Contra Costa Times",	
                   "Chicago Magazine",
                   "Chicago Sun-Times",
                   "Chicago Tribune",
                   "Chino Valley News",
                   "CNN",
                   "Daily Herald",
                   "Dallas Morning News",
                   "Dezeen Magazine",
                   "Fortune Magazine",
                   "Huffington Post",
                   "International Herald Tribune",
                   "KABC Eyewitness News",
                   "Kansas City Star",
                   "Knoxville Business Journal",
                   "Lancet",
                   "Los Angeles Times",
                   "La Habra Star",
                   "Ledger Dispatch",
                   "Long Beach PressTelegram.*$",
                   "Los Angeles Daily News",
                   "Metro News and Reviews",
                   "Metro Magazine",
                   "Midland ReporterTelegram",
                   "Momentum Mag (Blog) April",
                   "NBC\\s?News",
                   "New Times \\(San Luis Obispo",
                   "New York Magazine",
                   "New York Times",
                   "NLTimes",
                   "NPR",
                   "Oakland Post",
                   "Oakland Tribune",
                   "Parking Today",
                   "Planning Magazine",
                   "Pittsburgh PostGazette",
                   "Reuters",
                   "San Francisco Chronicle",	
                   "San Francisco Examiner",
                   "SF Examiner",
                   "San Diego Union",
                   "San Jose Mercury News",
                   "San Luis Obispo Tribune",
                   "Santa Fe Employees Magazine",
                   "Seattle Times",
                   "The Santa Fe Magazine",
                   "Santa Ynez Valley News February",
                   "Sacramento Business Journal",	
                   "Sacramento Bee",
                   "San Francisco Business Times",
                   "Sentinel Weekly News",
                   "South Florida Sun Sentinel",
                   "^South Ontario News",
                   "Streetsblog SF",
                   "Sun Sentinel",
                   "Sun Telegraph Newspaper",
                   "The Argus",
                   "The Atlantic Monthly",
                   "The Daily News of Los Angeles",
                   "The Daily Transcript",
                   "The Desert Sun",
                   "The Diablo Aviator",
                   "The Dispatch",
                   "The Economist",
                   "The El Dorado Hills Telegraph",
                   "The Eureka Reporter",
                   "The Grunion Gazette",
                   "The Guardian",
                   "The Journal of Commerce",
                   "The Lompoc Record",
                   "The Mercury News",
                   "The Morgan Hill Times",
                   "The Oakland Tribune",
                   "The Pinnacle News",
                   "The Salt Lake Tribute",
                   "The San Bernardino Sun",
                   "The Sun",
                   "The Tribune",
                   "The TriValley Herald",
                   "The Valley Sentinel",
                   "The Washington Post",
                   "TIME",
                   "USA Today",
                   "Valley Times",
                   "Via Magazine",
                   "Washington Post",
                   "Wall Street Journal")

media.authors <- paste(media.authors, collapse = "|")

df$media_journal <- ifelse(grepl(media.authors, df$Journal), T, F)
df$media_author <- ifelse(grepl(media.authors, df$Author), T, F)
df$media_pub <- ifelse(grepl(media.authors, df$Publisher), T, F)

df$media_journal <- ifelse(is.na(df$media_journal), F, df$media_journal)
df$media_author <- ifelse(is.na(df$media_author), F, df$media_author)
df$media_pub <- ifelse(is.na(df$media_pub), F, df$media_pub)

table(df$media_journal)
table(df$media_pub)
table(df$media_author)

df$media_citation <- ifelse(df$media_author == T | df$media_journal == T | df$media_pub == T, T, F)


## CONFERENCE WORDS -----
# Conference,Symposium -- all conference references
conf.words <- c("[Cc]onference", "[Ss]ymposium", "[Ss]ummit", "^Presented at", "^Presentation at", "^Presentation Delivered at", "Proceedings", "Annual Meeting", "Congress [Oo]n")
conf.words <- paste(conf.words, collapse = "|")

df$conf_journal <- ifelse(grepl(conf.words, df$Journal), T, F)
df$conf_pub <- ifelse(grepl(conf.words, df$Publisher), T, F)

df$conf_journal <- ifelse(is.na(df$conf_journal), F, df$conf_journal)
df$conf_pub <- ifelse(is.na(df$conf_pub), F, df$conf_pub)

table(df$conf_journal)
table(df$conf_pub)

df$conf_citation <- ifelse(df$conf_journal == T | df$conf_pub == T, T, F)



## POLICY WORDS??
# Code,  Assembly Bill, Policy

# 5. EXACT MATCHING CLEANED ANYSTYLE TO SCIMAGO ----
df$Journal <- trimws(df$Journal)
df$journal_match <- df$Journal %in% scimago$Journal # could also see journals
table(df$journal_match) # 5684, and bumping this up little by little to 5997 and now 6476, then 7286 then 7527 and now 8148 and 8240 and 8418 and 8547 and 9053
df$pub_match <- df$Publisher %in% scimago$Journal # could also see journals
table(df$pub_match) # 31 now 32
df$Journal <- ifelse(df$journal_match == F & df$pub_match == T, df$Publisher, df$Journal)
df$journalpub_match <- ifelse(df$journal_match == T | df$pub_match == T, T, F)



# 6. SUMMARY: AGENCY VS JOURNAL SOURCES? ----

journalmatch.df <- df %>% filter(journalpub_match == T)
orgmatch.df<- df %>% filter(agency_citation == T | org_citation == T | media_citation == T | conf_citation == T & journalpub_match == F)

nrow(journalmatch.df)/nrow(df)
# Overall we can match only 15% of all potential citations to journals
nrow(orgmatch.df)/nrow(df)
# Under this new scenario it is only 19%, so lost 6%. I am not sure if this is good or not, yet
# (so, 34% from whole list)

journal.df <- df %>% filter(!is.na(Journal) & agency_citation == F & org_citation == F & media_citation == F & conf_citation == F)
nrow(journalmatch.df)/nrow(journal.df) 
# Of the citations that have test in the journal column and are not agencies or orgs, what is our percentage that we can match?
#Started at 20% , now 25% of journal citations we can match, and now we are a 56%  now 58%


# Not matches
nomatch.df<- journal.df %>% filter(journal_match == F)


# 7. EXACT MATCHING TO AGENCIES ----

## MATCHING AGENCY NAMES ----
# Make a df with just the agency citations
agencymatch.df <- filter(orgmatch.df, agency_citation == T)
agencymatch.df <- unique(agencymatch.df)

# Give row numbers specific to the agency citations
agencymatch.df$row <- 1:nrow(agencymatch.df)

# Findings the location in each column where the match this, then assigning that location to the match.name column
exact.df <- data.table()
for (i in 1:length(agency.authors)){
  # Finds the location of where each line in the agency author matches in the list of agencies
  author.match <- data.table(grep(agency.authors[i], str_remove_all(agencymatch.df$Author, "\\.")))
  colnames(author.match) <- "row"
  # Assigns the names to those locations
  author.match$match.name <- agency.authors[i]
  #author.match$agency.row <- i
  exact.df <- rbind(author.match, exact.df, fill=T)
}

# Join the matches with the agency match
agencymatch.df.exact <- left_join(agencymatch.df, exact.df, by = "row")

# Rename the column
agencymatch.df.exact <- agencymatch.df.exact %>% rename("match.name.Author" = "match.name")

table(is.na(agencymatch.df.exact$match.name)) # 2638 agency Author matches (FALSE)

exact.df <- data.table()
for (i in 1:length(agency.authors)){
  author.match <- data.table(grep(agency.authors[i], agencymatch.df$Journal))
  colnames(author.match) <- "row"
  author.match$match.name <- agency.authors[i]
  #author.match$agency.row <- i
  exact.df <- rbind(author.match, exact.df, fill=T)
}

table(is.na(exact.df$match.name)) # 2459 Journal matches
agencymatch.df.exact2 <- left_join(agencymatch.df.exact, exact.df, by = "row")

# Rename the column
agencymatch.df.exact2 <- agencymatch.df.exact2 %>% rename("match.name.Journal" = "match.name")

table(is.na(agencymatch.df.exact2$match.name.Author), is.na(agencymatch.df.exact2$match.name.Journal))
# 474 Overlap

exact.df <- data.table()
for (i in 1:length(agency.authors)){
  author.match <- data.table(grep(agency.authors[i], agencymatch.df$Publisher))
  colnames(author.match) <- "row"
  author.match$match.name <- agency.authors[i]
  #author.match$agency.row <- i
  exact.df <- rbind(author.match, exact.df, fill=T)
}

table(is.na(exact.df$match.name)) # 3203 Publisher matches
agencymatch.df.exact3 <- left_join(agencymatch.df.exact2, exact.df, by = "row")

# Rename the column
agencymatch.df.exact3 <- agencymatch.df.exact3 %>% rename("match.name.Publisher" = "match.name")

agencymatch.df.exact3$match.name.Author <- str_remove_all(agencymatch.df.exact3$match.name.Author, "\\\\b|s\\?|\\?")
agencymatch.df.exact3$match.name.Journal <- str_remove_all(agencymatch.df.exact3$match.name.Journal, "\\\\b|s\\?|\\?")
agencymatch.df.exact3$match.name.Publisher <- str_remove_all(agencymatch.df.exact3$match.name.Publisher, "\\\\b|s\\?|\\?")

table(is.na(agencymatch.df.exact3$match.name.Author), is.na(agencymatch.df.exact3$match.name.Journal), is.na(agencymatch.df.exact3$match.name.Publisher))
# With all 3 overlap: 3
# With only 2 overlap: 471

# COLLAPSE THE WIDE REPEATS (prioritize author) ----

exact.agency.match <- agencymatch.df.exact3 %>% 
  filter(!is.na(match.name.Author) | !is.na(match.name.Journal) | !is.na(match.name.Publisher))

# Choose one column that gets priority for merging
exact.agency.match$match.name <- ifelse(!is.na(exact.agency.match$match.name.Author) & 
                                          is.na(exact.agency.match$match.name.Journal) &
                                          is.na(exact.agency.match$match.name.Publisher), 
                                        exact.agency.match$match.name.Author,
                                 ifelse(is.na(exact.agency.match$match.name.Author) & 
                                          !is.na(exact.agency.match$match.name.Journal) &
                                          is.na(exact.agency.match$match.name.Publisher), 
                                        exact.agency.match$match.name.Journal,
                                 ifelse(is.na(exact.agency.match$match.name.Author) & 
                                          is.na(exact.agency.match$match.name.Journal) &
                                          !is.na(exact.agency.match$match.name.Publisher), 
                                        exact.agency.match$match.name.Publisher,
                                ifelse(is.na(exact.agency.match$match.name.Author) & 
                                         !is.na(exact.agency.match$match.name.Journal) &
                                         !is.na(exact.agency.match$match.name.Publisher),
                                        exact.agency.match$match.name.Journal,
                                        exact.agency.match$match.name.Author))))

exact.agency.match <- select(exact.agency.match, -c(match.name.Author:match.name.Publisher))
exact.agency.match.u <- unique(exact.agency.match) #7529
table(is.na(exact.agency.match.u$match.name))

# COLLAPSE THE LONG REPEATS (prioritize "lowest" unit) ----
# How many have redundant rows
mult.rows <- exact.agency.match.u %>% group_by(row) %>% 
  count() %>% 
  filter(n>1) %>% 
  arrange(desc(n))
# 885 repeated value
table(mult.rows$n)

doubles <- exact.agency.match.u %>% group_by(row) %>% count() %>% filter(n > 1) %>% select(-n)
doubles.df <- exact.agency.match.u %>% filter(row %in% doubles$row) #1902 rows for 899 repeats
nrow(doubles.df) - nrow(doubles) # 937 rows I need to get rid of

# Using functions already read in to select just one of these, and then clean them up to re-run

make.duplicate.for2(exact.agency.match.u, "FHWA", "Federal Highway Administration", "Federal Highway Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "National Highway Traffic Safety Administration", "Department of Transportation", "National Highway Traffic Safety Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "NHTSA", "Department of Transportation", "National Highway Traffic Safety Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "FHWA", "Department of Transportation", "Federal Highway Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "NIST", "National Institute of Standards and Technology", "National Institute of Standards and Technology")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "OSHA", "CDC", "Centers for Disease Control and Prevention")
clean.for.next.input()


make.duplicate.for2(exact.agency.match.u, "SMGB", "State Mining and Geology Board", "State Mining and Geology Board")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "DOT", "Department of Transportation", "Department of Transportation")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "University of California", "California State University", "University of California")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "DOC", "Department of Conservation", "Department of Conservation")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Federal Highway Administration", "Department of Transportation", "Federal Highway Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Federal Transit Administration", "Federal Highway Administration", "Federal Transit Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Federal Transit Administration", "Department of Transportation", "Federal Transit Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "FTA", "Federal Transit Administration", "Federal Transit Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "FHWA", "DOT", "Federal Highway Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "FHA", "Federal Highway Administration", "Federal Highway Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "USDA", "Forest Service", "Forest Service")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "DOF", "Department of Finance", "Department of Finance")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "CEC", "California Energy Commission", "California Energy Commission")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "NPS", "National Park Service", "National Park Service")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "EPA", "Environmental Protection Agency", "Environmental Protection Agency")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "BLM", "Forest Service", "Bureau of Land Management")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "HUD", "Department of Housing and Urban Development", "Department of Housing and Urban Development")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u,  "HUD", "HCD", "Department of Housing and Urban Development")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u,  "Natural Resources Conservation Service", "Department of Agriculture", "Natural Resources Conservation Service")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Forest Service", "Department of Agriculture", "Forest Service")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "National Park Service", "Department of the Interior", "National Park Service")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Department of the Interior", "Bureau of Land Management", "Bureau of Land Management")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u,  "USGS", "National Park Service", "National Park Service")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u,  "NOAA", "National Oceanic and Atmospheric Administration", "National Oceanic and Atmospheric Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u,  "NASA", "National Aeronautics and Space Administration", "National Aeronautics and Space Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Federal Aviation Administration", "Department of Transportation", "Federal Aviation Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Department of Transportation", "California Department of Transportation", "California Department of Transportation")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Environmental Protection Agency", "California Environmental Protection Agency", "California Environmental Protection Agency")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Fish and Wildlife Service", "Department of Fish and Wildlife", "Department of Fish and Wildlife")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Agricultural Research Service", "Department of Agriculture", "Department of Agriculture")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Federal Motor Carrier Safety Administration", "Department of Transportation", "Department of Transportation")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Senate", "Department of Transportation", "Department of Transportation")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Department of Education", "California Department of Education", "California Department of Education")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "CDFW", "Fish and Wildlife Service", "Department of Fish and Wildlife")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "OPR", "Office of Planning and Research", "Office of Planning and Research")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "USGS", "Geological Survey", "Geological Survey")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Geological Survey", "Department of the Interior", "Geological Survey")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Bureau of Transportation Statistics", "Department of Transportation", "Department of Transportation")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "FRA", "Federal Railroad Administration", "Federal Railroad Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "CTC", "California Transportation Commission", "California Transportation Commission")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "ARB", "Air Resources Board", "Air Resources Board")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "DMV", "Department of Motor Vehicles", "Department of Motor Vehicles")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "UC", "University of California", "University of California")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Delta", "Delta Stewardship Council", "Delta Stewardship Council")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, '^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransportation\\s[Cc]ommission\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$', "California Transportation Commission", "California Transportation Commission")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,4}$", "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,3}Association\\s[Oo]f\\sGovernments$", "County Association of Governments")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransportation\\s[Cc]ommission\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,4}$", "County Transportation Commission")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransportation\\s[Aa]uthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,4}$", "County Transportation Authority")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransportation\\s[Aa]dministration\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,4}$", "County Transportation Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransit\\s[Aa]dministration\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,4}$", "County Transit Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}Water\\sDistrict\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,4}$", "County Water District")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}Flood\\sControl\\sDistrict$", "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,4}$", "County Flood Control District")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransit\\s[Aa]dministration\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", "Federal Transit Administration", "Federal Transit Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "NSF", "FHWA", "Federal Highway Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "FWS", "Fish and Wildlife Service", "Fish and Wildlife Service")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "National Science Foundation", "Federal Highway Administration", "Federal Highway Administration")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "FHWA", "Federal Highway Administration", "Department of Transportation", "Federal Highway Administration")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "University of California", "Department of Transportation", "California Department of Transportation", "University California")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "UC", "Department of Transportation", "California Department of Transportation", "University California")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "Environmental Protection Agency", "California Environmental Protection Agency", "Air Resources Board", "Air Resources Board")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "USDA", "Fish and Wildlife Service", "Environmental Protection Agency",  "Fish and Wildlife Service")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransit\\s[Aa]dministration\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", "Federal Transit Administration", "Department of Transportation", "Federal Transit Administration")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "NOAA", "NOAA Fisheries", "National Marine Fisheries Service", "National Oceanic and Atmospheric Administration")
clean.for.next.input()


mult.rows <- exact.agency.match.u %>% group_by(row) %>% 
  count() %>% 
  filter(n>1) %>% 
  arrange(desc(n))
# Down to 58 repeated value
table(mult.rows$n)


# CHOOSING PRIORITY ---------

make.duplicate.for2(exact.agency.match.u, "Census Bureau", "Environmental Protection Agency", "Environmental Protection Agency")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Army Corps of Engineers", "Geological Survey", "Army Corps of Engineers")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Census Bureau", "Department of Commerce", "Census Bureau")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "National Oceanic and Atmospheric Administration", "Department of Commerce", "National Oceanic and Atmospheric Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "SANDAG", "SACOG", "Association of Governments")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "SCAG", "SANDAG", "Association of Governments")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "BCDC", "MTC", "MTC")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Energy Information Administration", "Department of Energy", "Energy Information Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Department of Housing and Urban Development", "Department of Transportation", "Department of Housing and Urban Development")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "FEMA", "CEC", "California Energy Commission")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Federal Railroad Administration", "Department of the Interior", "Federal Railroad Administration")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Federal Register", "Department of the Interior", "Department of the Interior")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Department of Conservation","State Department", "Department of Conservation")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Department of Energy", "Bureau of Land Management", "Department of Energy")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "CCC", "OCTA", "OCTA")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,5}[Dd]ept[.]*\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", "Geological Survey", "Geological Survey")
clean.for.next.input() 

make.duplicate.for2(exact.agency.match.u, "Administration for Community Living", "Department of Health and Human Services", "Administration for Community Living")
clean.for.next.input()

make.duplicate.for2(exact.agency.match.u, "Department of Agriculture", "Department of the Interior", "Department of Agriculture")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "EPA", "Energy Information Administration", "Environmental Protection Agency", "Environmental Protection Agency")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "EPA", "Environmental Protection Agency", "Department of Transportation",  "Environmental Protection Agency")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "FTA", "EPA", "Environmental Protection Agency", "Environmental Protection Agency")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "EPA", "Department of Transportation", "Energy Information Administration", "Environmental Protection Agency")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "VA", "FTA", "Federal Transit Administration", "Federal Transit Administration")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "FEMA", "Energy Information Administration", "Federal Emergency Management Agency", "Federal Emergency Management Agency")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "National Marine Fisheries Service", "California Coastal Commission", "Department of Parks and Recreation", "California Coastal Commission")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "FWS", "Fish and Wildlife Service", "Department of the Interior", "Fish and Wildlife Service")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "Federal Highway Administration", "Department of Transportation", "California Department of Transportation", "California Department of Transportation")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "OEHHA", "Census Bureau", "Office of Environmental Health Hazard Assessment", "Office of Environmental Health Hazard Assessment")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "Department of Transportation", "California Department of Transportation", "SFCTA", "SFCTA")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransportation\\s[Cc]ommission\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,4}$", "RCTC", "RCTC")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "USGS", "USDA", "Census Bureau", "USGS")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "FTA", "Federal Transit Administration", "SCAG", "Federal Transit Administration")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "CDFW", "Department of Fish and Wildlife", "AMBAG", "Department of Fish and Wildlife")
clean.for.next.input()

make.duplicate.for3(exact.agency.match.u, "Federal Transit Administration", "Department of Transportation", "California Department of Transportation", "California Department of Transportation")
clean.for.next.input()

make.duplicate.for5(exact.agency.match.u, "NOAA", "DOC", "State Coastal Conservancy", "National Oceanic and Atmospheric Administration", "Department of Commerce", "National Oceanic and Atmospheric Administration")
clean.for.next.input()


# Join with agency lists to assign level (Federal, CA State, State, County, City, ) ----

# Recreate this into a dataframe
locallist <- read_csv("~/Box/truckee/data/agency_doc_metadata/agencylist.csv")
locallist <- locallist %>% select(agency, agency_code)
colnames(locallist)[2] <- "abbr"
# remove caltrans
locallist <- locallist[-61,]
locallist$level <- "sub-state"
locallist$abbr <- toupper(locallist$abbr)

agencylist <- read.csv("/Users/lizawood/Box/truckee/data/ca_fed_agencies_short.csv")
agencylist <- agencylist %>% select(agency, abbr, level)

agencymisc <- data.frame(c( "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,2}Agricultural\\sCommissioner’s\\sOffice\\s*([a-zA-Z]+\\s*){0,2}$",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,3}Association\\s[Oo]f\\sGovernments$",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,5}[Dd]ept[.]*\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", 
  "^California\\sAssociation\\sfor\\sCoordinated\\sTransportation$",
  "^California\\sAir\\sPollution\\sControl\\sOfficers",
  "^California\\s\\(State\\)\\sCoastal\\Conservancy",
  "^Caltrans\\s[Dd]istrict\\s*([a-zA-Z0-9]+\\s*){0,3}$",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,4}\\,\\s[Cc]ity\\s\\of$",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){1,3}\\sCounty\\s*([a-zA-Z]+\\s*){0,4}$",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,3}Council\\s[Oo]f\\sGovernments\\s*([a-zA-Z0-9:punct:]+\\s*){0,3}$",
  "^Journal of Transportation and Statistics$", # federal journal
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Mm]anagement\\s[Dd]istrict\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", 
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Mm]etropolitan\\s[Aa]uthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,2}$",
  "^Monthly Energy Review$", # EIA Report
  "^National\\sRenewable\\sEnergy\\sLaboratory\\s*([a-zA-Z0-9:punct:]+\\s*){0,2}$",
  "^National Register Bulletin$",
  "^Planning Assistance and Standards 23", # This is FHWA but I'm not catching it now
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Rr]ail[road]*\\s[Aa]uthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
  "^SFRWQCB\\s\\(San\\sFrancisco\\sBay\\sRegional",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}Flood\\sControl\\sDistrict$",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransit\\s[Aa]uthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransit\\s[Aa]dministration\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransportation\\s[Aa]uthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}Transportaiton\\sAuthority\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$", 
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}[Tt]ransportation\\s[Cc]ommission\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$",
  "^\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}Water\\sDistrict\\s*([a-zA-Z0-9:punct:]+\\s*){0,4}$"))

colnames(agencymisc) <- "abbr"
agencymisc$agency <- c("Agricultural Commissioner's Office",
                     "Assosciation of Governments",
                     "Misc. department",
                     "California Association for Coordinated Transportation",
                     "Calfiornia Air Pollution Control Officers",
                     "California Coastal Conservancy",
                     "Caltrans District",
                     "City of",
                     "County",
                     "Council of Governments",
                     "Department of Transportation",
                     "Management District",
                     "Metropolitan Authority",
                     "Energy Information Administration",
                     "National Renewable Energy Laboratory",
                     "National Register Bulletin",
                     "FHWA",
                     "Railroad Authority",
                     "SFRWQCB",
                     "Flood Control District",
                     "Transit Authority",
                     "Transit Administration",
                     "Transportation Authority",
                     "Transportation Authority",
                     "Transportation Commission",
                     "Water District")
agencymisc$level <- NA


agency.total <- rbind(locallist, agencylist, agencymisc)
agency.total <- map_df(agency.total, trimws)
exact.agency.match.u <- map_df(exact.agency.match.u, trimws)

colnames(agency.total)[1] <- "match.name"
agency.matches1 <- left_join(exact.agency.match.u, agency.total, by = "match.name")
colnames(agency.total) <-c("agency", "match.name", "level") 
agency.matches2 <- left_join(agency.matches1, agency.total, by = "match.name")

table(is.na(exact.agency.match.u$match.name))

# Unify names (turn abbreviations into full names)
# If agency is NA, then fill it with matchname, If abbr is na, fill it with matchname
agency.matches2$agency.final <- ifelse(is.na(agency.matches2$agency), agency.matches2$match.name, agency.matches2$agency)
agency.matches2$abbr.final <- ifelse(is.na(agency.matches2$abbr), agency.matches2$match.name, agency.matches2$abbr)

agency.matches2$abbr.final <- ifelse(str_detect(agency.matches2$abbr.final, "^\\^|\\s"), NA, agency.matches2$abbr.final)

agency.matches2$agency.author.level <- ifelse(is.na(agency.matches2$level.y) & 
                                               !is.na(agency.matches2$level), agency.matches2$level,
                                      ifelse(!is.na(agency.matches2$level.y) & 
                                               is.na(agency.matches2$level), agency.matches2$level.y,
                                      ifelse(!is.na(agency.matches2$level.y) & 
                                               !is.na(agency.matches2$level), agency.matches2$level, 
                                             agency.matches2$level)))


# Clean up agency.matches2
colnames(agency.matches2)
agency.matches2 <- agency.matches2 %>% select(Author, Year, Title, Journal, Publisher, DOI, URL, File, i, agency.final, abbr.final, agency.author.level)
colnames(agency.matches2)[c(10,11)] <- c("agency.author", "agency.author.abbr")

doublei <- agency.matches2 %>% group_by(i) %>% 
  count() %>% 
  filter(n>1) %>% 
  arrange(desc(n))

# if we are in the double i list and abbr is NA, remove -- this is because there was a double match somewhere and then we assigned an NA to the abbreviation

agency.matches2$remove <- ifelse(agency.matches2$i %in% doublei$i & is.na(agency.matches2$agency.author.abbr), T, F)
table(agency.matches2$remove)

agency.matches3 <- agency.matches2 %>% filter(remove == F)

doublei <- agency.matches3 %>% group_by(i) %>% 
  count() %>% 
  filter(n>1) %>% 
  arrange(desc(n))

# NO MORE DOUBLE I'S. THIS IS GREAT NEWS

# exact.agency.match.u -- merge with DF? so that eventually we have the whole dataframe with wither their govt documents, or their journal, and then I can do the summaries on all that are -- I think it will be only like 6500 documents. But, with 9000 journals and 6000 documents, this could be it.
colnames(agency.matches3)
agency.matches3$Year<- as.numeric(agency.matches3$Year)
dfi <- left_join(df, agency.matches3)
colnames(dfi)
dfi <- dfi %>% select(Author, Year, Title, Journal, Publisher, DOI, URL, File,level, i, agency.author, agency.author.abbr, agency.author.level, agency_citation, org_citation, media_citation, conf_citation, journalpub_match)
colnames(dfi)[9] <- "document_level"

# WRITE THIS CLEANED DF ----
write.csv(dfi, "~/Box/truckee/data/compiled_anystyle_results/combined_anystyle_citations_journal.agency.csv", row.names = F)
