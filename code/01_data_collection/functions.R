# include perl = T if you have a fancy grepl expression -- this is to make sure it reads regex normally

assignsubject <- function() {

  string = metadata$doc_title
  ap = str_detect(string, "[Aa]ction\\s+[Pp]lan")
  at = str_detect(string, "[Aa]ctive\\s+[Tt]ransport[a-z]*|[Cc]omplete\\s+[Ss]treet[s]*|[Pp]edestrian|[Bb]ike[a-z]*|[Bb]icycl[a-z]*|[Hh]ealthy\\s+[Cc]ommunit[a-z]*")
  aluc = str_detect(string, "[Aa]irport|aluc[p]*")
  cmaq = str_detect(string, "[Cc]ongest[a-z]*|[Aa]ir\\s+[Qq]uality|[Tt]raffic")
  ctp = str_detect(string, "(?<!\\s)CTP|[Cc]ounty[a-z]*\\s+[Tt]ransport[a-z]*\\s+[Pp]lan|
                   [Cc]ommunity-based\\s+[Tt]ransport[a-z]*\\s+[Pp]lan")
  climate_change = str_detect(string, "[Cc]limate|[Rr]esilience")
  ej = str_detect(string, "[Ee]nvironmental\\s+[Jj]ustice")
  envt = str_detect(string, "[Ss]tormwater|[Hh]abitat|[Ee]nvironment[a-z]*|[Bb]each")
  financial = str_detect(string, "[Bb]udget|[Ff]inancial|[Tt]axpayer\\s+[Oo]versight")
  ftip = str_detect(string, "FTIP|[Ff]ederal\\s+[Tt]ransportation\\s+[Pp]rogram")
  freight = str_detect(string, "[Ff]reight|[Gg]oods\\s+[Mm]ovement")
  growth = str_detect(string, "[Cc]ensus|[Gg]rowth|[Pp]opulation|[Ee]conom[a-z]*")
  hna = str_detect(string, "[Hh]ousing|(?<!\\s)RHNA")
  its = str_detect(string, "(?<!\\s)ITS|[Ii]ntelligent\\s+[Tt]ransport[a-z]*|[Ee]lectric\\s+[Vv]ehicle|
                   \\s+EV\\s+|[Aa]lternative\\s+[Ff]uel[s]*")
  mobility = str_detect(string, "[Mm]obility|[Tt]ransport[a-z]*|\\s+TOD")
  measure = str_detect(string, "[Mm]easure")
  model = str_detect(string, "[Mm]odel")
  outreach = str_detect(string, "[Oo]utreach|[Pp]ublic\\s+[Pp]articipation\\s+[Pp]lan|PPP|
                        [Pp]ublic\\s+[Ii]nvolvement")
  owp = str_detect(string, "(?<!\\s)OWP|[Oo]verall\\s+[Ww]ork\\s+[Pp]rogram|[Pp]erformance\\s+[Aa]udit|
                   [Pp]rogram\\s+[Aa]ssessment|[Pp]erformance\\s+[Mm]onitor[a-z]*")
  rail = str_detect(string, "\\s+[Rr]ail|[Dd]ouble\\s+[Tt]rack")
  rtip = str_detect(string, "(?<!\\s)RTIP| (?<!\\s)MTIP|[Rr]egional\\s+[Tt]ransportation\\s+[Ii]mprovement
                    \\s+[Pp]rogram|[Mm]etropolitan\\s+[Ii]mprovement\\s+[Tt]ransportation\\s+[Pp]rogram")
  rtp = str_detect(string, "RTP|[Rr]egional\\s+[Tt]ransportation\\s+[Pp]lan")
  road = str_detect(string, "[Rr]oad|[Cc]orridor|[Ss]treet|[Hh]ighway|[Ii]nterstate|(?<!\\s)I-[0-9]+|
                    [Ff]reeway|(?<!\\s)SR\\s+[0-9]+|[Pp]avement")
  safety = str_detect(string, "[Ss]afe[a-z]*")
  sustainability = str_detect(string, "[Bb]lueprint[s]*|[Gg]reenpint[s]*|[Ss]ustainab[a-z]*|[Ee]nergy")
  titlevi = str_detect(string, "[Tt]itle\\s+VI|[Dd]isabilit[a-z]*")
  transit = str_detect(string, "[Tt]ransit|[Bb]us|[Cc]ommute[a-z]*")

doc_subject = ifelse(rtp == T, "rtp",
              ifelse(ctp == T, "ctp",
              ifelse(ftip == T, "ftip",
              ifelse(rtip == T, "rtip",
              ifelse(at == T, "at",
              ifelse(cmaq == T, "cmaq",
              ifelse(climate_change == T, "climate_change",
              ifelse(ej == T, "ej",
              ifelse(freight == T, "freight",
              ifelse(measure == T, "measure",
              ifelse(outreach == T, "outreach",
              ifelse(hna == T, "hna",
              ifelse(growth == T, "growth",
              ifelse(ap == T, "ap",
              ifelse(road == T, "road",
              ifelse(safety == T, "safety",
              ifelse(transit == T, "transit",
              ifelse(rail == T, "rail", 
              ifelse(aluc == T, "aluc",
              ifelse(model == T, "model",
              ifelse(envt == T, "envt",
              ifelse(titlevi == T, "titlevi",
              ifelse(owp == T, "owp",
              ifelse(its == T, "its",
              ifelse(sustainability == T, "sustainability",
              ifelse(mobility == T, "mobility",
              ifelse(financial == T, "financial", NA_character_)))))))))))))))))))))))))))

metadata$doc_subject <<- doc_subject

  string = metadata$url
  ap = str_detect(string, "[Aa]ction_[Pp]lan")
  at = str_detect(string, "[Aa]ctive_[Tt]ransport[a-z]*|[Cc]omplete_[Ss]treet[s]*|[Pp]edestrian|
                    [Bb]ike[a-z]*|[Bb]icycl[a-z]*|[Hh]ealthy_[Cc]ommunit[a-z]*")
  aluc = str_detect(string, "[Aa]irport|aluc[p]*")
  cmaq = str_detect(string, "[Cc]ongest[a-z]*|[Aa]ir_[Qq]uality|[Tt]raffic")
  ctp = str_detect(string, "(?<!\\s)CTP|[Cc]ounty[a-z]*_[Tt]ransport[a-z]*_[Pp]lan|
                     [Cc]ommunity-based_[Tt]ransport[a-z]*_[Pp]lan")
  climate_change = str_detect(string, "[Cc]limate|[Rr]esilience")
  ej = str_detect(string, "[Ee]nvironmental_[Jj]ustice")
  envt = str_detect(string, "[Ss]tormwater|[Hh]abitat|[Ee]nvironment[a-z]*|[Bb]each")
  financial = str_detect(string, "[Bb]udget|[Ff]inancial|[Tt]axpayer_[Oo]versight")
  ftip = str_detect(string, "FTIP|[Ff]ederal_[Tt]ransportation_[Pp]rogram")
  freight = str_detect(string, "[Ff]reight|[Gg]oods_[Mm]ovement")
  growth = str_detect(string, "[Cc]ensus|[Gg]rowth|[Pp]opulation|[Ee]conom[a-z]*")
  hna = str_detect(string, "[Hh]ousing|(?<!\\s)RHNA")
  its = str_detect(string, "(?<!\\s)ITS|[Ii]ntelligent_[Tt]ransport[a-z]*|[Ee]lectric_[Vv]ehicle|
                     _EV_|[Aa]lternative_[Ff]uel[s]*|[Zz]ero-[Ee]mission")
  mobility = str_detect(string, "[Mm]obility|[Tt]ransport[a-z]*|_TOD")
  measure = str_detect(string, "[Mm]easure")
  model = str_detect(string, "[Mm]odel")
  outreach = str_detect(string, "[Oo]utreach|[Pp]ublic_[Pp]articipation_[Pp]lan|PPP|
                          [Pp]ublic_[Ii]nvolvement")
  owp = str_detect(string, "(?<!\\s)OWP|[Oo]verall_[Ww]ork_[Pp]rogram|[Pp]erformance_[Aa]udit|
                     [Pp]rogram_[Aa]ssessment|[Pp]erformance_[Mm]onitor[a-z]*")
  rail = str_detect(string, "_[Rr]ail|[Dd]ouble_[Tt]rack")
  rtip = str_detect(string, "(?<!\\s)RTIP| (?<!\\s)MTIP|[Rr]egional_[Tt]ransportation_[Ii]mprovement
                      _[Pp]rogram|[Mm]etropolitan_[Ii]mprovement_[Tt]ransportation_[Pp]rogram")
  rtp = str_detect(string, "RTP|[Rr]egional_[Tt]ransportation_[Pp]lan")
  road = str_detect(string, "[Rr]oad|[Cc]orridor|[Ss]treet|[Hh]ighway|[Ii]nterstate|(?<!\\s)I-[0-9]+|
                      [Ff]reeway|(?<!\\s)SR_[0-9]+|[Pp]avement")
  safety = str_detect(string, "[Ss]afe[a-z]*")
  sustainability = str_detect(string, "[Bb]lueprint[s]*|[Gg]reenpint[s]*|[Ss]ustainab[a-z]*|[Ee]nergy")
  titlevi = str_detect(string, "[Tt]itle_VI|[Dd]isabilit[a-z]*")
  transit = str_detect(string, "[Tt]ransit|[Bb]us|[Cc]ommute[a-z]*")

doc_subject2 = ifelse(rtp == T, "rtp",
               ifelse(ctp == T, "ctp",
               ifelse(ftip == T, "ftip",
               ifelse(rtip == T, "rtip",
               ifelse(at == T, "at",
               ifelse(its == T, "its",
               ifelse(cmaq == T, "cmaq",
               ifelse(climate_change == T, "climate_change",
               ifelse(ej == T, "ej",
               ifelse(freight == T, "freight",
               ifelse(measure == T, "measure",
               ifelse(outreach == T, "outreach",
               ifelse(hna == T, "hna",
               ifelse(titlevi == T, "titlevi",
               ifelse(sustainability == T, "sustainability",
               ifelse(growth == T, "growth",
               ifelse(ap == T, "ap",
               ifelse(road == T, "road",
               ifelse(safety == T, "safety",
               ifelse(transit == T, "transit",
               ifelse(rail == T, "rail", 
               ifelse(aluc == T, "aluc",
               ifelse(model == T, "model",
               ifelse(envt == T, "envt",
               ifelse(mobility == T, "mobility", NA_character_)))))))))))))))))))))))))

isna = is.na(doc_subject)
doc_subject = ifelse(isna == T, doc_subject2, doc_subject)
metadata$doc_subject <<- doc_subject
}

assigntype <- function() {
  
  string = metadata$doc_title
  agenda = str_detect(string, "[Aa]genda[s]*")
  appendix = str_detect(string, "[Aa][Pp][Pp][Ee][Nn][Dd][Ii][A-Z|a-z]+|[Ff]igure[s]*")
  addendum = str_detect(string, "[Aa]ddendum[s]*")
  amendment = str_detect(string, "[Aa]mendment[s]*|[Mm]odification[s]*")
  application = str_detect(string, "[Aa]pplication[s]*|[Gg]rant[s]*|[Ss]olicitation")
  brochure = str_detect(string, "[Bb]rochure[s]*")
  brief = str_detect(string, "[Bb]rief[a-z]*")
  budget = str_detect(string, "[Bb]udget|[Ff]inancial\\s+[Ss]tatement[s]*")  
  checklist = str_detect(string, "[Cc]hecklist[s]*")
  design = str_detect(string, "[Dd]esign[s]*|[Ee]xhibit[s]*")
  factsheet = str_detect(string, "[Ff]act[a-z]*|[Bb]ooklet|FAQ|InfoBits")
  letter = str_detect(string, "[Ll]etter[s]*")
  list = str_detect(string, "[Ll]ist")
  map = str_detect(string, "[Mm]ap[s]*")
  meeting_notes = str_detect(string, "[Nn]ote[s]*")
  memo = str_detect(string, "[Mm]emo[a-z]*")
  method = str_detect(string, "[Mm]ethod[a-z]*|[Pp]rocess")
  minutes = str_detect(string, "[Mm]inute[s]*")
  model = str_detect(string, "[Mm]odel")
  notice = str_detect(string, "[Nn]otice|[Dd]eclaration")
  policy = str_detect(string, "[Pp]olic[i|y]*")
  presentation = str_detect(string, "[Pp]resentation|[Pp]owerpoint|[Ww]ebinar[s]*")
  resolution = str_detect(string, "[Rr]esolution[s]*")
  summary = str_detect(string, "[Ss]ummary")
  toolkit = str_detect(string, "[Tt]oolkit[s]*")

  eir = str_detect(string, "(?<!\\s)EIR|(?<!\\s)EIS|[Ee]nvironmental\\s+[Ii]mpact")
  guidelines = str_detect(string, "[Gg]uideline[s]*|[Gg]uidebook|[Gg]uid.+|[Bb]est\\s+[Pp]ractice[s]
                          *|[Mm]anual|[Cc]riteria")
  plan = str_detect(string, "\\s[Pp]lan[s]*|(?<!\\s)RTP|(?<!\\s)MTP|(?<!\\s)CTP| (?<!\\s)ATP")
  project = str_detect(string, "[Pp]roject[s]*")
  print = str_detect(string, "[Pp]rint[s]*")
  program = str_detect(string, "[Pp]rogram|(?<!\\s)FTIP|(?<!\\s)RTIP|(?<!\\s)MTIP")
  report = str_detect(string, "[Rr]eport[s]*|[Ss]trategy|[Vv]ision|CMAQ|[Pp]aper")
  study = str_detect(string, "[Ss]tudy|[Aa]nalysis|[Aa]ssessment|[Aa]udit[s]*|[Ss]urvey[s]*|[Ff]orecast|[Ff]easibility")

  doc_type = ifelse(appendix == T, "appendix",
             ifelse (addendum == T, "addendum",
             ifelse(amendment == T, "amendment",
             ifelse(letter == T, "letter",
             ifelse(meeting_notes == T, "meeting_notes",
             ifelse (guidelines == T, "guidelines",
             ifelse(agenda == T, "agenda",
             ifelse(map == T, "map", 
             ifelse(toolkit == T, "toolkit",
             ifelse(budget == T, "budget",
             ifelse(memo == T, "memo",
             ifelse(factsheet == T, "fact_sheet",
             ifelse(brochure == T, "brochure",
             ifelse(minutes == T, "minutes",
             ifelse(minutes == T, "print",
             ifelse(presentation == T, "presentation",
             ifelse(resolution == T, "resolution",
             ifelse(checklist == T, "checklist",
             ifelse(policy == T, "policy",
             ifelse(notice == T, "notice",
             ifelse(brief == T, "brief",
             ifelse(model == T, "model",
             ifelse(method == T, "method",
             ifelse(eir ==T, "eir",
             ifelse(summary == T, "summary",
             ifelse(report == T, "report",
             ifelse(study == T, "study",
             ifelse(program == T, "program",
             ifelse(project == T, "project",
             ifelse(list == T, "list",
             ifelse(design == T, "design",
             ifelse(plan == T, "plan",
             ifelse(application == T, "application",NA_character_)))))))))))))))))))))))))))))))))
  
  string = metadata$url
  agenda = str_detect(string, "[Aa]genda[s]*")
  appendix = str_detect(string, "[Aa][Pp][Pp][Ee][Nn][Dd][Ii][A-Z|a-z]+|[Ff]igure[s]*")
  addendum = str_detect(string, "[Aa]ddendum[s]*")
  amendment = str_detect(string, "[Aa]mendment[s]*|[Mm]odification[s]*")
  application = str_detect(string, "[Aa]pplication[s]*|[Gg]rant[s]*|[Ss]olicitation")
  brochure = str_detect(string, "[Bb]rochure[s]*")
  brief = str_detect(string, "[Bb]rief[a-z]*")
  budget = str_detect(string, "[Bb]udget|[Ff]inancial_[Ss]tatement[s]*")  
  checklist = str_detect(string, "[Cc]hecklist[s]*")
  factsheet = str_detect(string, "[Ff]act[a-z]*|[Bb]ooklet|FAQ|InfoBits")
  letter = str_detect(string, "[Ll]etter[s]*")
  list = str_detect(string, "[Ll]ist")
  map = str_detect(string, "[Mm]ap[s]*|[Ii]nfographic")
  meeting_notes = str_detect(string, "[Nn]ote[s]*")
  memo = str_detect(string, "[Mm]emo[a-z]*")
  method = str_detect(string, "[Mm]ethod[a-z]*|[Pp]rocess")
  notice = str_detect(string, "[Nn]otice|[Dd]eclaration")
  minutes = str_detect(string, "[Mm]inute[s]*")
  model = str_detect(string, "[Mm]odel")
  policy = str_detect(string, "[Pp]olic[i|y]*")
  presentation = str_detect(string, "[Pp]resentation|[Pp]owerpoint|[Ww]ebinar[s]*")
  resolution = str_detect(string, "[Rr]esolution[s]*")
  summary = str_detect(string, "[Ss]ummary")
  toolkit = str_detect(string, "[Tt]oolkit[s]*")
  
  eir = str_detect(string, "(?<!\\s)EIR|(?<!\\s)EIS|[Ee]nvironmental_[Ii]mpact")
  guidelines = str_detect(string, "[Gg]uideline[s]*|[Gg]uidebook|[Gg]uid.+|[Bb]est_[Pp]ractice[s]
                          *|[Mm]anual|[Cc]riteria")
  plan = str_detect(string, "[Pp]lan[s]*|(?<!\\s)RTP|(?<!\\s)MTP|(?<!\\s)CTP| (?<!\\s)ATP")
  project = str_detect(string, "[Pp]roject[s]*")
  print = str_detect(string, "[A-Z|a-z]*[Pp]rint[s]*")
  program = str_detect(string, "[Pp]rogram|(?<!\\s)FTIP|(?<!\\s)RTIP|(?<!\\s)MTIP")
  report = str_detect(string, "[Rr]eport[s]*|[Ss]trategy|[Vv]ision|CMAQ|[Pp]aper")
  study = str_detect(string, "[Ss]tudy|[Aa]nalysis|[Aa]ssessment|[Aa]udit[s]*|[Ss]urvey[s]*|[Ff]orecast|[Ff]easibility")
  
  doc_type2 = ifelse(appendix == T, "appendix",
              ifelse (addendum == T, "addendum",
              ifelse(amendment == T, "amendment",
              ifelse(application == T, "application",
              ifelse(letter == T, "letter",
              ifelse(meeting_notes == T, "meeting_notes",
              ifelse (guidelines == T, "guidelines",
              ifelse(agenda == T, "agenda",
              ifelse(map == T, "map", 
              ifelse(toolkit == T, "toolkit",
              ifelse(budget == T, "budget",
              ifelse(memo == T, "memo",
              ifelse(factsheet == T, "fact_sheet",
              ifelse(brochure == T, "brochure",
              ifelse(minutes == T, "minutes",
              ifelse(minutes == T, "print",
              ifelse(presentation == T, "presentation",
              ifelse(resolution == T, "resolution",
              ifelse(checklist == T, "checklist",
              ifelse(policy == T, "policy",
              ifelse(notice == T, "notice",
              ifelse(brief == T, "brief",
              ifelse(model == T, "model",
              ifelse(method == T, "method",
              ifelse(eir ==T, "eir",
              ifelse(summary == T, "summary",
              ifelse(report == T, "report",
              ifelse(study == T, "study",
              ifelse(program == T, "program",
              ifelse(project == T, "project",
              ifelse(list == T, "list",
              ifelse(plan == T, "plan", NA_character_))))))))))))))))))))))))))))))))
  
  isna = is.na(doc_type)
  doc_type = ifelse(isna == T, doc_type2, doc_type)
  metadata$doc_type <<- doc_type
}

remove.type = c("addendum", "agenda", "egnda", "amendment", "application", "approval", "brochure", "budget", "brief","checklist", "letter", "notice", "meeting_notes", "list", "map", "toolkit", "memo", "program_amendment",  "form", "factsheet", "fact_sheet", "pa", "minutes", "resolution", "directory", "design", "flyer", "grant", "ap")
remove.subject = c("owp", "pa", "ap", "financial", "ports", "administration")

filterdocuments <- function() {
  md.filtered = data.frame()
  # Remove the subjects and types we don't want
  md.filtered = metadata %>% filter(!(doc_type %in% remove.type)) %>% filter(!(doc_subject %in% remove.subject)) 
  # Separate out agency type
  md.filtered$agency_type = str_replace_all(md.filtered$agency_type, "\\s&\\s", "_")
  # Improve title naming
  md.filtered$doc_title = str_replace_all(md.filtered$doc_title, "\\s", "_") 
  md.filtered$doc_title = str_remove_all(md.filtered$doc_title, "\\(|\\)")
  # Remove anything ending in a format we don't want
  md.filtered$url = ifelse(str_detect(md.filtered$url, ".xlsx$|.docx$|.jpeg$|.png$|.html$|.php$"), "nopdf", md.filtered$url)
  # If it does not end in a .pdf, make it end in a .pdf
  md.filtered$url = ifelse(!(str_detect(md.filtered$url, ".pdf$")), paste0(md.filtered$url, ".pdf"), md.filtered$url)
  # If there are white spaces in the url, put a percentage sign there
  md.filtered$url = str_replace_all(md.filtered$url, "\\s", "%")
                           
  md.filtered$doc_id <- 1:nrow(md.filtered)
  md.filtered <<- md.filtered
}