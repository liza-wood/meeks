

journals20 <- read.csv("~/Desktop/scimagojr2020.csv", sep = ";") %>% select(Title)
journals19 <- read.csv("~/Desktop/scimagojr2019.csv", sep = ";") %>% select(Title)
journals18 <- read.csv("~/Desktop/scimagojr2018.csv", sep = ";") %>% select(Title)
journals15 <- read.csv("~/Desktop/scimagojr2015.csv", sep = ";") %>% select(Title)
journals13 <- read.csv("~/Desktop/scimagojr2013.csv", sep = ";") %>% select(Title)
journals10 <- read.csv("~/Desktop/scimagojr2010.csv", sep = ";") %>% select(Title)

journals <- rbind(journals20, journals19, journals18, journals15, journals13, journals10) %>% unique()

journals <- journals %>% 
  mutate(title = tools::toTitleCase(base::trimws(stringr::str_remove_all(Title, 
                                                                         "\\.|\\,|\\;|\\*|\\-")))) %>% 
  select(Title)

# on all journals
df <- readRDS("data/clean-df.RDS")
alljournals <- df %>% filter(journalpub_match == T) %>% 
  select(cit_journal) %>% unique()

alljournals$openaccess <- ifelse(alljournals$cit_journal %in% journals$Title, T, F)
table(alljournals$openaccess)
113/nrow(alljournals)

# Ran traditional journalmatchtop in other script

trad <- data.frame(cit_journal = unique(journalmatch.top$cit_journal))
trad$openaccess <- ifelse(trad$cit_journal %in% journals$Title, T, F)
table(trad$openaccess)

# Ran emerging journalmatchtop in other script

emerg <- data.frame(cit_journal = unique(journalmatch.top$cit_journal))
emerg$openaccess <- ifelse(emerg$cit_journal %in% journals$Title, T, F)
table(emerg$openaccess)
