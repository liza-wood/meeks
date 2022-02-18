library(tidyverse)
library(lubridate)
library(cowplot)
library(data.table)
library(sjrdata)

scimago <- data.frame(sjr_journals) %>% 
  mutate(title = tools::toTitleCase(base::trimws(stringr::str_remove_all(title, 
                                                                         "\\.|\\,|\\;|\\*|\\-")))) %>% 
  select(title, sjr) %>% 
  group_by(title) %>% 
  summarize(avg_sjr = mean(sjr, na.rm = T))

# ---------------------------------------------------------------------#
# Section 5.2.1 ITS & Climate transportation documents' journals
# ---------------------------------------------------------------------#

df <- readRDS("data/clean-df.RDS")

# ---------------------------------------------------------------------#
# Each subject summary for text
# ---------------------------------------------------------------------#
# Quick review of each subject
its <- df %>% 
  filter(doc_subject == "Intelligent Transportation Systems")

# number of total
its %>% select(doc.id) %>% unique() %>% nrow()
identified.its <- its %>% filter(identified.citation == T) # 245 citations
journal.its <- identified.its %>% filter(journalpub_match == T) # 41 to journals
agency.its <- identified.its %>% filter(agency_citation == T) # 204 to agencies

# Number of unique journals
journal.its %>% select(cit_journal) %>% unique() %>% nrow() # 23 unique journals
# Number of unique agencies
agency.its %>% select(cit_agency_author_specific) %>% unique() %>% nrow() # 41 unique agencies

# Quick review of each subject
cc <- df %>% 
  filter(doc_subject == "Climate, Envt. & Sust.")

# number of total
cc %>% select(doc.id) %>% unique() %>% nrow()
identified.cc <- cc %>% filter(identified.citation == T) # 1589 citations
journal.cc <- identified.cc %>% filter(journalpub_match == T) # 633 to journals
agency.cc <- identified.cc %>% filter(agency_citation == T) # 956 to agencies

# Number of unique journals
journal.cc %>% select(cit_journal) %>% unique() %>% nrow() # 199 unique journals
# Number of unique agencies
agency.cc %>% select(cit_agency_author_specific) %>% unique() %>% nrow() # 94 unique agencies

# ---------------------------------------------------------------------#
# Creating Table 4
# ---------------------------------------------------------------------#
df <- df %>% 
  filter(doc_subject == "Intelligent Transportation Systems" | 
         doc_subject == "Climate, Envt. & Sust.")

identified.df <- df %>% filter(identified.citation == T) 
journal.df <- identified.df %>% filter(journalpub_match == T) 
agency.df <- identified.df %>% filter(agency_citation == T) 

# Journals and their rankings 

# Number of academic citations and unique journals
nrow(journal.df) # 674
journal.df %>% select(cit_journal) %>% unique() %>% nrow() # 207 unique journals

# Also for table: Number of documents with identified citations, by level
identified.df %>% select(doc.id) %>% unique %>% nrow()

identified.df %>% 
  select(doc.id, doc_owner_agency_level) %>% 
  group_by(doc_owner_agency_level) %>% 
  unique() %>% 
  count() %>% 
  rename("total_id_docs" = "n")

# ---------------------------------------------------------------------#
# Table 4A, part 1. Journal citations by agency level
# ---------------------------------------------------------------------#
journalmatch.top <- df %>% 
  filter(journalpub_match == T) %>% 
  group_by(doc_owner_agency_level, cit_journal) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(sum = sum(n), prop = n/sum) %>% 
  top_n(5) %>% 
  arrange(doc_owner_agency_level, -prop)

# Percent citations that are academic
nrow(journal.df)/nrow(identified.df)

# In text: What percentage of identified do academic citations account for?
unique(journalmatch.top$sum[journalmatch.top$doc_owner_agency_level == "State"])/nrow(identified.df[identified.df$doc_owner_agency_level == "State"])
unique(journalmatch.top$sum[journalmatch.top$doc_owner_agency_level == "Regional"])/nrow(identified.df[identified.df$doc_owner_agency_level == "Regional"])
unique(journalmatch.top$sum[journalmatch.top$doc_owner_agency_level == "County"])/nrow(identified.df[identified.df$doc_owner_agency_level == "County"])


# In text: top journal across all levels 
journalmatch.count <- df %>% 
  filter(journalpub_match == T) %>% 
  group_by(cit_journal) %>% count() %>% 
  ungroup() %>% 
  mutate(sum = sum(n)) %>% 
  mutate(prop = n/sum)

# ---------------------------------------------------------------------#
# Appending scimago rankings
# ---------------------------------------------------------------------#

df <- left_join(df, scimago, by = c("cit_journal" = "title"))

# In text: Overall summaries of sjr
df %>% 
  filter(journalpub_match == T) %>% 
  select(avg_sjr) %>% 
  summary()

sjr.list <- df %>% 
  filter(journalpub_match == T) %>% 
  select(avg_sjr) 
sd(sjr.list$avg_sjr)

# For table for SJR and top journals by agency level
journalmatch.count.if <- df %>% 
  filter(journalpub_match == T) %>% 
  group_by(doc_owner_agency_level , cit_journal, avg_sjr) %>% 
  count() %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(sum = sum(n)) %>% 
  mutate()

journalmatch.count.if.nolevel <- df %>% 
  filter(journalpub_match == T) %>% 
  group_by(cit_journal, avg_sjr) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(sum = sum(n)) %>% 
  mutate()

# See average sjr by level
journalmatch.count.if %>% 
  group_by(doc_owner_agency_level) %>% 
  summarize(avg = mean(avg_sjr, na.rm = T), 
            med = median(avg_sjr, na.rm = T), 
            sd = sd(avg_sjr, na.rm = T))

# ---------------------------------------------------------------------#
# Table 4a. part 2, adding in ranking
# ---------------------------------------------------------------------#
journalmatch.top.cit <- journalmatch.count.if %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(prop = round(n/sum*100, 1),
         avg_sjr = round(avg_sjr, 2)) %>% 
  top_n(5) %>% 
  arrange(doc_owner_agency_level, -prop)

# Or without agency level

journalmatch.top.cit.nolevel <- journalmatch.count.if.nolevel %>% 
  mutate(prop = round(n/sum*100, 1),
         avg_sjr = round(avg_sjr, 2)) %>% 
  top_n(10) %>% 
  arrange(-prop)

# ---------------------------------------------------------------------#
# Scimago journal categories
# ---------------------------------------------------------------------#

# First, lets make sure we remove any Q from the categories of the journals
removeQ <- function(x){str_remove(x, "\\(Q\\d\\)")}
df[,c(44:54)] <- data.frame(lapply(df[,c(44:54)], removeQ))
df[,c(44:54)] <- data.frame(lapply(df[,c(44:54)], function(x) trimws(x)))

# Lengthen the data frame of journal categories

jcats.long <- df %>% 
  filter(journalpub_match == T) %>% 
  select(cit_journal, cit_cat1:cit_cat11, doc_owner_agency_level, file) %>% 
  pivot_longer(cols = cit_cat1:cit_cat11, 
               names_to = "cat.num", 
               values_to = "Category") %>% 
  filter(!is.na(Category)) %>% 
  filter(Category != "")

# Category count
cat.count <- jcats.long %>% 
  group_by(Category) %>% 
  count() 

# Categories as a 
prop.cat.df <- jcats.long %>% 
  group_by(doc_owner_agency_level, Category) %>% 
  count(Category) %>% rename("cat.count" = "n") %>% 
  ungroup() %>%
  group_by(doc_owner_agency_level) %>% 
  mutate(total = sum(cat.count)) %>% 
  mutate(prop = cat.count/total) %>% 
  top_n(10) %>% 
  arrange(doc_owner_agency_level, -prop)

prop.cat.df.nolevel <- jcats.long %>% 
  group_by(Category) %>% 
  count(Category) %>% rename("cat.count" = "n") %>% 
  ungroup() %>%
  mutate(total = sum(cat.count)) %>% 
  mutate(prop = cat.count/total) %>% 
  top_n(10) %>% 
  arrange(-prop)

# ---------------------------------------------------------------------#
# Section 5.3.2 agency references
# ---------------------------------------------------------------------#

# Agencies

# Number of agency citations and unique agencies
nrow(agency.df)
agency.df %>% select(cit_agency_author_specific) %>% unique() %>% nrow() # 256 unique agencies

# ---------------------------------------------------------------------#
# Table 4b Agency citations by agency level
# ---------------------------------------------------------------------#
agencymatch.top <- df %>% 
  filter(agency_citation == T) %>% 
  group_by(doc_owner_agency_level, cit_agency_author_specific) %>% 
  count() %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(sum = sum(n), prop = round(n/sum*100, 1)) %>% 
  top_n(10) %>% 
  arrange(doc_owner_agency_level, -prop)

agencymatch.top.nolevel <- df %>% 
  filter(agency_citation == T) %>% 
  group_by(cit_agency_author_specific) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(sum = sum(n), prop = round(n/sum*100, 1)) %>% 
  top_n(10) %>% 
  arrange(-prop)

# Also for table: Number of documents with identified citations, by level
identified.df %>% 
  select(doc.id, doc_owner_agency_level) %>% 
  group_by(doc_owner_agency_level) %>% 
  unique() %>% 
  count() %>% 
  rename("total_id_docs" = "n")

# In text: What percentage of identified do academic citations account for?
unique(agencymatch.top$sum[agencymatch.top$doc_owner_agency_level == "State"])/nrow(identified.df[identified.df$doc_owner_agency_level == "State"])
unique(agencymatch.top$sum[agencymatch.top$doc_owner_agency_level == "Regional"])/nrow(identified.df[identified.df$doc_owner_agency_level == "Regional"])
unique(agencymatch.top$sum[agencymatch.top$doc_owner_agency_level == "County"])/nrow(identified.df[identified.df$doc_owner_agency_level == "County"])

# In text: top agencies across all levels 
agencymatch.count <- df %>% 
  filter(agency_citation == T) %>% 
  group_by(cit_agency_author_specific) %>% count() %>% 
  ungroup() %>% 
  mutate(sum = sum(n)) %>% 
  mutate(prop = n/sum)

