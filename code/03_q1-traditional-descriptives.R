library(tidyverse)
library(lubridate)
library(cowplot)
library(data.table)
library(sjrdata)

df <- readRDS("data/clean-df.RDS")

df <- df %>% 
  filter(doc_subject != "Intelligent Transportation Systems" & 
           doc_subject != "Climate & Sustainability")

identified.df <- df %>% filter(identified.citation == T) 
journal.df <- identified.df %>% filter(journalpub_match == T) 
agency.df <- identified.df %>% filter(agency_citation == T) 

# ---------------------------------------------------------------------#
# Section 5.2.1 Traditional transportation documents' journals
# ---------------------------------------------------------------------#

# Journals and their rankings 

# Number of academic citations and unique journals
nrow(journal.df) # 8355 academic citations
journal.df %>% select(cit_journal) %>% unique() %>% nrow() # 1101 unique journals

# Also for table: Number of documents with identified citations, by level
identified.df %>% 
  select(doc.id, doc_owner_agency_level) %>% 
  group_by(doc_owner_agency_level) %>% 
  unique() %>% 
  count() %>% 
  rename("total_id_docs" = "n")

# ---------------------------------------------------------------------#
# Table 3a part 1. Journal citations by agency level
# ---------------------------------------------------------------------#
journalmatch.top <- identified.df %>% 
  filter(journalpub_match == T) %>% 
  group_by(doc_owner_agency_level, cit_journal) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(sum = sum(n), prop = n/sum) %>% 
  top_n(10) %>% 
  arrange(doc_owner_agency_level, -prop)

# In text: What percentage of identified do academic citations account for?
unique(journalmatch.top$sum[journalmatch.top$doc_owner_agency_level == "State"])/nrow(identified.df[identified.df$doc_owner_agency_level == "State"])
unique(journalmatch.top$sum[journalmatch.top$doc_owner_agency_level == "Regional"])/nrow(identified.df[identified.df$doc_owner_agency_level == "Regional"])
unique(journalmatch.top$sum[journalmatch.top$doc_owner_agency_level == "County"])/nrow(identified.df[identified.df$doc_owner_agency_level == "County"])

# In text: top journal across all levels 
df %>% 
  filter(journalpub_match == T) %>% 
  group_by(cit_journal) %>% count() %>% 
  ungroup() %>% 
  mutate(sum = sum(n)) %>% 
  mutate(prop = n/sum) %>% 
  arrange(-prop)

# ---------------------------------------------------------------------#
# Appending scimago rankings
# ---------------------------------------------------------------------#

scimago <- data.frame(sjr_journals) %>% 
  mutate(title = tools::toTitleCase(base::trimws(stringr::str_remove_all(title, 
                                                      "\\.|\\,|\\;|\\*|\\-")))) %>% 
  select(title, sjr) %>% 
  group_by(title) %>% 
  summarize(avg_sjr = mean(sjr, na.rm = T))

df <- left_join(df, scimago, by = c("cit_journal" = "title"))

# In text: Overall summaries of sjr
df %>% 
  filter(journalpub_match == T) %>% 
  select(avg_sjr) %>% 
  summary()

# For table for SJR and top journals by agency level
journalmatch.count.if <- df %>% 
  filter(journalpub_match == T) %>% 
  group_by(doc_owner_agency_level , cit_journal, avg_sjr) %>% 
  count() %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(sum = sum(n)) %>% 
  mutate()

# See average sjr by level
journalmatch.count.if %>% 
  group_by(doc_owner_agency_level) %>% 
  summarize(avg = mean(avg_sjr, na.rm = T), 
            med = median(avg_sjr, na.rm = T), 
            sd = sd(avg_sjr, na.rm = T))

# ---------------------------------------------------------------------#
# Table 3a. part 2, adding in ranking
# ---------------------------------------------------------------------#
journalmatch.top.cit <- journalmatch.count.if %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(prop = round(n/sum*100, 1),
         avg_sjr = round(avg_sjr, 2)) %>% 
  top_n(10) %>% 
  arrange(doc_owner_agency_level, -prop)


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

# ---------------------------------------------------------------------#
# Section 5.2.2 Traditional agency references
# ---------------------------------------------------------------------#

# Agencies

# Number of agency citations and unique agencies
nrow(agency.df) # 6409
agency.df %>% select(cit_agency_author_specific) %>% unique() %>% nrow() # 256 unique agencies

# Also for table: Number of documents with identified citations, by level
# (this should be the same as for the journals)
identified.df %>% 
  select(doc.id, doc_owner_agency_level) %>% 
  group_by(doc_owner_agency_level) %>% 
  unique() %>% 
  count() %>% 
  rename("total_id_docs" = "n")

# In text: Unique agencies per level
length(unique(agency.df$cit_agency_author_specific[agency.df$doc_owner_agency_level == "State"]))
length(unique(agency.df$cit_agency_author_specific[agency.df$doc_owner_agency_level == "Regional"]))
length(unique(agency.df$cit_agency_author_specific[agency.df$doc_owner_agency_level == "County"]))
 

# ---------------------------------------------------------------------#
# Table 3b Agency citations by agency level
# ---------------------------------------------------------------------#
agencymatch.top <- df %>% 
  filter(agency_citation == T) %>% 
  group_by(doc_owner_agency_level, cit_agency_author_specific) %>% 
  count() %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(sum = sum(n), prop = round(n/sum*100, 1)) %>% 
  top_n(10) %>% 
  arrange(doc_owner_agency_level, -prop)

# In text: What percentage of identified do academic citations account for?
unique(agencymatch.top$sum[agencymatch.top$doc_owner_agency_level == "State"])/nrow(identified.df[identified.df$doc_owner_agency_level == "State"])
unique(agencymatch.top$sum[agencymatch.top$doc_owner_agency_level == "Regional"])/nrow(identified.df[identified.df$doc_owner_agency_level == "Regional"])
unique(agencymatch.top$sum[agencymatch.top$doc_owner_agency_level == "County"])/nrow(identified.df[identified.df$doc_owner_agency_level == "County"])

# In text: top agency across all levels 
agencymatch.count <- df %>% 
  filter(agency_citation == T) %>% 
  group_by(cit_agency_author_specific) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(sum = sum(n)) %>% 
  mutate(prop = n/sum)

# ---------------------------------------------------------------------#
# Figure 5. Scimago ranking plot
# ---------------------------------------------------------------------#

citrank.count <- df %>% 
  filter(journalpub_match == T, !is.na(avg_sjr)) %>% 
  group_by(cit_journal, avg_sjr) %>% count() 

top8journals <- journalmatch.count.if %>% 
  group_by(doc_owner_agency_level) %>% 
  select(-sum) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  select(cit_journal) %>% 
  unique()

citrank.count$top8 <- ifelse(citrank.count$cit_journal %in% top8journals$cit_journal, "Top-referenced journals", "Other journals")
table(citrank.count$top8)
citrank.count$top8 <- factor(citrank.count$top8, levels = c("Top-referenced journals", "Other journals"))

# Figure 5.

# Range of impact factors
citrank.count %>% 
  ggplot(aes(x = avg_sjr, y = n)) + 
  geom_point(aes(fill = top8), color = "black", alpha = 0.7, pch = 21, size = 2) +
  scale_fill_manual(values = c("black", "white")) + 
  #geom_smooth(method = "lm", color = "black") +
  xlim(0,30) + 
  ylim(0,210) +
  theme_classic() +
  theme(#legend.position = "none",
    text= element_text(size=14, family="Times"), 
    axis.text = element_text(hjust = .6, size=14),
    plot.title = element_text(hjust = .5, vjust = 0, size = 16), 
    strip.background  = element_blank()) +
  labs(x = "Average journal ranking (SJR)", 
       y = "Number of citations",
       fill = "")

# Figure 5 saving
ggsave(filename = "plots/fig5_sjr_by_citation.png", width = 6, height = 3)
