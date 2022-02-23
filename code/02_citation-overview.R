library(tidyverse)
library(data.table)
source("code/functions.R")

df <- readRDS("data/clean-df.RDS")

# Note: Figure 1 in Section 4.2 is in the 01_agency_doc_descriptives
# ---------------------------------------------------------------------#
# Section 4.3
# ---------------------------------------------------------------------#

# How many documents?
length(unique(df$doc.id)) # 5080
# How many agencies?
length(unique(df$doc_owner_agency_code)) # 59

potential.df <- df %>% filter(potential.citation == T) 
nrow(potential.df) #64,311 potential citations
potential.df %>% select(doc.id) %>% unique() %>% nrow() # across 2299 documents

identified.df <- df %>% filter(identified.citation == T) 
nrow(identified.df) #16598 identified citations
# Percentage of what we identified over potential
nrow(identified.df)/nrow(potential.df) # 25.8%

# Journal citations
journal.df <- identified.df %>% filter(journalpub_match == T) 
nrow(journal.df) # 9029 citations
# Academic citations over potential
nrow(journal.df)/nrow(potential.df) # 14.0%
# Academic citations over identified
nrow(journal.df)/nrow(identified.df)  # 54.4%

# Agency citations
agency.df <- identified.df %>% filter(agency_citation == T) 
nrow(agency.df) # 7569
# Agency citations over potential
nrow(agency.df)/nrow(potential.df) # 11.8%
# Agency citations over identified
nrow(agency.df)/nrow(identified.df) # 45.6%

# ---------------------------------------------------------------------#
# Section 5.1
# ---------------------------------------------------------------------#

# total identifiable documents
length(unique(identified.df$file)) # 1273 documents

# This is also the length of documents with identification
citations.per.document <- df %>% 
  filter(identified.citation == T) %>% 
  group_by(file) %>% count() 

nrow(citations.per.document)/length(unique(df$file)) # 25.1% of documents have identifiable citations

# What documents are citing just one citation?
one.cit.file <- identified.df %>% 
  group_by(file) %>% 
  count() %>% 
  filter(n == 1) %>% 
  select(file)

one.cit.df.journal <- journal.df %>% 
  filter(file %in% one.cit.file$file) %>% 
  select(doc_title, cit_author, cit_title, cit_journal)
# I used this list to remove some single word journals, in 01_

one.cit.df.agency <- agency.df %>% 
  filter(file %in% one.cit.file$file) %>% 
  select(doc_title, cit_author, cit_journal, cit_agency_author)

# Frequency of citation counts
colnames(citations.per.document) <- c("file", "cit.per.doc")
cit.numbers.counts <- citations.per.document %>% 
  group_by(cit.per.doc) %>% count()

# documents with one citation over number of documents with identified citations
cit.numbers.counts$n[cit.numbers.counts$cit.per.doc == 1]/nrow(citations.per.document)
summary(cit.numbers.counts$n)

cit.numbers.counts$condensed <- ifelse(cit.numbers.counts$cit.per.doc > 0 & 
                                       cit.numbers.counts$cit.per.doc < 3, "1-2",
                                ifelse(cit.numbers.counts$cit.per.doc >= 3 &
                                       cit.numbers.counts$cit.per.doc < 11, "3-10",
                                ifelse(cit.numbers.counts$cit.per.doc >= 11 &
                                       cit.numbers.counts$cit.per.doc < 26, "11-25",
                                ifelse(cit.numbers.counts$cit.per.doc >= 26, "26+", NA))))

# Grouped distribution of citation frequency
cit.numbers.counts %>% group_by(condensed) %>% 
  summarise(sum_n = sum(n)) %>% 
  mutate(prop = sum_n/nrow(citations.per.document))


# ---------------------------------------------------------------------#
# Data prep for figure 2
# ---------------------------------------------------------------------#

# I want to focus just on documents, but in this section I make sure I am focusing in on documents without any duplicates

docs <- df %>% 
  select(doc_subject, doc_type, identified.citation, 
         file, doc_owner_agency, doc_owner_agency_level)

docs.unique <- df %>% 
  select(doc_subject, doc_type, identified.citation, 
         file, doc_owner_agency, doc_owner_agency_level) %>% 
  unique()

# There are many duplicate documents because they have both a T and a F for different citations
double <- docs.unique %>% 
  group_by(file) %>% 
  count() %>% 
  filter(n>1) %>% 
  arrange(desc(n))

docs.unique <- docs.unique %>% mutate(duplicate = duplicated(file)) 
table(docs.unique$identified.citation)
table(docs.unique$duplicate)

# I need to get this into a properly unique document dataframe
docs.cit <- docs.unique %>% 
  filter(identified.citation == T) %>% 
  unique() %>% 
  select(-identified.citation, -duplicate) %>% 
  mutate(identified.citation = T)
docs.no.cit <- docs.unique %>% 
  filter(identified.citation == F) %>% 
  unique() %>% 
  select(-identified.citation, - duplicate)

docs.unique <- full_join(docs.cit, docs.no.cit)
docs.unique$identified.citation <- ifelse(is.na(docs.unique$identified.citation), F, T)
table(docs.unique$identified.citation)

# Assigning agency ownership to this dataframe by extracting the file title
docs.unique$agency <- str_extract(docs.unique$file, "(?<=documents\\/).*(?=\\/)")

docs.unique$identified.citation <- factor(docs.unique$identified.citation)
levels(docs.unique$identified.citation) <- c("No", "Yes")

# Checking that still we have the right number here
length(unique(docs.unique$file))

# ---------------------------------------------------------------------#
# Figure 3
# ---------------------------------------------------------------------#

# Subjects of documents

# Summary of documents with citations out of total documents
subject.summary <- docs.unique %>% 
  group_by(doc_subject, identified.citation) %>%
  count() %>% 
  ungroup() %>% 
  group_by(doc_subject) %>% 
  mutate(ndoc_total = sum(n), prop = n/ndoc_total) %>% 
  filter(identified.citation == "Yes") 

# For figure 3 text: what subject was percentage for each level?
subject.summary.by.level <- docs.unique %>% 
  group_by(doc_owner_agency_level, doc_subject) %>%
  count()  %>% 
  ungroup() %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(ndoc_total = sum(n), prop = n/ndoc_total)

# For discussion text
subject.summary.by.level.cit <- docs.unique %>% 
  group_by(doc_owner_agency_level, doc_subject, identified.citation) %>%
  count()  %>% 
  filter(identified.citation == "Yes") %>% 
  ungroup() %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(ndoc_total = sum(n), prop = n/ndoc_total)

# Plot of documents with citations out of total documents
p1 <- plot_counts_aesfill(docs.unique, 
                          y = docs.unique$doc_subject, 
                          fill = docs.unique$identified.citation, 
                          ylab = "", 
                          xlab = "Number of documents", 
                          title = "Documents by subject") + 
  scale_fill_manual(values=c("lightgray", "#808080")) + 
  labs(fill = "Has\ncitation") +
  theme(legend.position = "none")

# Types of documents

# Summary of documents with citations out of total documents
type.summary <- docs.unique %>% 
  group_by(doc_type, identified.citation) %>%
  count() %>% ungroup() %>% 
  group_by(doc_type) %>% 
  mutate(ndoc_total = sum(n), prop = n/ndoc_total) %>% 
  filter(identified.citation == "Yes")

p2 <- plot_counts_aesfill(docs.unique, 
                          y = docs.unique$doc_type, 
                          fill = docs.unique$identified.citation, 
                          ylab = "", 
                          xlab = "Number of documents", 
                          title = "Documents by type") + 
  scale_fill_manual(values=c("lightgray", "#808080")) + 
  labs(fill = "Has\ncitation") +
  theme(legend.position = "none")

# Agency documents

# Summary of documents with citations out of total documents

agency.summary <- docs.unique %>% 
  group_by(doc_owner_agency_level, identified.citation) %>%
  count() %>% ungroup() %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(ndoc_total = sum(n), prop = n/ndoc_total) %>% 
  filter(identified.citation == "Yes")

p3 <- plot_counts_aesfill(docs.unique, 
                          y = factor(docs.unique$doc_owner_agency_level, 
                                     levels = c("State", "Regional", "County")), 
                          fill = docs.unique$identified.citation, 
                          ylab = "", 
                          xlab = "Number of documents", 
                          title = "Documents by agency level") + 
  scale_fill_manual(values=c("lightgray", "#808080")) + 
  scale_x_continuous(breaks=seq(0,2600,650)) +
  labs(fill = "Has\ncitation")

## A look at state types: Research accounts for 95% of documents
docs.unique %>% 
  filter(doc_owner_agency_level == "State") %>% 
  group_by(doc_type) %>%
  count() %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n))

## A look at research x subject
docs.unique %>% 
  filter(doc_type == "Research") %>% 
  group_by(doc_subject, identified.citation) %>%
  count() %>% ungroup() %>% 
  group_by() %>% 
  mutate(ndoc_total = sum(n), prop = n/ndoc_total) %>% 
  filter(identified.citation == "Yes")

# Figure 3 compiled
plot_grid(p1, p2, p3, ncol=3, label_size = 10, label_fontfamily = "Times", labels = c("A", "B", "C"))

percent_docs_w_citations <- ggdraw() + 
  draw_plot(p1, x = 0, y = 0, width = .375, height = 1) + 
  draw_plot_label("A", family = "Times") + 
  draw_plot(p2, x= .375, y=0, width= .325, height = 1) + 
  draw_plot_label("B", x = .375, family = "Times") + 
  draw_plot(p3, x= .7, y=, width= .3, height = 1) +
  draw_plot_label("C", x = .7, family = "Times")

ggsave(filename = "plots/fig3_percent_docs_w_citations.png", plot = percent_docs_w_citations, width = 14, height = 4)

# ---------------------------------------------------------------------#
# Figure 4
# ---------------------------------------------------------------------#

# Will use this dataframe, which is every identified citation
docs.w.cit <- identified.df %>% 
  select(doc_subject, doc_type, file, doc_owner_agency, 
         doc_owner_agency_level, agency_citation)

# Subject citations

# Summary of citations by each subject out of total citations

subject.summary <- docs.w.cit %>% 
  group_by(doc_subject, agency_citation) %>%
  count() %>% ungroup() %>% 
  group_by(doc_subject) %>% 
  mutate(ncit_total = sum(n), prop = n/ncit_total) %>% 
  filter(agency_citation == F)

docs.w.cit$citation_type <- docs.w.cit$agency_citation
table(factor(docs.w.cit$citation_type))
docs.w.cit$citation_type <- factor(docs.w.cit$citation_type, levels = c("TRUE", "FALSE"))
table(docs.w.cit$citation_type)
levels(docs.w.cit$citation_type) <- c("Agency", "Journal")

p4 <- plot_counts_aesfill(docs.w.cit, 
                          y = docs.w.cit$doc_subject, 
                          fill = docs.w.cit$citation_type, 
                          ylab = "", 
                          xlab = "Number of citations",
                          title = "Citations by document subject") + 
  scale_fill_manual(values=c("lightgray", "#808080")) +
  scale_x_continuous(breaks=seq(0,5500,2750)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 1))

# Type citations

# Summary of citations by each subject out of total citations

type.summary <- docs.w.cit %>% 
  group_by(doc_type, agency_citation) %>%
  count() %>% ungroup() %>% 
  group_by(doc_type) %>% 
  mutate(ncit_total = sum(n), prop = n/ncit_total) %>% 
  filter(agency_citation == F)

p5 <- plot_counts_aesfill(docs.w.cit, 
                          y = docs.w.cit$doc_type, 
                          fill = docs.w.cit$citation_type, 
                          ylab = "", 
                          xlab = "Number of citations",
                          title = "Citations by document type") + 
  scale_fill_manual(values=c("lightgray", "#808080")) + 
  scale_x_continuous(breaks=seq(0,10000,5000)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 1))

# Agency citations

# Summary of citations by each subject out of total citations
agency.summary <- docs.w.cit %>% 
  group_by(doc_owner_agency_level, agency_citation) %>%
  count() %>% ungroup() %>% 
  group_by(doc_owner_agency_level) %>% 
  mutate(ncit_total = sum(n), prop = n/ncit_total) %>% 
  filter(agency_citation == F)

p6 <- plot_counts_aesfill(docs.w.cit, 
                          y = docs.w.cit$doc_owner_agency_level,
                          fill = docs.w.cit$citation_type, 
                          ylab = "", 
                          xlab = "Number of citations",
                          title = "Citations by agency level") +
  scale_fill_manual(values=c("lightgray", "#808080")) + 
  scale_x_continuous(breaks=seq(0,10000,5000)) +
  labs(fill = "Citation\ntype") 

# State exploration -- of all the citations, how many did the state account for?
docs.w.cit %>% 
  group_by(doc_owner_agency_level) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n))

# Of all the academic citations, how many did the state account for?
docs.w.cit %>% 
  group_by(doc_owner_agency_level, agency_citation) %>% 
  filter(agency_citation == F) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n))

# How many agencies own documents with citations?
summary <- docs.w.cit %>% 
  group_by(doc_owner_agency) %>%  
  count() %>% 
  mutate(prop = n/sum(n))

# Figure 4 compiled

percent_citations_as_agency <- ggdraw() + 
  draw_plot(p4, x = 0, y = 0, width = .375, height = 1) +
  draw_plot_label("A", family = "Times") + 
  draw_plot(p5, x= .375, y=0, width= .325, height = 1) +
  draw_plot_label("B", x = .375, family = "Times") + 
  draw_plot(p6, x= .7, y=, width= .3, height = 1) +
  draw_plot_label("C", x = .7, family = "Times")

ggsave(filename = "plots/fig4_percent_citations_as_agency.png", plot = percent_citations_as_agency, width = 14, height = 4)

