library(tidyverse)
library(cowplot)
library(data.table)
library(RColorBrewer)
library(igraph)
library(ggraph)
source("code/functions.R")

df <- readRDS("data/clean-df.RDS")

# Updating names to make the plot better
df$cit_journal[df$cit_journal == "Transportation Research Part A: Policy and Practice"] <- "Transportation Research Pt. A"
df$cit_journal[df$cit_journal == "Transportation Research Part B: Methodological"] <- "Transportation Research Pt. B"
df$cit_journal[df$cit_journal == "Transportation Research Part C: Emerging Technologies"] <- "Transportation Research Pt. C"
df$cit_journal[df$cit_journal == "Renewable and Sustainable Energy Reviews"] <- "Renewable & Sustainable Energy Review"
df$cit_journal[df$cit_journal == "Transportation\nResearch Record"] <- "Transportation Research Record"
df$cit_journal[df$cit_journal == "Journal of Geotechnical and Geoenvironmental Engineering  ASCE"] <- "J. of Geotech. & Geoenvt. Engin."
df$cit_journal[df$cit_journal == "Journal of the American Planning Association"] <- "J. of the Amer. Planning Assn."
df$cit_journal[df$cit_journal == "Journal of Transportation Engineering"] <- "J. of Transportation Engin."
df$cit_journal[df$cit_journal == "Journal of Structural Engineering"] <- "J. of Structural Engin."
df$cit_journal[df$cit_journal == "American Journal of Public Health"] <- "American J. of Public Health"
df$cit_journal[df$cit_journal == "Accident and Analysis Prevention"] <- "Accident & Analysis Prevention"
df$cit_journal[df$cit_journal == "Cement and Concrete Research"] <- "Cement & Concrete Research"


df$cit_agency_author_specific[df$cit_agency_author_specific == "Butte County Association of Governments"] <- "Butte COG"
df$cit_agency_author_specific[df$cit_agency_author_specific == "Kern Council of Governments"] <- "Kern COG"
df$cit_agency_author_specific[df$cit_agency_author_specific == "Stanislaus Council of Governments"] <- "Stanislaus COG"
df$cit_agency_author_specific[df$cit_agency_author_specific == "Department of Water Resources"] <- "CA DWR"
df$cit_agency_author_specific[df$cit_agency_author_specific == "California Transportation Commission"] <- "CA Transportation Commission"
df$cit_agency_author_specific[df$cit_agency_author_specific == "Southern California Association of Governments"] <- "SCAG"
df$cit_agency_author_specific[df$cit_agency_author_specific == "California Department of Transportation"] <- "Caltrans"
df$cit_agency_author_specific[df$cit_agency_author_specific == "Air Resources Board"] <- "CA ARB"
df$cit_agency_author_specific[df$cit_agency_author_specific == "Census Bureau"] <- "US Census Bureau"
df$cit_agency_author_specific[df$cit_agency_author_specific == "Department of Transportation"] <- "US DOT"
df$cit_agency_author_specific[df$cit_agency_author_specific == "California Department of Education"] <- "CA Dept. of Education"
df$cit_agency_author_specific[df$cit_agency_author_specific == "California Energy Commission"] <- "CA Energy Commission"
df$cit_agency_author_specific [df$cit_agency_author_specific == "Environmental Protection Agency"] <- "US EPA"
df$cit_agency_author_specific [df$cit_agency_author_specific == "Federal Highway Administration"] <- "US FHWA"
df$cit_agency_author_specific [df$cit_agency_author_specific == "Butte County Association of Governments"] <- "Butte COG"
df$cit_agency_author_specific [df$cit_agency_author_specific == "University of California"] <- "University of CA"
df$cit_agency_author_specific [df$cit_agency_author_specific == "Department of Energy"] <- "US Dept. of Energy"
df$cit_agency_author_specific [df$cit_agency_author_specific == "Fish and Wildlife Service"] <- "US FWS"

identified.df <- df %>% filter(identified.citation == T) 
journal.df <- identified.df %>% filter(journalpub_match == T) 
agency.df <- identified.df %>% filter(agency_citation == T) 

# I wanted to see if the state had earlier scientific use, but they don't
cityear.diff.ag <- identified.df %>%
  filter(journalpub_match == T) %>% 
  group_by(doc_owner_agency_level, year(doc_created), cit_year) %>% 
  mutate(lagtime = year(doc_created)-cit_year) %>% 
  group_by(doc_owner_agency_level) %>% 
  summarize(mean = mean(lagtime, na.rm = T), median = median(lagtime, na.rm = T))

agency.counts <- agency.df %>% 
  filter(!is.na(cit_agency_author_specific)) %>% 
  group_by(doc_owner_agency_level, cit_agency_author_specific, cit_agency_author_level) %>% 
  count(cit_agency_author_specific) %>% 
  rename("count" = "n") %>% 
  ungroup() %>% 
  mutate(sum = nrow(identified.df), percent = round(100*count/sum, 2)) %>% 
  rename("reference" = "cit_agency_author_specific",
         "cit_type" = "cit_agency_author_level")

journal.counts <- journal.df %>% 
  group_by(doc_owner_agency_level, cit_journal) %>% 
  count(cit_journal) %>% 
  rename("count" = "n") %>% 
  ungroup() %>% 
  mutate(sum = nrow(identified.df), percent = round(100*count/sum, 2)) %>% 
  rename("reference" = "cit_journal") %>% 
  mutate(cit_type = "journal")

total.counts <- rbind(agency.counts, journal.counts)

state <- total.counts %>% 
  filter(doc_owner_agency_level == "State") %>% 
  mutate(sum = sum(count), percent = round(100*count/sum, 2))

# What percent of state documents have what levels references
state %>% 
  group_by(cit_type) %>% 
  mutate(n = sum(count)) %>% 
  mutate(prop = n/sum) %>% 
  select(cit_type, n, sum, prop) %>% 
  unique() %>% 
  arrange(-n)

region <- total.counts %>% 
  filter(doc_owner_agency_level == "Regional") %>% 
  mutate(sum = sum(count), percent = round(100*count/sum, 2))

# What percent of region documents have what levels references
region %>% 
  group_by(cit_type) %>% 
  mutate(n = sum(count)) %>% 
  mutate(prop = n/sum) %>% 
  select(cit_type, n, sum, prop) %>% 
  unique() %>% 
  arrange(-n)

county <- total.counts %>% 
  filter(doc_owner_agency_level == "County") %>% 
  mutate(sum = sum(count), percent = round(100*count/sum, 2))

# What percent of county documents have what levels references
county %>% 
  group_by(cit_type) %>% 
  mutate(n = sum(count)) %>% 
  mutate(prop = n/sum) %>% 
  select(cit_type, n, sum, prop) %>% 
  unique() %>% 
  arrange(-n)

statetop <- state %>%
  top_n(15) %>% 
  arrange(-percent)

regiontop <- region %>% 
  top_n(15) %>% 
  arrange(-percent)

countytop <- county %>% 
  top_n(15) %>% 
  arrange(-percent)

grey <- brewer.pal(8, "Greys")

## Categories as a proportion of each agency's total citation, plotted using function in functions.R
# For colors
table(statetop$cit_type) # federal, journal, state
table(regiontop$cit_type) # federal, journal, regional, state
table(countytop$cit_type) # federal, regional, state

st <- plot.theme.levels(statetop, "State", 
                         colors = c(grey[6], grey[3], grey[5]), xlab = "") + 
  theme(legend.position = "none") +
  scale_y_continuous(breaks=seq(0,10,5))
rg <- plot.theme.levels(regiontop, "Regional", 
                         colors = c(grey[6], grey[3], grey[4], grey[5]), xlab = "") + 
  theme(legend.position = "none") +
  scale_y_continuous(breaks=seq(0,16,8))
co <- plot.theme.levels(countytop, "County", 
                         colors = c(grey[6], grey[4], grey[5]), xlab = "") + 
  theme(legend.position = "none") +
  scale_y_continuous(breaks=seq(0,8,4))

plot_grid(st, rg, co, ncol=3, label_size = 14, label_fontfamily = "Times", labels = c("A", "B", "C"))

ggsave(filename = "plots/fig6_combined_citations_by_level.png", width = 13, height = 5)


