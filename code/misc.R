
## PUBLCATION YEAR, ALL REFERENCES ----

# Taking a look, and it seems like the first legit identified reference is from 1918. Before that, NA
table(is.na(identified.df$cit_year))
identified.df$cit_year <- ifelse(identified.df$cit_year %in% c(1904,1912,1917), NA, identified.df$cit_year)

# Years
summary(identified.df$cit_year)

cityear.count <- identified.df %>% 
  group_by(cit_year) %>% count() 

cityear.count.perdoc <- identified.df %>% 
  group_by(year(doc_created), cit_year) %>% 
  count() %>% 
  mutate(diff = `year(doc_created)` - cit_year)

# Range of dates cited per document year
cityear.count.perdoc %>% 
  #filter(cit_year > 1950) %>%   
  ggplot(aes(x = cit_year, y = n)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~`year(doc_created)`, scales = "free")

# Differential -- mean lag time is 13 years
cityear.diff <- identified.df %>% 
  group_by(year(doc_created), cit_year) %>% 
  mutate(lagtime = year(doc_created)-cit_year) %>% 
  ungroup() %>% 
  summarize(mean = mean(lagtime, na.rm = T), median = median(lagtime, na.rm = T))

cityear.diff.ag <- identified.df %>%
  group_by(doc_owner_agency_level, year(doc_created), cit_year) %>% 
  mutate(lagtime = year(doc_created)-cit_year) %>% 
  group_by(doc_owner_agency_level) %>% 
  summarize(mean = mean(lagtime, na.rm = T), median = median(lagtime, na.rm = T))

cityear.count.perdoc %>% filter(diff>-1) %>%   
  ggplot(aes(x = diff, y = n)) + 
  geom_bar(stat = "identity")  +
  labs(x = "Year of document - Year of citation",
       y = "Number of citations") + # ,title = "Citation lag time") +
  theme_classic() + 
  theme(text= element_text(size=14, family="Times"), 
        axis.text = element_text(hjust = .6, size=14),
        plot.title = element_text(hjust = .5, vjust = 0, size = 16), 
        strip.background  = element_blank())

ggsave(filename = "plots/citation_lag.png", width = 4, height = 3)
