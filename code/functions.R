# Counting up things
plot_counts_aesfill <- function(df, y, fill,  ylab, xlab, title){
ggplot(df, aes(y = forcats::fct_infreq(y), fill = fill)) + 
    geom_bar() + theme_classic() + 
    labs(y = ylab, x = xlab, title = title) + 
    scale_fill_grey() + 
    theme(text= element_text(size=14, family="Times"), 
          axis.text = element_text(size=14),
          axis.text.x = element_text(size = 10),
          plot.title = element_text(hjust = .5, vjust = 0, size = 16), 
          strip.background  = element_blank())
}

plot_counts_geomfill <- function(df, y, fill,  ylab, xlab, title){
  ggplot(df, aes(y = forcats::fct_infreq(y))) + geom_bar(fill = fill) + theme_classic() + labs(y = ylab, x = xlab, title = title) + scale_fill_brewer() + theme(text= element_text(size=12, family="Times"), plot.title = element_text(hjust = .5, vjust = 0, size = 12), strip.background  = element_blank())
}

# Plotting certain themes
plot_subj_theme <- function(df, subject, plot_title, disagg = T){
doctype <- filter(df, doc_subject == subject)
cit.number <- doctype %>% filter(journalpub_match == T) 
assign("cit.number", cit.number, envir = .GlobalEnv)
doc.number <- cit.number %>% group_by(file) %>% count()
assign("doc.number", doc.number, envir = .GlobalEnv)

doctype.long <- doctype %>% filter(journalpub_match == T) %>% 
  select(cit_journal, cit_theme1:cit_theme5, doc_owner_agency_level, file) %>% 
  pivot_longer(cols = cit_theme1:cit_theme5, names_to = "theme.num", values_to = "Theme") %>% filter(!is.na(Theme))

prop.a.doctype <- doctype.long %>% group_by(doc_owner_agency_level, Theme) %>% count(Theme) %>% rename("theme.count" = "n") %>% ungroup() %>%  group_by(doc_owner_agency_level) %>% mutate(level.total = sum(theme.count)) %>% ungroup() %>% mutate(prop = theme.count/level.total)

prop.doctype <- doctype.long %>% group_by(Theme) %>% count(Theme) %>% rename("theme.count" = "n") %>% ungroup() %>%  mutate(total = sum(theme.count)) %>% ungroup() %>% mutate(prop = theme.count/total)

if(disagg == T){
ggplot(prop.a.doctype) + geom_bar(aes(x = reorder(Theme, -theme.count), y = prop, fill = doc_owner_agency_level), position = "dodge", stat = "identity") + scale_fill_brewer() + labs(x = "Theme", y = "Proportion of agency group's citations", fill = "", title = plot_title) + theme_classic() + theme(text= element_text(size=14, family="Times"), plot.title = element_text(hjust = .5, vjust = 0, size = 14), strip.background  = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 14),  axis.text.x=element_text(angle = -45, hjust = 0))}
else{
ggplot(prop.doctype) + geom_bar(aes(x = reorder(Theme, -theme.count), y = prop), position = "dodge", stat = "identity", fill = "darkgrey") + labs(x = "Theme", y = "Proportion of agency group's citations", fill = "", title = plot_title) + theme_classic() + theme(text= element_text(size=14, family="Times"), plot.title = element_text(hjust = .5, vjust = 0, size = 14), strip.background  = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 14),  axis.text.x=element_text(angle = -45, hjust = 0))}
}

plot_type_theme <- function(df, type, plot_title, disagg = T){
  doctype <- filter(df, doc_type == type)
  cit.number <- doctype %>% filter(journalpub_match == T) 
  assign("cit.number", cit.number, envir = .GlobalEnv)
  doc.number <- cit.number %>% group_by(file) %>% count()
  assign("doc.number", doc.number, envir = .GlobalEnv)
  
  doctype.long <- doctype %>% filter(journalpub_match == T) %>% 
    select(cit_journal, cit_theme1:cit_theme5, doc_owner_agency_level, file) %>% 
    pivot_longer(cols = cit_theme1:cit_theme5, names_to = "theme.num", values_to = "Theme") %>% filter(!is.na(Theme))

  prop.a.doctype <- doctype.long %>% group_by(doc_owner_agency_level, Theme) %>% count(Theme) %>% rename("theme.count" = "n") %>% ungroup() %>%  group_by(doc_owner_agency_level) %>% mutate(level.total = sum(theme.count)) %>% ungroup() %>% mutate(prop = theme.count/level.total)
  
  prop.doctype <- doctype.long %>% group_by(Theme) %>% count(Theme) %>% rename("theme.count" = "n") %>% ungroup() %>%  mutate(total = sum(theme.count)) %>% ungroup() %>% mutate(prop = theme.count/total)

  
  if(disagg == T){
    ggplot(prop.a.doctype) + geom_bar(aes(x = reorder(Theme, -theme.count), y = prop, fill = doc_owner_agency_level), position = "dodge", stat = "identity") + scale_fill_brewer() + labs(x = "Theme", y = "Proportion of agency group's citations", fill = "", title = plot_title) + theme_classic() + theme(text= element_text(size=14, family="Times"), plot.title = element_text(hjust = .5, vjust = 0, size = 14), strip.background  = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 14),  axis.text.x=element_text(angle = -45, hjust = 0))}
  else{
    ggplot(prop.doctype) + geom_bar(aes(x = reorder(Theme, -theme.count), y = prop), position = "dodge", stat = "identity", fill = "darkgrey") + labs(x = "Theme", y = "Proportion of citations", fill = "", title = plot_title) + theme_classic() + theme(text= element_text(size=14, family="Times"), plot.title = element_text(hjust = .5, vjust = 0, size = 14), strip.background  = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 14),  axis.text.x=element_text(angle = -45, hjust = 0))}
}


# Plotting subject categories
plot_subj_cat <- function(df, subject, plot_title, disagg = T, prop.min= 0){
  doctype <- filter(df, doc_subject == subject)
  cit.number <- doctype %>% filter(journalpub_match == T) 
  assign("cit.number", cit.number, envir = .GlobalEnv)
  doc.number <- cit.number %>% group_by(file) %>% count()
  assign("doc.number", doc.number, envir = .GlobalEnv)
  
  doctype.long <- doctype %>% filter(journalpub_match == T) %>% 
    select(cit_journal, cit_cat1:cit_cat11, doc_owner_agency_level, file) %>% 
    pivot_longer(cols = cit_cat1:cit_cat11, names_to = "theme.num", values_to = "Theme") %>% filter(!is.na(Theme))
  
  doctype.long$Theme <- str_remove(doctype.long$Theme, "\\(Q\\d\\)") 
  doctype.long$Theme <- trimws(doctype.long$Theme)
  
  prop.a.doctype <- doctype.long %>% group_by(doc_owner_agency_level, Theme) %>% count(Theme) %>% rename("theme.count" = "n") %>% ungroup() %>%  group_by(doc_owner_agency_level) %>% mutate(level.total = sum(theme.count)) %>% ungroup() %>% mutate(prop = theme.count/level.total) %>% 
    filter(prop > prop.min) 
  
  prop.doctype <- doctype.long %>% group_by(Theme) %>% count(Theme) %>% rename("theme.count" = "n") %>% ungroup() %>%  mutate(total = sum(theme.count)) %>% ungroup() %>% mutate(prop = theme.count/total) %>% 
    filter(prop > prop.min) 
  
  if(disagg == T){
    ggplot(prop.a.doctype) + geom_bar(aes(x = reorder(Theme, -theme.count), y = prop, fill = doc_owner_agency_level), position = "dodge", stat = "identity") + scale_fill_brewer() + labs(x = "Theme", y = "Proportion of agency group's citations", fill = "", title = plot_title) + theme_classic() + theme(text= element_text(size=14, family="Times"), plot.title = element_text(hjust = .5, vjust = 0, size = 14), strip.background  = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 14),  axis.text.x=element_text(angle = -45, hjust = 0))}
  else{
    ggplot(prop.doctype) + geom_bar(aes(x = reorder(Theme, -theme.count), y = prop), position = "dodge", stat = "identity", fill = "darkgrey") + labs(x = "Theme", y = "Proportion of citations", fill = "", title = plot_title) + theme_classic() + theme(text= element_text(size=14, family="Times"), plot.title = element_text(hjust = .5, vjust = 0, size = 14), strip.background  = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 14),  axis.text.x=element_text(angle = -45, hjust = 0))}
}

# Plotting subject journals
plot_subj_journal <- function(df, subject, plot_title, disagg = T, prop.min= 0){
  doctype <- df %>% filter(doc_subject == subject, journalpub_match == T)
  cit.number <- doctype %>% filter(journalpub_match == T) 
  assign("cit.number", cit.number, envir = .GlobalEnv)
  doc.number <- cit.number %>% group_by(file) %>% count()
  assign("doc.number", doc.number, envir = .GlobalEnv)
  
  prop.a.doctype <- doctype %>% group_by(doc_owner_agency_level, cit_journal) %>% count(cit_journal) %>% rename("j.count" = "n") %>% ungroup() %>%  group_by(doc_owner_agency_level) %>% mutate(level.total = sum(j.count)) %>% ungroup() %>% mutate(prop = j.count/level.total) %>% 
    filter(prop > prop.min) 
  
  prop.doctype <- doctype %>% group_by(cit_journal) %>% count(cit_journal) %>% rename("j.count" = "n") %>% ungroup() %>%  mutate(total = sum(j.count)) %>% ungroup() %>% mutate(prop = j.count/total) %>% 
    filter(prop > prop.min) 
  
  if(disagg == T){
    ggplot(prop.a.doctype) + geom_bar(aes(x = reorder(cit_journal, -j.count), y = prop, fill = doc_owner_agency_level), position = "dodge", stat = "identity") + scale_fill_brewer() + labs(x = "Journal", y = "Proportion of agency group's citations", fill = "", title = plot_title) + theme_classic() + theme(text= element_text(size=14, family="Times"), plot.title = element_text(hjust = .5, vjust = 0, size = 14), strip.background  = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 14),  axis.text.x=element_text(angle = -45, hjust = 0))}
  else{
    ggplot(prop.doctype) + geom_bar(aes(x = reorder(cit_journal, -j.count), y = prop), position = "dodge", stat = "identity", fill = "darkgrey") + labs(x = "Journal", y = "Proportion of citations", fill = "", title = plot_title) + theme_classic() + theme(text= element_text(size=14, family="Times"), plot.title = element_text(hjust = .5, vjust = 0, size = 14), strip.background  = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 14),  axis.text.x=element_text(angle = -45, hjust = 0))}
}

plot.level <- function(df, color.n, ylim, title, ylab){
  ggplot(df) + 
    geom_bar(aes(x = reorder(Category, -cat.count.per.level), y = prop), 
             position = "dodge", stat = "identity", fill = wes_palette("Cavalcanti1")[color.n]) +
    labs(x = "Category", y = ylab, fill = "", title = title) + 
    coord_cartesian(ylim = c(0, ylim)) +
    theme_classic() + 
    theme(text= element_text(size=14, family="Times"), 
          plot.title = element_text(hjust = .5, vjust = 0, size = 14), 
          legend.text = element_text(size = 8), legend.title = element_text(size = 8),  
          axis.text.x=element_text(angle = 45, hjust = 1))
}

plot.level.nc <- function(df, ylim, title, ylab, xlab){
  ggplot(df) + 
    geom_bar(aes(x = reorder(Category, -cat.count.per.level), y = prop), 
             position = "dodge", stat = "identity", fill = "#808080") +
    labs(x = xlab, y = ylab, fill = "", title = title) + 
    theme_classic() + 
    theme(text= element_text(size=14, family="Times"), 
          plot.title = element_text(hjust = .5, vjust = 0, size = 14), 
          legend.text = element_text(size = 8), legend.title = element_text(size = 10),  
          axis.text.x=element_text(angle = 45, hjust = 1)) +
    coord_flip(ylim = c(0, ylim)) 
    #coord_cartesian(ylim = c(0, ylim))
}

plot.level.agency <- function(df, ylim, title, ylab, xlab){
  ggplot(df) + 
    geom_bar(aes(x = reorder(cit_agency_author_specific, -agency.count), y = prop), 
             position = "dodge", stat = "identity", fill = "#808080") +
    labs(x = xlab, y = ylab, fill = "", title = title) + 
    theme_classic() + 
    theme(text= element_text(size=14, family="Times"), 
          plot.title = element_text(hjust = .5, vjust = 0, size = 14), 
          legend.text = element_text(size = 8), legend.title = element_text(size = 10)) +  
          #axis.text.x=element_text(angle = 45, hjust = 1)) +
    coord_flip(ylim = c(0, ylim)) 
}

# Function for plotting subject agencies
plot_subj_agency <- function(df, subject, plot_title, n, ylim){
  cit.number <- filter(df, doc_subject == subject)
  assign("cit.number", cit.number, envir = .GlobalEnv)
  doc.number <- cit.number %>% group_by(file) %>% count()
  assign("doc.number", doc.number, envir = .GlobalEnv)
  
  prop.agency <- cit.number %>% 
    filter(!is.na(cit_agency_author_specific)) %>% 
    group_by(cit_agency_author_specific) %>% 
    count(cit_agency_author_specific) %>% rename("agency.count" = "n") %>% 
    ungroup() %>% 
    mutate(level.total = sum(agency.count)) %>% 
    ungroup() %>% 
    mutate(prop = agency.count/level.total) %>% 
    top_n(n)
  
  ggplot(prop.agency, aes(x = reorder(cit_agency_author_specific, -agency.count), y = prop)) + 
    geom_bar(position = "dodge", stat = "identity", fill = "#808080") + 
    labs(x = "Cited agency", y = "Proportion of\ngrey-literature citations", 
         fill = "", title = plot_title) + 
    theme_classic() + 
    theme(text= element_text(size=14, family="Times"), 
          plot.title = element_text(hjust = .5, vjust = 0, size = 14), 
          legend.text = element_text(size = 8), legend.title = element_text(size = 10),  
          axis.text.x=element_text(angle = 45, hjust = 1)) +
    coord_flip(ylim = c(0, ylim)) 
}
# Function for plotting subject journals
plot_subj_journal <- function(df, subject, plot_title, n, ylim){
  doctype <- df %>% filter(doc_subject == subject, journalpub_match == T)
  cit.number <- doctype %>% filter(journalpub_match == T) 
  assign("cit.number", cit.number, envir = .GlobalEnv)
  doc.number <- cit.number %>% group_by(file) %>% count()
  assign("doc.number", doc.number, envir = .GlobalEnv)
  
  prop.doctype <- doctype %>% 
    group_by(cit_journal) %>% 
    count(cit_journal) %>% 
    rename("j.count" = "n") %>% 
    ungroup() %>%  
    mutate(total = sum(j.count)) %>% 
    ungroup() %>% 
    mutate(prop = j.count/total) %>% 
    top_n(n)
  
  ggplot(prop.doctype) + 
    geom_bar(aes(x = reorder(cit_journal, -j.count), y = prop), 
             position = "dodge", stat = "identity", fill = "#808080") + 
    labs(x = "Journal", y = "Proportion of\nacademic citations", fill = "", title = plot_title) + 
    theme_classic() + 
    theme(text= element_text(size=14, family="Times"), 
          plot.title = element_text(hjust = .5, vjust = 0, size = 14), 
          legend.text = element_text(size = 8), legend.title = element_text(size = 10),  
          axis.text.x=element_text(angle = 45, hjust = 1)) +
    coord_flip(ylim = c(0, ylim)) 
}

plot.theme.levels <- function(df, plot_title, ylim, colors, xlab){
  ggplot(df) + 
    geom_bar(aes(x = reorder(reference, -together.prop), y = together.prop, fill = cit_type), 
             position = "dodge", stat = "identity") + 
    scale_fill_manual(values = colors) +
    labs(x = xlab, y = "Proportion of citations", fill = "", title = plot_title) + 
   theme_classic() + 
    theme(text= element_text(size=14, family="Times"), 
          plot.title = element_text(hjust = .5, vjust = 0, size = 14), 
          #legend.position = "none",  
          axis.text.x=element_text(angle = 45, hjust = 1)) +
    coord_flip(ylim = c(0, ylim))
}

