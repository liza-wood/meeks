library(tidyverse)
library(cowplot)
library(data.table)
library(RColorBrewer)
library(igraph)
library(ggraph)

df <- readRDS("data/clean-df.RDS")

df <- df %>% 
  filter(doc_subject == "Intelligent Transportation Systems" | 
           doc_subject == "Climate & Sustainability")

df$cit_agency_author_specific[df$cit_agency_author_specific == "California Department of Transportation"] <- "Caltrans"
df$cit_agency_author_specific[df$cit_agency_author_specific == "Air Resources Board"] <- "CA ARB"
df$cit_agency_author_specific[df$cit_agency_author_specific == "Census Bureau"] <- "US Census Bureau"
df$cit_agency_author_specific[df$cit_agency_author_specific == "Department of Transportation"] <- "US DOT"
df$cit_agency_author_specific[df$cit_agency_author_specific == "California Department of Education"] <- "CA Dept. of Education"
df$cit_agency_author_specific[df$cit_agency_author_specific == "California Energy Commission"] <- "CA Energy Commission"
df$cit_agency_author_specific[df$cit_agency_author_specific == "California Transportation Commission"] <- "CA Transportation Commission"
df$cit_agency_author_specific [df$cit_agency_author_specific == "Environmental Protection Agency"] <- "US EPA"
df$cit_agency_author_specific [df$cit_agency_author_specific == "Federal Highway Administration"] <- "US FHWA"
df$cit_agency_author_specific [df$cit_agency_author_specific == "University of California"] <- "University of CA"
df$cit_agency_author_specific [df$cit_agency_author_specific == "Department of Energy"] <- "US Dept. of Energy"
df$cit_agency_author_specific [df$cit_agency_author_specific == "Geological Survey"] <- "US GS"

df$cit_journal[df$cit_journal == "Transportation Research Part A: Policy and Practice"] <- "Transportation Research Pt. A"
df$cit_journal[df$cit_journal == "Journal of Geotechnical and Geoenvironmental Engineering  ASCE"] <- "J. of Geotech. & Geoenvt. Engin."
df$cit_journal[df$cit_journal == "Marine Ecology  Progress Series"] <- "Marine Ecology"


identified.df <- df %>% filter(identified.citation == T) 
journal.df <- identified.df %>% filter(journalpub_match == T) 
agency.df <- identified.df %>% filter(agency_citation == T) 

edge.list <- identified.df %>% 
  mutate(reference = case_when(
    agency_citation == T ~ cit_agency_author_specific,
    journalpub_match == T ~ cit_journal,
    T ~ "error"
  )) %>% 
  select(doc_owner_agency, reference)

edge.list$reference[edge.list$reference == "California Department of Transportation"] <- "Caltrans"
edge.list$doc_owner_agency[edge.list$doc_owner_agency == "California Department of Transportation"] <- "Caltrans"

edge.list.w <- edge.list %>% 
  group_by(doc_owner_agency, reference) %>% count()
table(edge.list.w$n)
edge.list.w <- filter(edge.list.w, n ==1 ) %>% select(-n)
edge.list <- edge.list %>% anti_join(edge.list.w)

nodes.doc.owners <- identified.df %>% 
  select(doc_owner_agency, doc_owner_agency_level) %>% 
  unique() %>% 
  mutate("sjr" = NA)
colnames(nodes.doc.owners) <- c("name", "agency_level", "sjr")

nodes.agency.cits <- agency.df %>% 
  filter(!is.na(cit_agency_author_specific)) %>% 
  select(cit_agency_author_specific, cit_agency_author_level) %>% 
  unique() %>% 
  mutate("sjr" = NA)
colnames(nodes.agency.cits) <- c("name", "agency_level", "sjr")

nodes.journal.cits <- journal.df %>% 
  select(cit_journal, cit_sjr) %>% 
  unique() %>% 
  rename("name" = "cit_journal", "sjr" = "cit_sjr") %>% 
  mutate("agency_level" = NA) %>% 
  select(name, agency_level, sjr)
colnames(nodes.journal.cits) <- c("name", "agency_level", "sjr")


node.list <- rbind(nodes.doc.owners, nodes.agency.cits, nodes.journal.cits)

node.list$name[node.list$name == "California Department of Transportation"] <- "Caltrans"
node.list$agency_level[node.list$name == "Caltrans"] <- "State (CA)"

node.list <- distinct(node.list)

net <- graph_from_data_frame(d = edge.list, vertices = node.list, directed = T) 

clr <- RColorBrewer::brewer.pal(n = 8, name = "Set2")
grey <- brewer.pal(8, "Greys")

table(V(net)$agency_level)
V(net)$color <- ifelse(V(net)$agency_level == "City", grey[3],
                ifelse(V(net)$agency_level == "County", grey[3],
                ifelse(V(net)$agency_level == "First Nations", grey[3],
                ifelse(V(net)$agency_level == "Regional", grey[4],
                ifelse(V(net)$agency_level == "State (CA)", grey[5],
                ifelse(V(net)$agency_level == "State (Other)", grey[5],
                ifelse(V(net)$agency_level == "Federal", grey[7], clr[5]))))))) # why doesnt this work
V(net)$color[is.na(V(net)$agency_level)] <- "blue"


V(net)$indeg <- log(igraph::degree(net, mode="in")) +.5
table(V(net)$indeg)

maxdeg <- length(names(table(V(net)$indeg)))

V(net)$inlabel <- unname(ifelse(V(net)$indeg > 
                                  as.numeric(names(table(V(net)$indeg))[maxdeg-8]), 
                                names(V(net)), "")) 
V(net)$label.color = "black"
V(net)$inlabel.size = ifelse(V(net)$indeg > 
                               as.numeric(names(table(V(net)$indeg))[maxdeg-8]), 
                             V(net)$indeg/2, 0)

# Before choosing those with n >1, it was all one component, now I zero in on just the main component
V(net)$comp <- igraph::components(net)$membership
table(V(net)$comp)
maxcomp <- names(table(V(net)$comp))[table(V(net)$comp) == max(table(V(net)$comp))]
main <- induced_subgraph(net, V(net)$comp == as.numeric(maxcomp))
V(main)$core <- coreness(main)
table(V(main)$core)
#sum(table(V(main)$core)[-c(1:3)])/sum(table(V(main)$core)) # the inner 50%
#maincore <- induced_subgraph(main, V(main)$core %in% c(3:81))
#table(components(maincore)$membership)

# In degree: who is getting cited?
ggraph(main, layout = "lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(main)$color, size= V(main)$indeg) + 
  geom_node_text(aes(label = V(main)$inlabel), size = V(main)$inlabel.size, 
                 color=V(main)$label.color, repel=T) +
  theme_void() 

# What about adding an agency facet -- I don't think this makes a whole lot of sense

# --------------------------------------------------------------
# By agency level
# --------------------------------------------------------------

# I would like to be able to subset these by only certain level document owners
## To reduce copy and paste I am going to re-write over my own code for now with each level

state.edge.list <- identified.df %>% 
  filter(doc_owner_agency_level == "State") %>% 
  mutate(reference = case_when(
    agency_citation == T ~ cit_agency_author_specific,
    journalpub_match == T ~ cit_journal,
    T ~ "error"
  )) %>% 
  select(doc_owner_agency, reference)

state.edge.list$doc_owner_agency[state.edge.list$doc_owner_agency == "California Department of Transportation"] <- "Caltrans"
state.edge.list$reference[state.edge.list$dreference == "California Department of Transportation"] <- "Caltrans"

state.edge.list.w <- state.edge.list %>% 
  group_by(doc_owner_agency, reference) %>% count()
state.edge.list.w <- filter(state.edge.list.w, n ==1 ) %>% select(-n)
state.edge.list <- state.edge.list %>% anti_join(state.edge.list.w)

state.net <- graph_from_data_frame(d = state.edge.list, vertices = node.list, directed = T) 

table(V(state.net)$agency_level)
V(state.net)$color <- ifelse(V(state.net)$agency_level == "City", grey[3],
                      ifelse(V(state.net)$agency_level == "County", grey[3],
                      ifelse(V(state.net)$agency_level == "First Nations", grey[3],
                      ifelse(V(state.net)$agency_level == "Regional", grey[4],
                      ifelse(V(state.net)$agency_level == "State (CA)", grey[5],
                      ifelse(V(state.net)$agency_level == "State (Other)", grey[5],
                      ifelse(V(state.net)$agency_level == "Federal", grey[7],clr[5]))))))) # why doesnt this work
V(state.net)$color[is.na(V(state.net)$agency_level)] <- "blue"

V(state.net)$indeg <- log(igraph::degree(state.net, mode="in")) +.5
table(V(state.net)$indeg)
maxdeg <- length(names(table(V(state.net)$indeg)))

V(state.net)$inlabel <- unname(ifelse(V(state.net)$indeg > 
                                        as.numeric(names(table(V(state.net)$indeg))[maxdeg-4]), 
                                      names(V(state.net)), "")) 
V(state.net)$label.color = "black"
V(state.net)$inlabel.size = ifelse(V(state.net)$indeg > 
                                     as.numeric(names(table(V(state.net)$indeg))[maxdeg-4]), 
                                   V(state.net)$indeg/2, 0)

# Before choosing those with n >1, it was all one component, now I zero in on just the state.main component
V(state.net)$comp <- igraph::components(state.net)$membership
table(V(state.net)$comp)
maxcomp <- names(table(V(state.net)$comp))[table(V(state.net)$comp) == max(table(V(state.net)$comp))]
state.main <- induced_subgraph(state.net, V(state.net)$comp == as.numeric(maxcomp))
V(state.main)$core <- coreness(state.main)
table(V(state.main)$core)
# No need to cut back to a core
state.main.core <- state.main

# In degree: who is getting cited?
state <- ggraph(state.main.core, layout = "lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(state.main.core)$color, size= V(state.main.core)$indeg, alpha = .7) + 
  geom_node_text(aes(label = V(state.main.core)$inlabel), size = V(state.main.core)$inlabel.size, 
                 color=V(state.main.core)$label.color, repel=T) +
  theme_void() ; state


# REGIONAL ----

region.edge.list <- identified.df %>% 
  filter(doc_owner_agency_level == "Regional") %>% 
  mutate(reference = case_when(
    agency_citation == T ~ cit_agency_author_specific,
    journalpub_match == T ~ cit_journal,
    T ~ "error"
  )) %>% 
  select(doc_owner_agency, reference)

region.edge.list$doc_owner_agency[region.edge.list$doc_owner_agency == "California Department of Transportation"] <- "Caltrans"
region.edge.list$reference[region.edge.list$dreference == "California Department of Transportation"] <- "Caltrans"

region.edge.list.w <- region.edge.list %>% 
  group_by(doc_owner_agency, reference) %>% count()
region.edge.list.w <- filter(region.edge.list.w, n ==1 ) %>% select(-n)
region.edge.list <- region.edge.list %>% anti_join(region.edge.list.w)

region.net <- graph_from_data_frame(d = region.edge.list, vertices = node.list, directed = T) 

table(V(region.net)$agency_level)
V(region.net)$color <- ifelse(V(region.net)$agency_level == "City", grey[3],
                       ifelse(V(region.net)$agency_level == "County", grey[3],
                       ifelse(V(region.net)$agency_level == "First Nations", grey[3],
                       ifelse(V(region.net)$agency_level == "Regional", grey[4],
                       ifelse(V(region.net)$agency_level == "State (CA)", grey[5],
                       ifelse(V(region.net)$agency_level == "State (Other)", grey[5],
                       ifelse(V(region.net)$agency_level == "Federal", grey[7],clr[5]))))))) # why doesnt this work
V(region.net)$color[is.na(V(region.net)$agency_level)] <- "blue"

V(region.net)$indeg <- log(igraph::degree(region.net, mode="in")) +.5
table(V(region.net)$indeg)
maxdeg <- length(names(table(V(region.net)$indeg)))

V(region.net)$inlabel <- unname(ifelse(V(region.net)$indeg > 
                                         as.numeric(names(table(V(region.net)$indeg))[maxdeg-7]), 
                                       names(V(region.net)), "")) 
V(region.net)$label.color = "black"
V(region.net)$inlabel.size = ifelse(V(region.net)$indeg > 
                                      as.numeric(names(table(V(region.net)$indeg))[maxdeg-7]), 
                                    V(region.net)$indeg/2, 0)

# Before choosing those with n >1, it was all one component, now I zero in on just the region.main component
V(region.net)$comp <- igraph::components(region.net)$membership
table(V(region.net)$comp)
maxcomp <- names(table(V(region.net)$comp))[table(V(region.net)$comp) == max(table(V(region.net)$comp))]
region.main <- induced_subgraph(region.net, V(region.net)$comp == as.numeric(maxcomp))
V(region.main)$core <- coreness(region.main)
table(V(region.main)$core)
# don't need to cit back
region.main.core <- region.main

# In degree: who is getting cited?
region <- ggraph(region.main.core, layout = "lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(region.main.core)$color, size= V(region.main.core)$indeg, alpha = .7) + 
  geom_node_text(aes(label = V(region.main.core)$inlabel), size = V(region.main.core)$inlabel.size, 
                 color=V(region.main.core)$label.color, repel=T) +
  theme_void() ; region


# COUNTY ----

county.edge.list <- identified.df %>% 
  filter(doc_owner_agency_level == "County") %>% 
  mutate(reference = case_when(
    agency_citation == T ~ cit_agency_author_specific,
    journalpub_match == T ~ cit_journal,
    T ~ "error"
  )) %>% 
  select(doc_owner_agency, reference)

county.edge.list$doc_owner_agency[county.edge.list$doc_owner_agency == "California Department of Transportation"] <- "Caltrans"
county.edge.list$reference[county.edge.list$dreference == "California Department of Transportation"] <- "Caltrans"

county.edge.list.w <- county.edge.list %>% 
  group_by(doc_owner_agency, reference) %>% count()
county.edge.list.w <- filter(county.edge.list.w, n ==1 ) %>% select(-n)
county.edge.list <- county.edge.list %>% anti_join(county.edge.list.w)

county.net <- graph_from_data_frame(d = county.edge.list, vertices = node.list, directed = T) 

table(V(county.net)$agency_level)
V(county.net)$color <- ifelse(V(county.net)$agency_level == "City", grey[3],
                       ifelse(V(county.net)$agency_level == "County", grey[3],
                       ifelse(V(county.net)$agency_level == "First Nations", grey[3],
                       ifelse(V(county.net)$agency_level == "Regional", grey[4],
                       ifelse(V(county.net)$agency_level == "State (CA)", grey[5],
                       ifelse(V(county.net)$agency_level == "State (Other)", grey[5],
                       ifelse(V(county.net)$agency_level == "Federal", grey[7],clr[5]))))))) # why doesnt this work
V(county.net)$color[is.na(V(county.net)$agency_level)] <- "blue"

V(county.net)$indeg <- log(igraph::degree(county.net, mode="in")) +.5
table(V(county.net)$indeg)
maxdeg <- length(names(table(V(county.net)$indeg)))

V(county.net)$inlabel <- unname(ifelse(V(county.net)$indeg > 
                                         as.numeric(names(table(V(county.net)$indeg))[maxdeg-7]), 
                                       names(V(county.net)), "")) 
V(county.net)$label.color = "black"
V(county.net)$inlabel.size = ifelse(V(county.net)$indeg > 
                                      as.numeric(names(table(V(county.net)$indeg))[maxdeg-7]), 
                                    V(county.net)$indeg/2, 0)

# Before choosing those with n >1, it was all one component, now I zero in on just the county.main component
V(county.net)$comp <- igraph::components(county.net)$membership
table(V(county.net)$comp)
maxcomp <- names(table(V(county.net)$comp))[table(V(county.net)$comp) == max(table(V(county.net)$comp))]
county.main <- induced_subgraph(county.net, V(county.net)$comp == as.numeric(maxcomp))
V(county.main)$core <- coreness(county.main)
table(V(county.main)$core)
county.main.core <- county.main

# In degree: who is getting cited?
county <- ggraph(county.main.core, layout = "lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(county.main.core)$color, size= V(county.main.core)$indeg, alpha = .7) + 
  geom_node_text(aes(label = V(county.main.core)$inlabel), size = V(county.main.core)$inlabel.size, 
                 color=V(county.main.core)$label.color, repel=T) +
  theme_void() ; county



plot_grid(state, region, county, nrow = 1)
ggsave(filename = "plots/alt-fig6_networks-emerging.png", width = 10, height = 3)
