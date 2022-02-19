library(tidyverse)
library(cowplot)
library(data.table)
library(RColorBrewer)
library(igraph)
library(ggraph)

df <- readRDS("data/clean-df.RDS")

df <- df %>% 
  filter(doc_subject == "Intelligent Transportation Systems" | 
           doc_subject == "Climate, Envt. & Sust.")

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

edge.list$doc_owner_agency[edge.list$doc_owner_agency == "Caltrans"] <- "California Department of Transportation"

edge.list.w <- edge.list %>% 
  group_by(doc_owner_agency, reference) %>% count()
table(edge.list.w$n)
edge.list.w <- filter(edge.list.w, n > 2)

nodes.doc.owners <- identified.df %>% 
  select(doc_owner_agency, doc_owner_agency_level) %>% 
  unique() %>% 
  mutate("sjr" = NA)
colnames(nodes.doc.owners) <- c("name", "agency_level", "sjr")

nodes.doc.owners$agency_level[nodes.doc.owners$name == "Caltrans"] <- "State (CA)"
nodes.doc.owners$name[nodes.doc.owners$name == "Caltrans"] <- "California Department of Transportation"

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
node.list <- distinct(node.list)

net <- graph_from_data_frame(d = edge.list.w, vertices = node.list, directed = T) 
clr <- RColorBrewer::brewer.pal(n = 8, name = "Set2")
grey <- brewer.pal(8, "Greys")

table(V(net)$agency_level)
V(net)$color <- ifelse(V(net)$agency_level == "City", grey[3],
                ifelse(V(net)$agency_level == "County", grey[3],
                ifelse(V(net)$agency_level == "First Nations", grey[3],
                ifelse(V(net)$agency_level == "Regional", grey[4],
                ifelse(V(net)$agency_level == "State (CA)", grey[5],
                ifelse(V(net)$agency_level == "State (Other)", grey[5],
                ifelse(V(net)$agency_level == "Federal", grey[6],
                ifelse(is.na(V(net)$agency_level), clr[5], clr[5]))))))))
V(net)$color[is.na(V(net)$agency_level)] <- "blue"


V(net)$indeg <- log(igraph::degree(net, mode="in")) +.5
table(V(net)$indeg)
V(net)$outdeg <- log(igraph::degree(net, mode="out")) +.5
table(V(net)$outdeg)

V(net)$inlabel <- unname(ifelse(V(net)$indeg > 1.8, names(V(net)), "")) 
V(net)$outlabel <- unname(ifelse(V(net)$outdeg > 2.2, names(V(net)), "")) 
V(net)$label.color = "black"
V(net)$inlabel.size = ifelse(V(net)$indeg > 1.8, V(net)$indeg, 0)
V(net)$outlabel.size = ifelse(V(net)$outdeg > 2.2, V(net)$outdeg, 0)


# Before choosing those with n >1, it was all one component, now I zero in on just the main component
V(net)$comp <- igraph::components(net)$membership
table(V(net)$comp)
main <- induced_subgraph(net, V(net)$comp == 1)
V(main)$core <- coreness(main)
table(V(main)$core)
main <- induced_subgraph(main, V(main)$core %in% c(3:4))

# In degree: who is getting cited?
ggraph(main, layout = "lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(main)$color, size= V(main)$indeg) + 
  geom_node_text(aes(label = V(main)$inlabel), size = V(main)$inlabel.size, 
                 color=V(main)$label.color, repel=T) +
  theme_void() 

# Out degree: who is citing things
ggraph(main, layout="lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(main)$color, size= V(main)$outdeg) + 
  geom_node_text(aes(label = V(main)$outlabel), size = V(main)$outlabel.size, 
                 color=V(main)$label.color, repel=T) +
  theme_void()
