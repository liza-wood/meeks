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

region <- total.counts %>% 
  filter(doc_owner_agency_level == "Regional") %>% 
  mutate(sum = sum(count), percent = round(100*count/sum, 2))

county <- total.counts %>% 
  filter(doc_owner_agency_level == "County") %>% 
  mutate(sum = sum(count), percent = round(100*count/sum, 2))

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


# NETWORK ----

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

table(V(net)$agency_level)
V(net)$color <- ifelse(V(net)$agency_level == "City", clr[1],
                ifelse(V(net)$agency_level == "County", clr[2],
                ifelse(V(net)$agency_level == "First Nations", clr[3],
                ifelse(V(net)$agency_level == "Regional", clr[4],
                ifelse(V(net)$agency_level == "State (CA)", clr[5],
                ifelse(V(net)$agency_level == "State (Other)", clr[5],
                ifelse(V(net)$agency_level == "Federal", clr[6],
                ifelse(is.na(V(net)$agency_level), NA, clr[8]))))))))


V(net)$indeg <- log(igraph::degree(net, mode="in")) +.5
table(V(net)$indeg)
V(net)$outdeg <- log(igraph::degree(net, mode="out")) +.5
table(V(net)$outdeg)
V(net)$alldeg <- log(igraph::degree(net, mode="all")) +.5
table(V(net)$alldeg)

V(net)$inlabel <- unname(ifelse(V(net)$indeg > 3.5, names(V(net)), "")) 
V(net)$outlabel <- unname(ifelse(V(net)$outdeg > 3.9, names(V(net)), "")) 
V(net)$alllabel <- unname(ifelse(V(net)$alldeg > 4, names(V(net)), "")) 
V(net)$label.color = "black"
V(net)$label.size = 2


# Before choosing those with n >1, it was all one component, now I zero in on just the main component
V(net)$comp <- igraph::components(net)$membership
table(V(net)$comp)
main <- induced_subgraph(net, V(net)$comp == 1)

# In degree: who is getting cited?
ggraph(main, layout="lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(main)$color, size= V(main)$indeg) + 
  geom_node_text(aes(label = V(main)$inlabel), size = V(main)$label.size, 
                 color=V(main)$label.color, repel=T) +
  theme_void() 

# Out degree: who is citing things
ggraph(main, layout="lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(main)$color, size= V(main)$outdeg) + 
  geom_node_text(aes(label = V(main)$outlabel), size = V(main)$label.size, 
                 color=V(main)$label.color, repel=T) +
  theme_void() 




edge.list <- agency.df %>% filter(!is.na(cit_agency_author_specific)) %>% select(doc_owner_agency, cit_agency_author_specific) 
edge.list.w <- edge.list %>% 
  group_by(doc_owner_agency, cit_agency_author_specific) %>% count()
colnames(edge.list.w) <- c("doc_agency", "cit_agency", "weight")

edge.list$doc_owner_agency[edge.list$doc_owner_agency == "Caltrans"] <- "California Department of Transportation"

node.list1 <- agency.df %>% 
  filter(!is.na(cit_agency_author_specific)) %>% 
  select(doc_owner_agency, doc_owner_agency_level, doc_owner_agency_type) %>% 
  unique()
colnames(node.list1) <- c("agency", "agency_level", "agency_type")

node.list2 <- agency.df %>% 
  filter(!is.na(cit_agency_author_specific)) %>% 
  select(cit_agency_author_specific, cit_agency_author_level) %>% 
  unique()
colnames(node.list2) <- c("agency", "agency_level")
node.list2$agency_type <- NA

node.list <- rbind(node.list1, node.list2)
node.list <- distinct(node.list)

node.list <- node.list %>% filter(duplicated(node.list$agency) == F)
table(node.list$agency_level)

# not changing the name bc i need to make sure edge list and node list match
#node.list$agency_level[node.list$agency_level == "State"] <- "State (CA)"
#node.list$agency[node.list$agency == "Caltrans"] <- "California Department of #Transportation"
#node.list$agency_type[node.list$agency == "California Department of Transportation"] <- NA

node.list <- distinct(node.list)

# igraph ----
library(igraph)
net <- graph_from_data_frame(d = edge.list.w, vertices = node.list, directed = T) 


# Edge attributes
E(net)$weight 
# Set edge width based on weight:
E(net)$width <- E(net)$weight/6
E(net)$edge.color <- "gray80"

table(V(net)$agency_level)

V(net)$color <- ifelse(V(net)$agency_level == "state", "darkgoldenrod",
                       ifelse(V(net)$agency_level == "county", "darkred",
                              ifelse(V(net)$agency_level == "federal", "darkblue",
                                     ifelse(V(net)$agency_level == "regional", "darkgreen",
                                            ifelse(is.na(V(net)$agency_level), "gray50", NA)))))

# Vertex attributes
deg <- degree(net, mode="out")
V(net)$size <- log(deg) + .5

# Labels
# The labels are currently node IDs, Setting them to NA will render no labels:
#V(net)$label <- NA
# Or set only some labels
V(net)$agency_abb
V(net)$label <- unname(ifelse(degree(net, mode = "in")[V(net)] > 40, names(V(net)), "")) 
V(net)$label.color="black"

# Arrows
E(net)$arrow.size <- .02

# Plotting in igraph
net <- simplify(net, remove.multiple = F, remove.loops = T) 
plot(net)

delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0)
  delete.vertices(graph, isolates)
}

net2 <- delete.isolates(net)
plot(net2)

# NOTE TO SELF -- THIS SHOULD BE WEIGHTED MAYBE SO THAT IT IS RELATIVE TO THE NUMBER OF DOCUMENTS THEY HAVE. CAN I DO THAT? ONE SOLUTION IS JUST TO FOCUS ON IN DEGREE

# ggraph ----
library(ggraph)
library(ggnetwork)
library(tidygraph)

#Can graph igrpah object
net <- graph_from_data_frame(d = edge.list, vertices = node.list, directed = T) 
#net2 <- delete.isolates(net) where did this function come from/
#net2 <- igraph::delete.vertices(net , which(degree(net)==0))

clr <- RColorBrewer::brewer.pal(n = 8, name = "Set2")
RColorBrewer::display.brewer.pal(n = 8, name = "Set2")


V(net)$color <- ifelse(V(net)$agency_level == "City", clr[1],
                       ifelse(V(net)$agency_level == "County", clr[2],
                              ifelse(V(net)$agency_level == "First Nations", clr[3],
                                     ifelse(V(net)$agency_level == "Regional", clr[4],
                                            ifelse(V(net)$agency_level == "State (CA)", clr[5],
                                                   ifelse(V(net)$agency_level == "State (Other)", clr[5],
                                                          ifelse(V(net)$agency_level == "Federal", clr[6],
                                                                 ifelse(is.na(V(net)$agency_level), NA, clr[8]))))))))
deg <- igraph::degree(net, mode="in")
V(net)$size <- log(deg) +.5
table(deg)
V(net)$label <- unname(ifelse(igraph::degree(net, mode = "in")[V(net)] > 30, names(V(net)), "")) 
V(net)$label.color = "black"
V(net)$label.size = 3


ggraph(net, layout="lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(net)$color, size= V(net)$size) + 
  geom_node_text(aes(label = V(net)$label), size = V(net)$label.size, color=V(net)$label.color, repel=T) +
  facet_wrap(~agency_level) +
  #facet_nodes(~agency_level) +
  theme_void()

ggraph(net, layout="lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(net)$color, size= V(net)$size) + 
  geom_node_text(aes(label = V(net)$label), size = V(net)$label.size, color=V(net)$label.color, repel=T) +
  #facet_wrap(~agency_level) +
  #facet_nodes(~agency_level) +
  theme_void()

# I would like to be able to subset these by only certain level document owners

# STATE NODE LIST ----
state.edge.list <- agency.df %>% filter(!is.na(cit_agency_author_specific) & doc_owner_agency_level == "State") %>% select(doc_owner_agency, cit_agency_author_specific)
state.edge.list.w <- edge.list %>% group_by(doc_owner_agency, cit_agency_author_specific) %>% count()
colnames(state.edge.list.w) <- c("doc_agency", "cit_agency", "weight")

state.edge.list$doc_owner_agency[state.edge.list$doc_owner_agency == "Caltrans"] <- "California Department of Transportation"

state.node.list1 <- agency.df %>% filter(doc_owner_agency_level == "State") %>% select(doc_owner_agency, doc_owner_agency_level, doc_owner_agency_type)
colnames(state.node.list1) <- c("agency", "agency_level", "agency_type")
state.node.list2 <- agency.df %>% filter(!is.na(cit_agency_author_specific) & doc_owner_agency_level == "State") %>% select(cit_agency_author_specific, cit_agency_author_level)

colnames(state.node.list2) <- c("agency", "agency_level")
state.node.list2$agency_type <- NA
state.node.list <- rbind(state.node.list1, state.node.list2)
state.node.list <- distinct(state.node.list)

state.node.list <- state.node.list %>% filter(duplicated(state.node.list$agency) == F)
table(state.node.list$agency_level)

state.node.list$agency_level[state.node.list$agency_level == "State"] <- "State (CA)"
state.node.list$agency[state.node.list$agency == "Caltrans"] <- "California Department of Transportation"
state.node.list$agency_type[state.node.list$agency == "California Department of Transportation"] <- NA

state.node.list <- distinct(state.node.list)

# REGIONAL NODE LIST ----
reg.edge.list <- agency.df %>% filter(!is.na(cit_agency_author_specific) & doc_owner_agency_level == "Regional") %>% select(doc_owner_agency, cit_agency_author_specific)
reg.edge.list.w <- edge.list %>% group_by(doc_owner_agency, cit_agency_author_specific) %>% count()
colnames(reg.edge.list.w) <- c("doc_agency", "cit_agency", "weight")

reg.edge.list$doc_owner_agency[reg.edge.list$doc_owner_agency == "Caltrans"] <- "California Department of Transportation"

reg.node.list1 <- agency.df %>% filter(doc_owner_agency_level == "Regional") %>% select(doc_owner_agency, doc_owner_agency_level, doc_owner_agency_type)
colnames(reg.node.list1) <- c("agency", "agency_level", "agency_type")
reg.node.list2 <- agency.df %>% filter(!is.na(cit_agency_author_specific) & doc_owner_agency_level == "Regional") %>% select(cit_agency_author_specific, cit_agency_author_level)

colnames(reg.node.list2) <- c("agency", "agency_level")
reg.node.list2$agency_type <- NA
reg.node.list <- rbind(reg.node.list1, reg.node.list2)
reg.node.list <- distinct(reg.node.list)

reg.node.list <- reg.node.list %>% filter(duplicated(reg.node.list$agency) == F)
table(reg.node.list$agency_level)

reg.node.list$agency_level[reg.node.list$agency_level == "State"] <- "State (CA)"
reg.node.list$agency[reg.node.list$agency == "Caltrans"] <- "California Department of Transportation"
reg.node.list$agency_type[reg.node.list$agency == "California Department of Transportation"] <- NA

reg.node.list <- distinct(reg.node.list)

# COUNTY NODE LIST ----
county.edge.list <- agency.df %>% filter(!is.na(cit_agency_author_specific) & doc_owner_agency_level == "County") %>% select(doc_owner_agency, cit_agency_author_specific)
county.edge.list.w <- edge.list %>% group_by(doc_owner_agency, cit_agency_author_specific) %>% count()
colnames(county.edge.list.w) <- c("doc_agency", "cit_agency", "weight")

county.edge.list$doc_owner_agency[county.edge.list$doc_owner_agency == "Caltrans"] <- "California Department of Transportation"

county.node.list1 <- agency.df %>% filter(doc_owner_agency_level == "County") %>% select(doc_owner_agency, doc_owner_agency_level, doc_owner_agency_type)
colnames(county.node.list1) <- c("agency", "agency_level", "agency_type")
county.node.list2 <- agency.df %>% filter(!is.na(cit_agency_author_specific) & doc_owner_agency_level == "County") %>% select(cit_agency_author_specific, cit_agency_author_level)

colnames(county.node.list2) <- c("agency", "agency_level")
county.node.list2$agency_type <- NA
county.node.list <- rbind(county.node.list1, county.node.list2)
county.node.list <- distinct(county.node.list)

county.node.list <- county.node.list %>% filter(duplicated(county.node.list$agency) == F)
table(county.node.list$agency_level)

county.node.list$agency_level[county.node.list$agency_level == "State"] <- "State (CA)"
county.node.list$agency[county.node.list$agency == "Caltrans"] <- "California Department of Transportation"
county.node.list$agency_type[county.node.list$agency == "California Department of Transportation"] <- NA

county.node.list <- distinct(county.node.list)


# PLOT ----
net <- graph_from_data_frame(d = reg.edge.list, vertices = reg.node.list, directed = T) 

clr <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
RColorBrewer::display.brewer.pal(n = 8, name = "Dark2")

V(net)$color <- ifelse(V(net)$agency_level == "City", clr[6],
                       ifelse(V(net)$agency_level == "County", clr[3],
                              ifelse(V(net)$agency_level == "First Nations", clr[7],
                                     ifelse(V(net)$agency_level == "Regional", clr[5],
                                            ifelse(V(net)$agency_level == "State (CA)", clr[2],
                                                   ifelse(V(net)$agency_level == "State (Other)", clr[2],
                                                          ifelse(V(net)$agency_level == "Federal", clr[1],
                                                                 ifelse(is.na(V(net)$agency_level), NA, clr[8]))))))))
deg <- igraph::degree(net, mode="in")
V(net)$size <- deg/40
V(net)$label <- unname(ifelse(igraph::degree(net, mode = "in")[V(net)] > 65, names(V(net)), "")) 
V(net)$label.color = "black"
V(net)$label.size = 3


reg <- ggraph(net, layout="lgl") +
  geom_edge_link(color = "gray50", alpha = 0.1) +
  geom_node_point(color = V(net)$color, size= V(net)$size) + 
  geom_node_text(label = V(net)$label, size = V(net)$label.size, color=V(net)$label.color, repel=T) +
  #facet_wrap(~agency_level) +
  #facet_nodes(~agency_level) +
  theme_void() +
  labs(title = "Regional (MPO) Documents") +
  theme(text= element_text(size=12, family="Times"), 
        plot.title = element_text(hjust = .5, vjust = 0, size = 12))



plot_grid(county, reg, state, nrow = 1)
ggsave(filename = "plots/docs_citation_network.png", width = 11, height = 4)
