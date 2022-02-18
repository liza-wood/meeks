# 7. Q4. ---- WHERE DOES SCIENTIFIC INFORMATION ENTER? ----
# Q4: Where does scientific information about these emerging issues appear to enter into the state transportation policy subsystem
df$cit_journal[df$cit_journal == "Transportation Research Part A: Policy and Practice"] <- "Transportation Research Pt. A"
df$cit_journal[df$cit_journal == "Transportation Research Part B: Methodological"] <- "Transportation Research Pt. B"
df$cit_journal[df$cit_journal == "Transportation Research Part C: Emerging Technologies"] <- "Transportation Research Pt. C"
df$cit_journal[df$cit_journal == "Renewable and Sustainable Energy Reviews"] <- "Renewable & Sustainable Energy Review"
df$cit_journal[df$cit_journal == "Transportation\nResearch Record"] <- "Transportation Research Record"
df$cit_journal[df$cit_journal == "Journal of Geotechnical and Geoenvironmental Engineering  ASCE"] <- "J. of Geotech. & Geoenvt. Engin."
df$cit_journal[df$cit_journal == "Journal of the American\nPlanning Asscn."] <- "J. of the Amer. Planning Assn."
df$cit_journal[df$cit_journal == "Journal of Transportation Engineering"] <- "J. of Transportation Engin."
df$cit_journal[df$cit_journal == "Journal of Structural Engineering"] <- "J. of Structural Engin."
df$cit_journal[df$cit_journal == "American Journal of Public Health"] <- "American J. of Public Health"
df$cit_journal[df$cit_journal == "Accident and Analysis Prevention"] <- "Accident & Analysis Prevention"
df$cit_journal[df$cit_journal == "Cement and Concrete Research"] <- "Cement & Concrete Research"

agency.df$cit_agency_author_specific[agency.df$cit_agency_author_specific == "Butte County Association of Governments"] <- "Butte COG"
agency.df$cit_agency_author_specific[agency.df$cit_agency_author_specific == "Kern Council of Governments"] <- "Kern COG"
agency.df$cit_agency_author_specific[agency.df$cit_agency_author_specific == "Stanislaus Council of Governments"] <- "Stanislaus COG"
agency.df$cit_agency_author_specific[agency.df$cit_agency_author_specific == "Department of Water Resources"] <- "CA DWR"
agency.df$cit_agency_author_specific[agency.df$cit_agency_author_specific == "California Transportation Commission"] <- "CA Transportation Commission"
agency.df$cit_agency_author_specific[agency.df$cit_agency_author_specific == "Southern California Association of Governments"] <- "SCAG"

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
  mutate(total = sum(count)) %>% 
  ungroup() %>% 
  mutate(prop.total = count/total) %>% 
  rename("reference" = "cit_agency_author_specific")
journal.counts <- df %>% 
  filter(journalpub_match == T) %>% 
  group_by(doc_owner_agency_level, cit_journal) %>% 
  count(cit_journal) %>% 
  rename("count" = "n") %>% 
  ungroup() %>% 
  mutate(total = sum(count)) %>% 
  ungroup() %>% 
  mutate(prop.total = count/total) %>% 
  rename("reference" = "cit_journal")

agency.short <- agency.counts %>% select(-cit_agency_author_level)
total.counts <- rbind(agency.short, journal.counts)



state.ag <- agency.counts %>% 
  filter(doc_owner_agency_level == "State") %>% 
  mutate(n = sum(count), cit_type = cit_agency_author_level) %>% 
  select(-cit_agency_author_level)
state.j <- journal.counts %>% 
  filter(doc_owner_agency_level == "State") %>% 
  mutate(n = sum(count), cit_type = "journal") 

state <- rbind(state.ag, state.j)

state <- state %>% 
  mutate(together.total = sum(unique(state$n))) %>% 
  mutate(together.prop = count/together.total) 

state.spread <- state %>% 
  group_by(cit_type) %>% 
  summarize(sum.prop = sum(together.prop)) %>% 
  arrange(desc(sum.prop))

state <- state %>%
  top_n(15)

region.ag <- agency.counts %>% 
  filter(doc_owner_agency_level == "Regional") %>% 
  mutate(n = sum(count), cit_type = cit_agency_author_level) %>% 
  select(-cit_agency_author_level)
region.j <- journal.counts %>% 
  filter(doc_owner_agency_level == "Regional") %>% 
  mutate(n = sum(count), cit_type = "journal") 

region <- rbind(region.ag, region.j)

region <- region %>% 
  mutate(together.total = sum(unique(region$n))) %>% 
  mutate(together.prop = count/together.total) 

region.spread <- region %>% 
  group_by(cit_type) %>% 
  summarize(sum.prop = sum(together.prop)) %>% 
  arrange(desc(sum.prop))

region <- region %>% 
  top_n(15)

county.ag <- agency.counts %>% 
  filter(doc_owner_agency_level == "County") %>% 
  mutate(n = sum(count), cit_type = cit_agency_author_level) %>% 
  select(-cit_agency_author_level)
county.j <- journal.counts %>% 
  filter(doc_owner_agency_level == "County") %>% 
  mutate(n = sum(count), cit_type = "journal") 

county <- rbind(county.ag, county.j)
county <- county %>% 
  mutate(together.total = sum(unique(county$n))) %>% 
  mutate(together.prop = count/together.total)

county.spread <- county %>% 
  group_by(cit_type) %>% 
  summarize(sum.prop = sum(together.prop)) %>% 
  arrange(desc(sum.prop))

county <- county %>% 
  top_n(15)


display.brewer.pal(8, "Greys")
grey <- brewer.pal(8, "Greys")

table(state$cit_type)
state$cit_type <- factor(state$cit_type, levels = c("Federal", "State (CA)", "journal"))
levels(state$cit_type) <- c("Agency: Federal", "Agency: State", "Academic journal")
table(region$cit_type)
region$cit_type <- factor(region$cit_type, levels = c("Federal", "State (CA)", "Regional", "journal"))
levels(region$cit_type) <- c("Agency: Federal", "Agency: State", "Agency: Regional",  "Academic journal")
table(county$cit_type)
county$cit_type <- factor(county$cit_type, levels = c("Federal", "State (CA)", "Regional"))
levels(county$cit_type) <- c("Agency: Federal", "Agency: State", "Agency: Regional")

## Categories as a proportion of each agency's total citation, plotted using function in functions.R
st <- plot.theme.levels(state, "State", .11, colors = c(grey[6], grey[5], grey[3]), xlab = "") + theme(legend.position = "none") 
rg <- plot.theme.levels(region, "Regional", .17, colors = c(grey[6], grey[5], grey[4], grey[3]), xlab = "") + theme(legend.position = "none")
co <- plot.theme.levels(county, "County", .09, colors = c(grey[6], grey[5], grey[4]), xlab = "") + theme(legend.position = "none")
plot_grid(st, rg, co, ncol=3, label_size = 14, label_fontfamily = "Times", labels = c("A", "B", "C"))

ggsave(filename = "plots/combined_level.png", width = 12, height = 5)

## Table ----

state.spread <- state.spread[-8,]
total.props <- cbind(state.spread, region.spread, county.spread)
total.props[2] <- round(total.props[2], 3)
total.props[4] <- round(total.props[4], 3)
total.props[6] <- round(total.props[6], 3)

# 7. NETWORK ----
# edgelist and nodelist ----

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
