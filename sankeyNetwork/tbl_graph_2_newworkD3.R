library(networkD3)
library(tidyverse)
library(tidygraph)
load("../data/myData.Rdata")


##########################################################
df <- 
  sicnu_contribution_by_college %>% 
  select(discipline_cn, coll_name_cn, n_paper, n_cited) %>% 
  arrange(discipline_cn, desc(n_cited)) %>% 
  select(discipline_cn, coll_name_cn, count = n_cited) %>% 
  filter(count > 0)

df
##########################################################

edges <- df %>% as_tbl_graph() %>% activate(edges) %>% as_tibble()
nodes <- df %>% as_tbl_graph() %>% activate(nodes) %>% as_tibble()

edges
nodes


##########################################################
edges_d3 <- edges %>% 
  mutate(from = from - 1, to = to - 1)

nodes_d3 <- nodes %>%
  mutate(id = row_number() - 1) %>% 
  relocate(id)

sankey_d3 <- sankeyNetwork(
  Links = edges_d3,                   # edges-list 
  Nodes = nodes_d3,                   # Nodes list 
  Source = "from", Target = "to",     # edges-data 
  NodeID = "name",                    # nodes data
  Value = "count",                    # edges-data 
  #Nodesize = ,                       # nodes data
  #Group = ,                          # nodes data
  height = 1400,
  width = 1000,
  nodeWidth = 30,
  fontSize = 14, unit = "Letter(s)")

saveNetwork(sankey_d3, file = "networkD3.html", selfcontained = FALSE)
##########################################################




