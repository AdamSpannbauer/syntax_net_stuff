# file tasks:
#   take output of scripts/read_parse_file.R (dataframe of modified conll tables)
#   create edges/nodes dataframes to write to neo4j
#   write data to neo4j

#basic graph structure
#   (word)-->(parent_word)
#----------------------------------------------------------------------------------------

library(tidyverse)
library(RNeo4j)
source("scripts/helper_functions.R")

#######################
# prep graph for data #
#######################
graph <- startGraph("http://localhost:7474/db/data")
clear(graph, FALSE)
try(addConstraint(graph, "word", "alias"), silent = T)

#######################
# prep data for graoh #
#######################
doc_df <- read_csv("data/tagged_doc_df.csv")

nodes_df <- doc_df %>% 
  mutate(sent_word_id = paste(sent_id, "_", word_id)) %>% 
  group_by(stem) %>% 
  summarise(sent_word_ids = paste(sent_word_id, collapse=", "),
            cpostags      = paste(cpostag, collapse=", "),
            postags       = paste(postag, collapse=", "),
            deprels       = paste(deprel, collapse=", ")) %>% 
  ungroup()

edges_df <- doc_df %>% 
  select(stem, parent_stem, deprel, parent_deprel, sent_id) %>% 
  group_by(stem, parent_stem, deprel, parent_deprel) %>% 
  summarise(sent_ids = paste(sent_id, collapse=", "),
            weight   = n()) %>% 
  ungroup() %>% 
  arrange(desc(weight)) %>% 
  filter(!is.na(parent_deprel))

#######################
# write data to graoh #
#######################
query_nodes <- 'MERGE (w:word {alias: row.stem, sent_word_ids: row.sent_word_ids, 
               cpostags: row.cpostags, postags: row.postags, deprels: row.deprels })'
build_graph(nodes_df, graph, query_nodes)

query_edges <- 'MATCH (a:word {alias: row.stem})
                MATCH (b:word {alias: row.parent_stem})
                MERGE (a)-[r:rel {child_deprel: row.deprel, parent_deprel: row.parent_deprel,
                                  sent_ids: row.sent_ids, weight: row.weight}]->(b)'
build_graph(edges_df, graph, query_edges)
