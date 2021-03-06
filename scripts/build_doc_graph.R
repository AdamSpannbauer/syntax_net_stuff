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
try(addConstraint(graph, "word", "name"), silent = T)
try(addIndex(graph, "word", "name"),      silent = T)

#######################
# prep data for graoh #
#######################
doc_df <- read_csv("data/tagged_doc_df.csv")

nodes_df <- doc_df %>% 
  mutate(sent_word_id = paste(sent_id, "_", word_id)) %>% 
  group_by(token) %>% 
  summarise(sent_word_ids = paste(unique(sent_word_id), collapse=", "),
            cpostags      = paste(unique(cpostag), collapse=", "),
            postags       = paste(unique(postag), collapse=", "),
            deprels       = paste(unique(deprel), collapse=", ")) %>% 
  ungroup()

edges_df <- doc_df %>% 
  select(token, parent_stem, deprel, parent_deprel, sent_ids = sent_id) %>% 
  mutate(weight = 1) %>% 
  # group_by(token, parent_stem, deprel, parent_deprel) %>% 
  # summarise(sent_ids = paste(unique(sent_id), collapse=", "),
  #           weight   = n()) %>% 
  # ungroup() %>% 
  # arrange(desc(weight)) %>% 
  filter(!is.na(parent_deprel))

#######################
# write data to graoh #
#######################
query_nodes <- 'MERGE (w:word {name: row.token, sent_word_ids: row.sent_word_ids, 
               cpostags: row.cpostags, postags: row.postags, deprels: row.deprels })'
build_graph(nodes_df, graph, query_nodes)

query_edges <- 'MATCH (a:word {name: row.token})
                MATCH (b:word {name: row.parent_stem})
                MERGE (a)-[r:rel {child_deprel: row.deprel, parent_deprel: row.parent_deprel,
                                  sent_ids: row.sent_ids, weight: row.weight}]->(b)'
build_graph(edges_df, graph, query_edges)
