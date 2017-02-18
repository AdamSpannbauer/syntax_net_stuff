library(tidyverse)
library(magrittr)
library(igraph)
source("scripts/helper_functions.R")

syntaxnet_path      <- "models/syntaxnet"
syntaxnet_data_path <- paste0(syntaxnet_path, "/test_data")
data_in_file        <- paste0(syntaxnet_data_path,"/test_data.txt")
data_out_file       <- paste0(syntaxnet_data_path,"/test_data_parsed.txt")
doc_path            <- "data/little_red_riding_hood.txt"
# doc_path            <- "data/goldilocks.txt"

# read goldilocks and prep for syntaxnet parse
read_lines(doc_path) %>% 
  parse_sentences() %>% 
  write_sentences(data_in_file)

# run manual syntax net.... system command not working..
# follow up on using system/system2
# og_wd <- setwd(syntaxnet_path)
# system("syntaxnet/demo.sh", ignore.stdout = !verbose, wait = TRUE)
# system("syntaxnet/demo.sh", ignore.stdout = !verbose, wait = TRUE)
# setwd(og_wd)

#data out table columns
#      "ID": Word index (starts at 1; in sentence)
#    "FORM": Surface form of the word
#   "LEMMA": Lemmatized form of the word
# "CPOSTAG": Coarse-grained POS-tag
#  "POSTAG": Fine-grained POS-tag
#    "HEAD": Head of the current token for dependency relation
#  "DEPREL": The dependency relation
# "PDEPREL": The dependency relation for PHEAD (currently ignored)

parse_df <- read_delim(data_out_file, 
                       delim = "\t", 
                       col_names = FALSE) %>%
  rename(word_id  = X1,
         token    = X2,
         lemma    = X3,
         cpostag  = X4,
         postag   = X5,
         feats    = X6,
         head     = X7,
         deprel   = X8,
         phead    = X9) %>% 
  mutate(sent_id = create_sent_id(word_id)) %>% 
  select(sent_id, word_id, token,
         cpostag, postag, head, deprel)

sent_nested_df <- parse_df %>% 
  nest(-sent_id) %>% 
  mutate(data = map(data, ~append_head_info(.x)))

#plot sentences 
plot_sentence(sent_nested_df$data[[2]], 
              vertex.color="cyan",
              vertex.size=1)



