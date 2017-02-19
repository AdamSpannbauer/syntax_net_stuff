# file tasks:
#   prep a document for syntax net parsey mcparseface
#   read/clean conll table results of parsed doc
#   plot sentence hierarchy
#----------------------------------------------------------------------------------------

library(tidyverse)
library(magrittr)
source("scripts/helper_functions.R")

############################
# define needed file paths #
############################
doc_path            <- "data/little_red_riding_hood.txt"
# doc_path            <- "data/goldilocks.txt"

#data paths require modifying syntaxnet context file
syntaxnet_path      <- "models/syntaxnet"
syntaxnet_data_path <- paste0(syntaxnet_path, "/test_data")
data_in_file        <- paste0(syntaxnet_data_path,"/test_data.txt")
data_out_file       <- paste0(syntaxnet_data_path,"/test_data_parsed.txt")

##########################
# prep doc for syntaxnet #
##########################
read_lines(doc_path) %>% 
  parse_sentences() %>% 
  write_sentences(data_in_file) #write sentences to file 1 sent per line

########################## BROKEN # BROKEN # BROKEN #
# run parsey mcparseface # BROKEN # BROKEN # BROKEN #
########################## BROKEN # BROKEN # BROKEN #
# run manual syntax net.... system command not working..
# follow up on using system/system2
# og_wd <- setwd(syntaxnet_path)
# system("syntaxnet/demo.sh", ignore.stdout = !verbose, wait = TRUE)
# system("syntaxnet/demo.sh", ignore.stdout = !verbose, wait = TRUE)
# setwd(og_wd)

###################################
# conll table parse output format #
###################################
#reference: http://universaldependencies.org/format.html
#      "ID": Word index (starts at 1; in sentence)
#    "FORM": Surface form of the word
#   "LEMMA": Lemmatized form of the word
# "CPOSTAG": Coarse-grained POS-tag
#  "POSTAG": Fine-grained POS-tag
#    "HEAD": Head of the current token for dependency relation
#  "DEPREL": The dependency relation
# "PDEPREL": The dependency relation for PHEAD (currently ignored)

######################
# clean parse output #
######################
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

#append parent word info to each child row
sent_nested_df <- parse_df %>% 
  nest(-sent_id) %>% 
  mutate(data = map(data, ~append_head_info(.x)))

#stem words to dim reduce
doc_tag_df <- sent_nested_df %>% 
  unnest() %>% 
  filter(cpostag != ".") %>% 
  mutate(token    = tolower(token)) %>% 
  mutate(stem     = stem_complete(token)) %>% 
  mutate(stem_pos = paste0(token, "_", deprel)) %>% 
  mutate(parent_stem = stem_complete(parent_token)) %>% 
  mutate(parent_stem_pos = paste0(parent_token, "_", parent_deprel))

#save output
write_csv(doc_tag_df, "data/tagged_doc_df.csv")

##########################
# viz sentence hieracrhy #
##########################
tbl_ind <- 5
plot_sentence_vis_net(sent_nested_df$data[[tbl_ind]])
