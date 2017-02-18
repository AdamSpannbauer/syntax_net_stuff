#take vector of word ids and assign sent ids, new sent id when word id resets to 1
# example:
    # create_sent_id(c(1:3,1:4))
    # [1] 1 1 1 2 2 2 2
create_sent_id <- function(word_sent_id) {
  which(word_sent_id == 1) %>% 
    c((length(word_sent_id)+1)) %>% 
    diff() %>% 
    map2(1:length(.), ~rep(.y, .x)) %>% 
    unlist()
}

#parse text into sentences
parse_sentences <- function(doc) {
  sentences <- stringr::str_split(string = doc, 
                     pattern = stringr::regex("(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?)\\s")) %>% 
    unlist() %>% 
    trimws()
  
  sentences[which(sentences != "")]
}

#write sentences to file for parseface
write_sentences <- function(sentences, file="models/syntaxnet/test_data/test_data.txt") {
  sentences_write <- paste0(sentences, "\n")
  readr::write_lines(sentences_write, file)
}

#lookup a word's head word info (i.e. append )
append_head_info <- function(conll_df){
  child_df  <- conll_df
  parent_df <- conll_df %>% 
    select(word_id,
           parent_token  = token,
           parent_deprel = deprel)
  
  left_join(child_df, parent_df, by=c("head"="word_id")) %>% 
    mutate(deps = paste0(word_id, ":", deprel, "|", head, ":", parent_deprel)) %>% 
    select(word_id, token, cpostag, postag, head, 
           deprel, deps, parent_token, parent_deprel)
}

# use igraph to plot sentence structure
# ... passed to plot.igraph
plot_sentence <- function(sentence_conll_tbl, ...) {
  edges_df <- tibble(source=sentence_conll_tbl$word_id, target=sentence_conll_tbl$head) %>% 
    filter(target != 0)
  nodes_df <- tibble(id=sentence_conll_tbl$word_id, name=sentence_conll_tbl$token)
  
  ig <- graph.data.frame(edges_df, directed=FALSE, nodes_df)
  plot(ig, ...)
}

#bad
# parseface <- function(sentences, syntaxnet_path = "models/syntaxnet", verbose = FALSE) {
#   og_wd <- setwd(syntaxnet_path)
#   on.exit(setwd(og_wd))
#   
#   sentences_write <- paste0(sentences, "\n")
#   
#   readr::write_lines(sentences_write, "test_data/test_data.txt")
#   
#   system("syntaxnet/demo.sh", ignore.stdout = !verbose, wait = TRUE)
#   system("syntaxnet/demo.sh", ignore.stdout = !verbose, wait = TRUE)
# 
#   parse_df <- read_delim("test_data/test_data_parsed.txt", delim = "\t", 
#                          col_names = FALSE,
#                          col_types = cols(
#                            X1 = col_integer(),
#                            X2 = col_character(),
#                            X3 = col_character(),
#                            X4 = col_character(),
#                            X5 = col_character(),
#                            X6 = col_character(),
#                            X7 = col_integer(),
#                            X8 = col_character(),
#                            X9 = col_character(),
#                            X10 = col_character()
#                          )) %>%
#     rename(word_id  = X1,
#            token    = X2,
#            lemma    = X3,
#            cpostag  = X4,
#            postag   = X5,
#            feats    = X6,
#            head     = X7,
#            deprel   = X8,
#            phead    = X9) %>% 
#     mutate(sent_id = create_sent_id(word_id))
#   
#   return(parse_df)
# }

