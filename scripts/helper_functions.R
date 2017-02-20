# file tasks:
#   define various functions to be used by other scripts
#   should prolly organize them better
#----------------------------------------------------------------------------------------

#take vector of word ids and assign sent ids, new sent id when word id resets to 1
# example:
    # create_sent_id(c(1:3,1:4))
    # [1] 1 1 1 2 2 2 2
create_sent_id <- function(word_sent_id) {
  which(word_sent_id == 1) %>% 
    c((length(word_sent_id)+1)) %>% 
    diff() %>% 
    purrr::map2(1:length(.), ~rep(.y, .x)) %>% 
    unlist()
}

#parse text into sentences/rm blank elements before returning
parse_sentences <- function(doc) {
  sentences <- stringr::str_split(string = doc, 
                     pattern = stringr::regex("(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?)\\s")) %>% 
    unlist() %>% 
    trimws()
  
  sentences[which(sentences != "")]
}

#function to fix words split by line returns in doc read
fix_line_split_words <- function(doc) {
  df <- tibble(doc) %>% 
    tidytext::unnest_tokens(bigram, 
                            doc, 
                            to_lower = T,
                            token = "ngrams",
                            n=2) %>% 
    mutate(toke_gram  = stringr::str_replace_all(bigram, " ", "")) %>% 
    mutate(spell_flag = hunspell::hunspell_check(toke_gram)) %>% 
    filter(spell_flag)
  
  qdap::mgsub(doc, 
              pattern       = paste0("\\b", df$bigram, "\\b"),
              replacement   = df$toke_gram,
              fixed         = FALSE, 
              order.pattern = TRUE)
}

#write sentences to file for parseface
write_sentences <- function(sentences, file="models/syntaxnet/test_data/test_data.txt") {
  sentences_write <- paste0(sentences, "\n")
  readr::write_lines(sentences_write, file)
}

#lookup a word's head word info (i.e. append parent word's to child word's row)
append_head_info <- function(conll_df){
  child_df  <- conll_df
  parent_df <- conll_df %>% 
    dplyr::select(word_id,
                  parent_token  = token,
                  parent_deprel = deprel)
  
  dplyr::left_join(child_df, parent_df, by=c("head"="word_id")) %>% 
    dplyr::mutate(deps = paste0(word_id, ":", deprel, "|", head, ":", parent_deprel)) %>% 
    dplyr::select(word_id, token, cpostag, postag, head, 
                  deprel, deps, parent_token, parent_deprel)
}

# function to plot sentence structure with igraph
# ... passed to plot.igraph
# plot_sentence_ig <- function(sentence_conll_tbl, ...) {
#   edges_df <- tibble(source=sentence_conll_tbl$word_id, target=sentence_conll_tbl$head) %>% 
#     filter(target != 0)
#   nodes_df <- tibble(id=sentence_conll_tbl$word_id, name=sentence_conll_tbl$token)
#   
#   ig <- igraph::graph.data.frame(edges_df, directed=FALSE, nodes_df)
#   plot(ig, ...)
# }

#function to plot sentence structure with visnetwork
plot_sentence_vis_net <- function(sentence_conll_tbl) {
  tbl <- sentence_conll_tbl %>% 
    dplyr::filter(cpostag != ".")
  edges_df <- dplyr::tibble(to     = tbl$word_id, 
                            from   = tbl$head) %>% 
    filter(from != 0)
  nodes_df <- dplyr::tibble(id=tbl$word_id, 
                            label=tbl$token)
  
  visNetwork::visNetwork(nodes_df, edges_df) %>% 
    visNetwork::visHierarchicalLayout(sortMethod="directed")
}

#hunspell stemming function
my_hunspell_stem <- function(token) {
  stem_token <- hunspell::hunspell_stem(token)[[1]]
  if (length(stem_token) == 0) return(token) else return(stem_token[1])
}
#vectorized hunspell stemmer
hunspell_stemmer <- Vectorize(my_hunspell_stem, "token", USE.NAMES = FALSE)

#function to convert tokens to stems
#can return stems or recompleted stems
#shortest token returned as recompleted stem if return_stem=FALSE
stem_complete <- function(tokens, return_stem=FALSE) {
  if (return_stem) {
    return_col <- "stem"
  } else {
    return_col <- "restem"
  }
  
  og_tokens       <- tokens
  hunspell_tokens <- hunspell_stemmer(tokens)
  snow_hun_tokens <- SnowballC::wordStem(hunspell_tokens)
  #stem off ers from end of words
  mega_stem_tokens <- stringr::str_replace_all(snow_hun_tokens, stringr::regex("er$"), "") %>% 
    stringr::str_replace_all(stringr::regex("ed$"), "e")
  
  dplyr::tibble(og=og_tokens, stem=mega_stem_tokens) %>% 
    dplyr::mutate(n_char = nchar(og)) %>% 
    dplyr::mutate(n_char = ifelse(is.na(n_char), 99, n_char)) %>% 
    dplyr::mutate(stem = tolower(stem)) %>% 
    dplyr::mutate(stem = ifelse(stem=="na", NA_character_, stem)) %>% 
    dplyr::group_by(stem) %>% 
    dplyr::mutate(restem = og[which.min(n_char)]) %>% 
    dplyr::ungroup() %>% 
    .[[return_col]]
}

#rneo4j build graph with periodic commit
#query must reference tbl vars prefixed with 'row.'
build_graph <- function(tbl, graph, query, neo_import_dir="~/Documents/Neo4j/default.graphdb/import") {
  tmp_csv <- paste0(neo_import_dir, "/tmp.csv")
  readr::write_csv(tbl, tmp_csv)
  
  query_start <- paste0('USING PERIODIC COMMIT\nLOAD CSV WITH HEADERS FROM "file:///tmp.csv" AS row')
  query_full  <- paste0(query_start,"\n",query)
  
  RNeo4j::cypher(graph, query_full)
  summary(graph)
}
