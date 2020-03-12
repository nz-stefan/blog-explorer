################################################################################
# Create data model for the Shiny app to consume. 
#
# The generation of the topic model is inspired by Julia Silge's blog article on
# Structural Topic Models (https://juliasilge.com/blog/evaluating-stm/)
#
# The data exported is a list containing the following objects:
# 
# - n_topics <integer>          -> Number of topics in the topic model 
# - d_top_terms <tibble>        -> top terms of each model
# - d_gamma_terms <tibble>      -> topic prevalence along with top words
# - d_nodes <tibble>            -> nodes of the topic correlation graph
# - d_edges <tibble>            -> edges of the topic correlation graph
# - d_estimated_effect <tibble> -> topic model relevance by publication date
# 
# Author: Stefan Schliebs
# Created: 2020-03-12 08:37:45
################################################################################


library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(stopwords)
library(tidyr)
library(stm)
library(purrr)
library(logging)
library(visNetwork)

# load additional STM tools
source("src/stm-utils.R")


# Config ------------------------------------------------------------------

# path to data
F_R_BLOGGER_BLOGS <- "data/r-blogger-blogs.rds"

# path to export topic model to
F_TOPIC_MODEL <- "data/topic-model.rds"

# path where to export data model of the Shiny app
F_APP_DATA_MODEL <- "data/app-data-model.rds"

# additional stopwords to remove from the corpus
MORE_STOPWORDS <- c(
  "data", "day", "true", "false", "span", "na", "function", "functions", "amp", "gl", "library", "code", "null", 
  "package", "packages", "df", "fill", "color", "gonna", "txt", "readme", "time", "id", "date", "type", "file", 
  "files", "related", "post", "create", "science", "don", "ll", "models", "plot", "mathbf", "ve", "lot", "image",
  "add", "map", "job", "word", "http", "https", "www", "org", "html", "com", "rules", "item", "dataset", "email",
  "col"
)

# number of topic
NUM_TOPICS <- 60

# configure logging
basicConfig()


# Prepare data ------------------------------------------------------------

# load data
loginfo("Loading data from '%s'", F_R_BLOGGER_BLOGS)
d_blogs <- readRDS(F_R_BLOGGER_BLOGS)

# convert data into tidy text dataframe
loginfo("Processing corpus")
d_tidy <- 
  d_blogs %>% 
  
  # remove blogs with less than 50 words
  mutate(n_words = map_dbl(blog_content, str_count)) %>%
  filter(n_words > 50) %>% 
  
  # we need a document ID for the conversion into a sparse term-document matrix
  mutate(doc_id = 1:n()) %>%
  
  # convert blog content into tidy format
  unnest_tokens(word, blog_content, token = "words") %>%
  
  # remove stopwords
  anti_join(stop_words, by = "word") %>% 
  anti_join(tibble(word = MORE_STOPWORDS), by = "word") %>%
  
  # remove rare words
  add_count(word) %>%
  filter(n > 100) %>%
  select(-n)


# convert to sparse matrix as required by stm package
loginfo("Preparing training data")
d_sparse <- 
  d_tidy %>%
  count(doc_id, word) %>%
  cast_sparse(doc_id, word, n)



# Extract basic stats -----------------------------------------------------

n_blogs <- n_distinct(d_tidy$doc_id)
n_authors <- n_distinct(d_tidy$blog_author)
n_words <- nrow(d_tidy)
n_book_pages <- round(n_words / 300)  # about 300 words per typical book page




# Train model -------------------------------------------------------------

# train model
loginfo("Training model with %d topics", NUM_TOPICS)
# m_topic <- stm(d_sparse, K = NUM_TOPICS, verbose = TRUE, max.em.its = 75)
m_topic <- readRDS(F_TOPIC_MODEL)

# export to disk
loginfo("Exporting topic model to '%s'", F_TOPIC_MODEL)
saveRDS(m_topic, F_TOPIC_MODEL)



# Process model outputs ---------------------------------------------------

# unpack topic model
d_beta <- tidy(m_topic)

# compute top terms per topic
d_top_terms <- 
  d_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  mutate(top_word = beta == max(beta)) %>% 
  arrange(topic) %>%
  ungroup()

# compute gamma marix (probabilities that each document is generated from each topic)
d_gamma <- tidy(m_topic, matrix = "gamma", document_names = rownames(d_sparse))

# compute topic prevalence
d_gamma_terms <- 
  d_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(d_top_terms, by = "topic")



# Create topic correlation network ----------------------------------------

loginfo("Creating topic correlation graph")

# compute topic correlations
m_corr <- stm::topicCorr(m_topic, cutoff = 0.075)

# extract adjacency matrix from topic correlations and build a network via igraph
g_vis <- 
  igraph::simplify(igraph::graph.adjacency(m_corr$poscor, mode = "undirected", weighted = TRUE)) %>% 
  toVisNetworkData()

# add additional properties to the graph nodes
d_nodes <- 
  g_vis$nodes %>% 
  left_join(d_top_terms %>% filter(top_word) %>% select(-top_word), by = c(id = "topic")) %>% 
  left_join(
    d_gamma_terms %>% filter(top_word) %>% select(topic, gamma),
    by  = c(id = "topic")
  ) %>% 
  mutate(size = 1000 * gamma) %>% 
  select(id, label = term, size)

# add additional properties to the graph edges
d_edges <- 
  g_vis$edges %>% 
  mutate(width = weight * 20)

# visNetwork(nodes = d_nodes, edges = d_edges, height = "500px")



# Estimate effect of publication date -------------------------------------

loginfo("Estimating effect of publication date")

# separate meta information (publication_date by document) from corpus
d_meta <- 
  d_tidy %>% 
  group_by(doc_id) %>% 
  summarise(publication_date = as.numeric(max(publication_date))) 

loginfo("Running estimation model. This may take a minute or more.")
system.time(
  d_effect <- 
    estimateEffect(
      c(1:NUM_TOPICS) ~ s(publication_date), 
      m_topic,  
      d_meta
    )
)


# unpack estimated effect into tidy format
# extraction routine copied from https://github.com/mikajoh/tidystm
d_estimated_effect <- 
  extract.estimateEffect(
    x = d_effect,
    covariate = "publication_date",
    method = "continuous",
    model = m_topic
  ) %>% 
  mutate(publication_date = as.Date(covariate.value, origin = "1970-01-01")) %>% 
  select(topic, publication_date, estimate, ci_lower = ci.lower, ci_upper = ci.upper) %>% 
  as_tibble()


# d_estimated_effect %>% 
#   left_join(d_top_terms %>% filter(top_word)) %>% 
#   ggplot(aes(x = publication_date, y = estimate, ymin = ci_lower, ymax = ci_upper)) +
#   facet_wrap(~ paste(topic, term), ncol = 10) +
#   geom_ribbon(alpha = .25) +
#   geom_line()


# Export combined object --------------------------------------------------

loginfo("Exporting data model to '%s'", F_APP_DATA_MODEL)

# prepare list with all objects to export
l_export <- list(
  n_blogs <- n_blogs,
  n_authors <- n_authors,
  n_words <- n_words,
  n_book_pages <- n_book_pages,
  n_topics = NUM_TOPICS,
  d_top_terms = d_top_terms,
  d_gamma_terms = d_gamma_terms,
  d_nodes = d_nodes,
  d_edges = d_edges,
  d_estimated_effect = d_estimated_effect
)

saveRDS(l_export, file = F_APP_DATA_MODEL)

loginfo("Done")
