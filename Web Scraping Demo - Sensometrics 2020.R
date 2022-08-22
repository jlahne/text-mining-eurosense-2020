
# License -----------------------------------------------------------------

# Copyright (c) 2020 Jacob Lahne
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Setup -------------------------------------------------------------------

library(rvest)
library(tidyverse)
library(robotstxt)
library(tidytext)

x <- read_html("https://cocktailvirgin.blogspot.com/2020/09/the-herbivore.html")


# Check robots.txt for permissions ----------------------------------------

robotstxt("https://cocktailvirgin.blogspot.com")


# Scraping functions ------------------------------------------------------

get_body <- function(x){
  
  x %>%
    html_nodes("#main") %>%
    html_nodes("div.post-body") %>%
    html_text(trim = T)
  
}

get_ingredients <- function(x){
  
  x %>%
    html_nodes(".post-labels a") %>%
    html_text()
  
}

get_title <- function(x){
  
  x %>%
    html_nodes("h3") %>%
    html_text(trim = T)
  
}

get_date <- function(x){
  
  x %>%
    html_nodes(".date-header") %>%
    html_text()
  
}

get_recipe <- function(x){
  
  x %>%
    html_nodes(".recipeDirection") %>%
    html_text()
  
}

get_links <- function(x){
  
  x %>%
    html_nodes("div.post-body") %>%
    html_nodes("a") %>%
    html_attr(name = "href")
  
}

get_title(x)
get_ingredients(x)
get_body(x)
get_date(x)

# Basic text parsing ------------------------------------------------------

load("cocktailvirgin_demodata.RData")
library(spacyr)
library(skimr)

demo_posts

# Let's look at the ingredients (based on tags) for this set of drinks
demo_posts %>%
  mutate(title = str_squish(title)) %>%
  filter(!str_detect(ingredients, "\\*[^original|^hot]")) %>% # here we are removing some posts that are not recipes
  unnest_tokens(output = ingredient,
                input = ingredients,
                token = "regex",
                pattern = " ### ") %>% # when I scraped these reviews I piled tags into a single column, separated by " ### " for easy later parsing
  filter(!str_detect(ingredient, "#|\\*")) -> # and finally we are removing symbols the author used to mark "non-ingredient" tags
  demo_ingredients

demo_ingredients %>%
  group_by(ingredient) %>%
  count(sort = T)

# Now let's look at splitting apart the recipes (first part of the posts) from
# the descriptions and commentary
demo_posts %>%
  mutate(title = str_squish(title)) %>%
  filter(!str_detect(ingredients, "\\*[^original|^hot]")) %>%
  separate(col = body, 
           into = c("recipe", "description"), 
           sep = "\\.\\n", extra = "merge") -> # here we are splitting at the first period followed by a new-line (\n), which in about 95% of the articles seems to be the normal split point
  demo_posts_separated

demo_posts
demo_posts_separated

# Finally, let's pull out only adjectives on the assumption
# that these are more likely to be descriptors (NB this is a bad assumption!)
spacy_initialize()
demo_posts_separated %>%
  transmute(doc_id = post_id,
            text = description) %>% # This is the TIF format (https://github.com/ropensci/tif) required by spacyr: two-columns, one with a document ID and the other with the full text
  spacy_parse(additional_attributes = c("is_stop", "like_num")) %>% # spaCy is a pre-trained neural-network for NLP, here we request additional attributes: is it a stopword and is it like a number?
  as_tibble() ->
  demo_posts_parsed
spacy_finalize()

demo_posts_parsed
demo_posts_parsed %>% 
  filter(pos == "ADJ",
         !is_stop,
         !like_num,
         entity == "",
         str_length(lemma) > 1) %>% # these conditions are that the part-of-speech is adjective, the lemma is not a "named entity" (like a proper noun), and is not a stop-word or number, and is not a singe character
  select(doc_id, token, lemma) %>%
  left_join(demo_posts_separated %>% mutate(post_id = as.character(post_id)), by = c("doc_id" = "post_id")) -> # here we rejoin the parsed tokens with the original data
  demo_posts_tokenized

# What are the most frequent adjectives used to talk about these cocktails?
demo_posts_tokenized %>%
  group_by(lemma) %>%
  count(sort = T) # Huh, they look quite like descriptors...

# Now, for a more complicated pass: what are the most distinguishing adjectives for each ingredient (using term frequency-inverse document frequency as the metric)
demo_posts_parsed %>% 
  filter(pos == "ADJ",
         !is_stop,
         !like_num,
         entity == "",
         str_length(lemma) > 1) %>% 
  select(doc_id, token, lemma) %>%
  left_join(demo_ingredients %>% mutate(post_id = as.character(post_id)), by = c("doc_id" = "post_id")) %>% # here we join the parsed data with the ingredient data
  group_by(ingredient, lemma) %>%
  count(sort = T) %>%
  bind_tf_idf(term = lemma, document = ingredient, n = n) %>% # TF-IDF is a metric that looks at the (log) ratio of frequency of term in a document (here an ingredient) to frequency of the term across the whole corpus
  group_by(ingredient) %>%
  mutate(lemma = reorder_within(lemma, by = tf_idf, within = ingredient)) %>%
  top_n(n = 10, wt = tf_idf) %>%
  filter(ingredient %in% c("gin", "rum", "campari", "whiskey (rye)", "scotch", "vermouth (sweet)")) %>%
  ungroup() %>%
  mutate(lemma = reorder(lemma, tf_idf)) %>%
  ggplot(aes(x = lemma, y = tf_idf)) + 
  geom_col(aes(fill = ingredient %>% as.factor), show.legend = F) +
  coord_flip() + 
  scale_x_reordered() + 
  facet_wrap(~ingredient, scales = "free")
