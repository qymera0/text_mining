
library(tidyverse)
library(tidytext)
library(janeaustenr)
library(gutenbergr)
library(forcats)
library(stringr)

# 3.1 TERM FREQUENCY IN JANE AUSTEN ---------------------------------------

book_words <-
        austen_books() %>%
        unnest_tokens(word, text) %>%
        count(book, word, sort = T)

total_words <-
        book_words %>%
        group_by(book) %>%
        summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

ggplot(book_words, aes(n/total, fill = book)) +
        geom_histogram(show.legend = FALSE) +
        xlim(NA, 0.0009) +
        facet_wrap(~book, ncol = 2, scales = "free_y")

# 3.2 ZIPF´S LAW ----------------------------------------------------------

freq_by_rank <-
        book_words %>%
        group_by(book) %>%
        mutate(rank = row_number(), # Previus table was already ordered
               `term frequency` = n/total)

freq_by_rank %>% 
        ggplot(aes(rank, `term frequency`, color = book)) + 
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
        scale_x_log10() +
        scale_y_log10()

# Fit a pwer law

rank_subset <-
        freq_by_rank %>%
        filter(rank < 500,
               rank > 100)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

# Plot fitted power law

freq_by_rank %>% 
        ggplot(aes(rank, `term frequency`, color = book)) + 
        geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
        scale_x_log10() +
        scale_y_log10()

# We have found a result close to the classic version of Zipf’s law for the corpus
# of Jane Austen’s novels. The deviations we see here at high rank are not uncommon
# for many kinds of language; a corpus of language often contains fewer rare words
# than predicted by a single power law. The deviations at low rank are more unusual.
# Jane Austen uses a lower percentage of the most common words than many collections 
# of language. This kind of analysis could be extended to compare authors, or to
# compare any other collections of text; it can be implemented simply using tidy
# data principles.

# 3.3 THE BIND_TF_IDF FUNCTION --------------------------------------------

book_words <-
        book_words %>%
        bind_tf_idf(word, book, n)

book_words%>%
        select(-total) %>%
        arrange(desc(tf_idf))

book_words %>%
        arrange(desc(tf_idf)) %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>% 
        group_by(book) %>% 
        top_n(15) %>% 
        ungroup() %>%
        ggplot(aes(word, tf_idf, fill = book)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(~book, ncol = 2, scales = "free") +
        coord_flip()

# 3.4 A CORPUS OF PHYSICS TEXTS -------------------------------------------

physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")

physics_words <-
        physics %>%
        unnest_tokens(word, text) %>%
        count(author, word, sort = T)

plot_physics <-
        physics_words %>%
        bind_tf_idf(word, author, n) %>%
        mutate(word = fct_reorder(word, tf_idf)) %>%
        mutate(author = factor(author, levels = c("Galilei, Galileo",
                                                 "Huygens, Christiaan", 
                                                 "Tesla, Nikola",
                                                 "Einstein, Albert")))

plot_physics %>% 
        group_by(author) %>% 
        top_n(15, tf_idf) %>% 
        ungroup() %>%
        mutate(word = reorder(word, tf_idf)) %>%
        ggplot(aes(word, tf_idf, fill = author)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(~author, ncol = 2, scales = "free") +
        coord_flip()

physics %>%
        filter(str_detect(text, "_k_")) %>%
        select(text)

physics %>%
        filter(str_detect(text, "RC")) %>%
        select(text)

# Remove custom stop words

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))

physics_words <- anti_join(physics_words, mystopwords, 
                           by = "word")
