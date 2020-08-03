library(topicmodels)
library(tidyverse)
library(tidytext)
library(tidyr)
library(gutenbergr)
library(scales)

data("AssociatedPress")

AssociatedPress

# 1 CREATE LDA MODELS -----------------------------------------------------

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

## 1.1 Word-topic probabilities

ap_topics <- tidy(ap_lda, matrix = "beta")

# Probabilities of a teram for each topic

ap_top_terms <-
        ap_topics %>%
        group_by(topic) %>% 
        top_n(10, beta) %>% 
        ungroup() %>% 
        arrange(topic, -beta)

ap_top_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()

# Log ratio of probabilities between topics

beta_spread <-
        ap_topics %>% 
        mutate(topic = paste0("topic", topic)) %>% 
        spread(topic, beta) %>% 
        filter(topic1 > .001 | topic2 > 0.001) %>% 
        mutate(log_ratio = log2(topic2 / topic1))

## 1.2 Document-topic probabilities

# Proibability of a document is refered of wach topic

ap_documents <- tidy(ap_lda, matrix = "gamma")

# 2 EXAMPLE: GREAT LIBRARY HEIST ------------------------------------------

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

book <-
        gutenberg_works(title %in% titles) %>% 
        gutenberg_download(meta_fields = "title")


# divide into documents, each representing one chapter

by_chapter <-
        book %>% 
        group_by(title) %>% 
        mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
        ungroup() %>%
        filter(chapter > 0) %>%
        unite(document, title, chapter)

# split into words

by_chapter_word <- 
        by_chapter %>%
        unnest_tokens(word, text)

# find document-word counts

word_counts <- 
        by_chapter_word %>%
        anti_join(stop_words) %>%
        count(document, word, sort = TRUE) %>%
        ungroup()

## 2.1 LDA on chapters

# Create a Document-term matrix

chapters_dtm <-
        word_counts %>%
        cast_dtm(document, word, n)

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))

chapter_topics <- tidy(chapters_lda, matrix = "beta")

# Top 5 terms in each topic

top_terms <-
        chapter_topics %>% 
        group_by(topic) %>% 
        top_n(5, beta) %>% 
        ungroup() %>% 
        arrange(topic, -beta)

top_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()

## 2.2 Per-document classification

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")

# reseparate document into title and chapter

chapters_gamma <-
        chapters_gamma %>% 
        separate(document, c("title", "chapter"),
                 sep = "_",
                 convert = T)


# reorder titles in order of topic 1, topic 2, etc before plotting

chapters_gamma %>%
        mutate(title = reorder(title, gamma * topic)) %>%
        ggplot(aes(factor(topic), gamma)) +
        geom_boxplot() +
        facet_wrap(~ title)


chapter_classifications <- chapters_gamma %>%
        group_by(title, chapter) %>%
        top_n(1, gamma) %>%
        ungroup()

chapter_classifications

book_topics <- chapter_classifications %>%
        count(title, topic) %>%
        group_by(title) %>%
        top_n(1, n) %>%
        ungroup() %>%
        transmute(consensus = title, topic)

chapter_classifications %>%
        inner_join(book_topics, by = "topic") %>%
        filter(title != consensus)

## 2.3 By word assignments

assignments <- augment(chapters_lda, data = chapters_dtm)

# Words incorrectly classified

assignments <- 
        assignments %>%
        separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
        inner_join(book_topics, by = c(".topic" = "topic"))

assignments

# Confusion Matrix

assignments %>%
        count(title, consensus, wt = count) %>%
        group_by(title) %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(consensus, title, fill = percent)) +
        geom_tile() +
        scale_fill_gradient2(high = "red", label = percent_format()) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank()) +
        labs(x = "Book words were assigned to",
             y = "Book words came from",
             fill = "% of assignments")

# Common mistaken words

wrong_words <- assignments %>%
        filter(title != consensus)

wrong_words
