library(tidyverse)
library(tidytext)
library(textdata)
library(janeaustenr)
library(stringr)
library(wordcloud)
library(reshape2)

# 2.1 SENTIMENTS DATASET --------------------------------------------------

get_sentiments("afinn")

get_sentiments("bing")

get_sentiments("nrc")


# 2.2 SENTIMENT ANALYSIS WITH INNER JOIN ----------------------------------

tidy_books <- 
        austen_books() %>%
        group_by(book) %>%
        mutate(linenumber = row_number(),
               chapter = cumsum(str_detect(text, 
                                           regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
        ungroup() %>%
        unnest_tokens(word, text)

# Filter joy words

nrc_joy <-
        get_sentiments("nrc") %>%
        filter(sentiment == "joy")

tidy_books %>%
        filter(book == "Emma") %>%
        inner_join(nrc_joy) %>%
        count(word, sort = T)

# Sentiment analysis for each 80 lines. The sentiment score is the 
# difference between the positive and negative counts of words.

jane_austen_sentiment <-
        tidy_books %>%
        inner_join(get_sentiments("bing")) %>%
        count(book, index = linenumber %/% 80, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~book, ncol = 2, scales = "free_x")

# 2.3 COMPARING THREE SENTIMENT DICTIONARIES ------------------------------

pride_prejudice <-
        tidy_books %>%
        filter(book == "Pride & Prejudice")

pride_prejudice

afinn <- pride_prejudice %>% 
        inner_join(get_sentiments("afinn")) %>% 
        group_by(index = linenumber %/% 80) %>% 
        summarise(sentiment = sum(value)) %>% 
        mutate(method = "AFINN")

bing_and_nrc <- bind_rows(pride_prejudice %>% 
                                  inner_join(get_sentiments("bing")) %>%
                                  mutate(method = "Bing et al."),
                          pride_prejudice %>% 
                                  inner_join(get_sentiments("nrc") %>% 
                                                     filter(sentiment %in% c("positive", 
                                                                             "negative"))) %>%
                                  mutate(method = "NRC")) %>%
        count(method, index = linenumber %/% 80, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)

# compare each lexicon

bind_rows(afinn, 
          bing_and_nrc) %>%
        ggplot(aes(index, sentiment, fill = method)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~method, ncol = 1, scales = "free_y")

# Bias between lexicons

get_sentiments("nrc") %>%
        filter(sentiment %in% c("positive",
                                "negative")) %>%
        count(sentiment)

get_sentiments("bing") %>%
        count(sentiment)

# 2.4 MOST COMMOM POSITIVE AND NEGATIVE WORDS -----------------------------

bing_word_counts <-
        tidy_books %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = T) %>%
        ungroup()

bing_word_counts

bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()

# Add miss to custom stop words

custom_stop_words <- bind_rows(tibble(word = c("miss"), 
                                      lexicon = c("custom")), 
                               stop_words)

# 2.5 WORD CLOUDS ---------------------------------------------------------

tidy_books %>%
        anti_join(stop_words) %>%
        count(word) %>%
        with(wordcloud(word, n, max.words = 100))

# Word clouds for positive / negative words

tidy_books %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = T) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("gray20", "gray80",
                                    max.words = 100))

# 2.6 LOOKING AT UNITS BEYOND JUST WORDS ----------------------------------

# tokenize in sentences usint period

PandP_sentences <-
        tibble(text = prideprejudice) %>%
        unnest_tokens(sentence, text, token = "sentences")

austen_chapters <-
        austen_books() %>%
        group_by(book) %>%
        unnest_tokens(chapter, text, token = "regex",
                      pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
        ungroup()

austen_chapters %>%
        group_by(book) %>%
        summarise(chapters = n())

bingnegative <-
        get_sentiments("bing") %>%
        filter(sentiment == "negative")

wordcounts <- 
        tidy_books %>%
        group_by(book, chapter) %>%
        summarize(words = n())

# Ratio of negative words by chapter

tidy_books %>%
        semi_join(bingnegative) %>%
        group_by(book, chapter) %>%
        summarize(negativewords = n()) %>%
        left_join(wordcounts, by = c("book", "chapter")) %>%
        mutate(ratio = negativewords/words) %>%
        filter(chapter != 0) %>%
        top_n(1) %>%
        ungroup()


