library(tidyverse)
library(tidytext)
library(tm)

# 5.1 TIDYING A DOCUMENT-TERM MATRIX --------------------------------------


data("AssociatedPress", package = "topicmodels") # already a DTM

AssociatedPress

terms <- Terms(AssociatedPress)

ap_td <- tidy(AssociatedPress)

ap_sentiments <-
        ap_td %>%
        inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments %>%
        count(sentiment, term, wt = count) %>%
        ungroup() %>%
        filter(n >= 200) %>%
        mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
        mutate(term = reorder(term, n)) %>%
        ggplot(aes(term, n, fill = sentiment)) +
        geom_bar(stat = "identity") +
        ylab("Contribution to sentiment") +
        coord_flip()

# 5.2 CASTING TIDY TEXT DATA INTO A MATRIX --------------------------------

ap_td %>%
        cast_dtm(document, term, count)

# 5.3 TIDYNG CORPUS WITH METADATA -----------------------------------------

data("acq")

acq

acq[[1]]

acq_td <- tidy(acq)


acq_tokens<- 
        acq_td %>%
        select(-places) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words, by = "word")

acq_tokens %>%
        count(word, sort = TRUE)

acq_tokens %>%
        count(id, word)%>%
        bind_tf_idf(word, id, n)%>%
        arrange(desc(tf_idf))

