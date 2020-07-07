
library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringr)
library(gutenbergr)
library(scales)

# 1.2 unnest_tokens FUNCTION ----------------------------------------------

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

# Convert text to a tibble

text_df <- tibble(line = 1:4,
                  text = text)

text_df

# Unnest tokens

text_df %>%
        unnest_tokens(word, text)

# 1.3 TIDYING JANE AUSTEN -------------------------------------------------

original_books <-
        austen_books() %>%
        group_by(book) %>%
        mutate(lineNumber = row_number(),
               chapter = cumsum(str_detect(text, 
                                           regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
        ungroup()

# Tokenize

tidy_books <-
        original_books %>%
        unnest_tokens(word, text)
        

# Remove stop words

data(stop_words)

tidy_books <-
        tidy_books %>%
        anti_join(stop_words)

# Count words

tidy_books %>%
        count(word, sort = TRUE)

# Vizualize

tidy_books %>%
        count(word, sort = TRUE) %>%
        filter(n > 600) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) + 
        geom_col() + 
        xlab(NULL) + 
        coord_flip()

# 1.4 THE GUTENBERGR PACKAGE ----------------------------------------------

# Download H G Wells books

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <-
        hgwells %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)

tidy_hgwells %>%
        count(word, sort = TRUE)

# Download Bronte Sisters

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)

tidy_bronte %>%
        count(word, sort = TRUE)

# Binding data sets together to compare with Jane Austeen

frequency <-
        bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                  mutate(tidy_hgwells, author = "H.G. Wells"), 
                  mutate(tidy_books, author = "Jane Austen"))%>%
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(author, word) %>%
        group_by(author) %>%
        mutate(proportion = n / sum(n)) %>%
        select(-n) %>%
        spread(author, proportion) %>%
        gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

# Plot the relations between authors

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
        facet_wrap(~author, ncol = 2) +
        theme(legend.position="none") +
        labs(y = "Jane Austen", x = NULL)

# Estimate the correlation between words

cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)
