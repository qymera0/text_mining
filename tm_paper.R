library(tm)

# 01 DATA IMPORT ----------------------------------------------------------

# Ovid data

txt <- system.file("texts", "txt", package = "tm")

ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"),
                readerControl = list(language = "lat"))

# Reuters data

reutCrude <- system.file("texts", "crude", package = "tm")

reutersCrude <- VCorpus(DirSource(reutCrude, mode = "binary"),
                   readerControl = list(reader = readReut21578XMLasPlain)) # The reader already transform to plain text

reutAcq <- system.file("texts", "acq", package = "tm")

reutersAcq <- VCorpus(DirSource(reutAcq, mode = "binary"),
                      readerControl = list(reader = readReut21578XMLasPlain))

# 02 INPSECTING CORPORA ---------------------------------------------------

# Ovid data

inspect(ovid[1:2])

# Accessing individual documents

meta(ovid[[2]], "id")

identical(ovid[[2]], ovid[["ovid_2.txt"]])

# Print a specific text from corpus

inspect(ovid[[2]])

# Transform a text to character

lapply(ovid[1:2], as.character)

# 03 TRANSFORMATIONS ------------------------------------------------------

# Eliminate white spaces

reutersCrude <- tm_map(reutersCrude, stripWhitespace)

reutersAcq <- tm_map(reutersAcq, stripWhitespace)

# Convert to lower case

reutersCrude <- tm_map(reutersCrude, content_transformer(tolower))

reutersAcq <- tm_map(reutersAcq, content_transformer(tolower))

# Remove stop words

reutersCrude <- tm_map(reutersCrude, removeWords, stopwords("english"))

reutersAcq <- tm_map(reutersAcq, removeWords, stopwords("english"))

# Stemming

reutersCrude <- tm_map(reutersCrude, stemDocument)

reutersAcq <- tm_map(reutersAcq, stemDocument)

# 04 FILTERS --------------------------------------------------------------

# create a index to filter after

idx <- meta(reutersCrude, "id") == '237' & meta(reuters, "heading") == 'INDONESIA SEEN AT CROSSROADS OVER ECONOMIC CHANGE'

reuters[idx]

# 05 META DATA MANAGEMENT -------------------------------------------------

DublinCore(reutersCrude[[1]], "Creator") <- "Ano Nymous" # Changes the value of a metadata

meta(reutersCrude[[1]])

meta(reutersCrude, tag = "test", type = "corpus") <- "test meta"

meta(reutersCrude, type = "corpus")

meta(reutersCrude, "foo") <- letters[1:20]

meta(reutersCrude)
