library(DBI)
library(RMariaDB)
library(dplyr)

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port = 3307,
                 user = "root",
                 password = "", 
                 dbname = "president_text_analysis")

setwd("file_path")

font_add(family = "a", regular = "Oswald-Regular.ttf")
showtext_auto()

# 1. Basic
# 1. TTR
print("--- [Basic] TTR ---")
source("Basic/TTR.R", encoding = "UTF-8")

# 2. word frequency
print("--- [Basic] word frequency ---")
source("Basic/Word frequency.R", encoding = "UTF-8")

# 3. avg_sentence
print("--- [Basic] avg sentence ---")
source("Basic/avg_sentence.R", encoding = "UTF-8")

# 4. pos ratio
print("--- [Basic] pos ratio ---")
source("Basic/pos ratio.R", encoding = "UTF-8")

# 5. pronoun analysis
print("--- [Basic] pronoun analysis ---")
source("Basic/pronoun.R", encoding = "UTF-8")

# 2. Advanced
# 1. co-occurance
print("--- [Advanced] co-occrance(ngram, correlation, network graph) ---")
source("Advanced/co-occurance.R", encoding = "UTF-8")

# 2. emtion analysis
print("--- [Advanced] emtion anlaysis ---")
source("Advanced/emotion analysis.R", encoding = "UTF-8")

# 3. odd ratio
print("--- [Advanced] odd ratio ---")
source("Advanced/odd_ratio.R", encoding = "UTF-8")

# 4. parse tree depth
print("--- [Advanced] parse tree depth ---")
source("Advanced/parse_tree_depth.R", encoding = "UTF-8")

# 5. sentiment
print("--- [Advanced] sentiment ---")
source("Advanced/sentiment.R", encoding = "UTF-8")

# 3. ML 

print("--- [ML] LDA ---")
source("ML/LDA.R", encoding = "UTF-8")

print("--- [ML] STM ---")
source("ML/STM.R", encoding = "UTF-8")