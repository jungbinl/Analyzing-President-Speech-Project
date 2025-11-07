library(udpipe)
library(dplyr)
library(stringr)

total_data <- read.csv("total_raw_data.csv")

total_speech = udpipe_annotate(ud_model, x = total_data$document) %>% as.data.frame() 
total_speech <- total_speech %>% mutate(doc_id = str_replace_all(doc_id, "[^0-9]", ""), doc_id = str_squish(doc_id), doc_id = as.numeric(doc_id))
total_speech <- total_speech %>% left_join(total_data, by = "doc_id")
total_speech$token <- tolower(total_speech$token)
total_speech$token <- lemmatize_words(total_speech$token)

after_1960_data <- read.csv("after_1960_raw_data")

after_1960_speech = udpipe_annotate(ud_model, x = after_1960_data$document) %>% as.data.frame()
after_1960_speech = after_1960_speech %>% filter(str_count(token) > 1)
after_1960_speech <- after_1960_speech %>% mutate(doc_id = str_replace_all(doc_id, "[^0-9]", ""), doc_id = str_squish(doc_id), doc_id = as.numeric(doc_id))
after_1960_speech <- after_1960_speech %>% left_join(after_1960_data, by = "doc_id")
after_1960_speech$token <- tolower(after_1960_speech$token)
after_1960_speech$token <- lemmatize_words(after_1960_speech$token)

write.csv(total_data, "total_speech_data.csv", row.names = FALSE)
write.csv(after_1960_data, "after_1960_speech.csv", row.names = FALSE)
