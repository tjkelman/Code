library(googlesheets)
library(likert)
library(dplyr)

sheet_meta <- gs_key("1PK4L9iS4tZzQvKlhQxtntJOAUaFH9kytTp5EFb8Ustk", lookup = FALSE, visibility = "private")
pre_data <- gs_read(sheet_meta, ws = "Pre", col_names = TRUE, skip = 0, verbose = TRUE, literal = FALSE)
post_data <- gs_read(sheet_meta, ws = "Post", col_names = TRUE, skip = 0, verbose = TRUE, literal = FALSE)

all_data <- bind_rows(pre_data, post_data) 

likert_data <- all_data %>%
  select(husbandry_rank:pre_post) %>% 
  mutate_at(vars(contains("rank")), 
            ~factor(., levels = c("Not knowledgeable", "Somewhat knowledgeable",
                                  "Moderately knowledgeable", "Knowledgeable", 
                                   "Highly knowledgeable"))) %>% 
  mutate(pre_post = factor(pre_post, levels = c("post", "pre"),
                           labels = c("Post-workshop", "Pre-workshop")))

colnames(likert_data)[1:7] <- c("Husbandry practices", "Environment/Land management",
                                "Housing & equipment", "Predator management",
                                "Food safety", "Economics & profitabilty",
                                "Marketing")  
likert_obj <- likert(as.data.frame(select(likert_data, 1:7)), grouping = likert_data$pre_post)
likert.bar.plot(likert_obj)
ggsave("../Plots/likert_plot.png", width = 10, height = 10)
