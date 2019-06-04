library(googlesheets)
library(likert)
library(dplyr)
library(tidyr)
library(purrr)

sheet_meta <- gs_key("1PK4L9iS4tZzQvKlhQxtntJOAUaFH9kytTp5EFb8Ustk", lookup = FALSE, visibility = "private")
pre_data <- gs_read(sheet_meta, ws = "Pre", col_names = TRUE, skip = 0, verbose = TRUE, literal = FALSE)
post_data <- gs_read(sheet_meta, ws = "Post", col_names = TRUE, skip = 0, verbose = TRUE, literal = FALSE)

pre_unique_data <- pre_data %>% 
  distinct(name, .keep_all = TRUE)
post_unique_data <- post_data %>% 
  distinct(name, .keep_all = TRUE)

all_data <- bind_rows(pre_unique_data, post_unique_data)    

likert_data <- all_data %>%
  select(husbandry_rank:pre_post) %>% 
  mutate_at(vars(contains("rank")), 
            ~factor(., levels = c("Not knowledgeable", "Somewhat knowledgeable",
                                  "Moderately knowledgeable", "Knowledgeable", 
                                   "Highly knowledgeable"))) %>% 
  mutate(pre_post = factor(pre_post, levels = c("post", "pre"),
                           labels = c("Post-workshop", "Pre-workshop"))) %>% 
  drop_na()

colnames(likert_data)[1:7] <- c("Husbandry practices", "Environment/Land management",
                                "Housing & equipment", "Predator management",
                                "Food safety", "Economics & profitabilty",
                                "Marketing")  

likert_obj <- likert(as.data.frame(select(likert_data, 1:7)), grouping = likert_data$pre_post)
p <- likert.bar.plot(likert_obj)

paired_data <- pre_data %>% 
  inner_join(post_data, by = c("name", "workshop_date"), 
             suffix = c("_pre", "_post")) %>% 
  drop_na() %>% 
  mutate_at(vars(contains("rank")), 
            ~factor(., levels = c("Not knowledgeable", "Somewhat knowledgeable",
                                  "Moderately knowledgeable", "Knowledgeable", 
                                  "Highly knowledgeable")))

wt_pre <- paired_data %>% 
  select(husbandry_rank_pre:marketing_rank_pre)
wt_post <- paired_data %>% 
  select(husbandry_rank_post:marketing_rank_post)

wt_results <- 
  map2(wt_pre, wt_post, ~ wilcox.test(as.numeric(.x), as.numeric(.y), paired=TRUE)) %>% 
  set_names(~ str_extract(., "[:graph:]+(?=_pre)"))
  
label <- wt_results %>% 
  map_chr("p.value") %>% 
  paste0("p = ", .)

dat_text <- data.frame(
  label = label,
  Item   = p$data$Item[1:7]
)

p + geom_text(
  data    = dat_text,
  mapping = aes(x = 1.5, y= 75, label = label),
  inherit.aes = FALSE
)

ggsave("../Plots/likert_plot.png", width = 10, height = 10)
