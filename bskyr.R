source("bsky_functions.R")

session <- getSession()

not_followed <- getNotFollwed()

filtered_not_follow <- not_followed %>% 
  filter(did != myDid) %>%
  filter(grepl(paste(filter_list, collapse = "|"),toupper(description))
         | 
         grepl(paste(filter_list, collapse = "|"),toupper(display_name))
         )
  

print(paste("Filtered to follow: ", nrow(filtered_not_follow)))

follow_not_followed(filtered_not_follow)

filtered_not_follow_to_block <- not_followed %>% 
  filter(did != myDid) %>%
  filter(grepl(paste(block_list, collapse = "|"),toupper(description))) %>%
  filter(!did %in% filtered_not_follow$did)

#blockUsers(filtered_not_follow_to_block)
