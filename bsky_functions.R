library(pacman)

pacman::p_load(bskyr, dplyr, httr2)

# YOU NEED TO DECLARE 3 VALUES IN YOUR .Renviron file
# BLUESKY_APP_USER=bluesky_user.bsky.social
# BLUESKY_APP_PASS=hhh-6667-hgfr-7777
# BLUSKY_DID_ME=did:plc:jshadjashdsj6jashdkjsa5ksajdklsa3

filter_list <- c("DEMOCRACY", "NAFO", "FELLA", "UKRAINE", "MUGA", "MAGA", "RUZZIA", 
                 "VATNIK", "SLAVA UKRAINI", "PROPAGANDA", "CANADIAN", "CANADA", "QUEBEC", "🇺🇦", "🌻",
                 "💙", "💛", "NO DM", "🇨🇦", "🍁", "DEMOCRACY", "EMPATH", "#AI", "#ML", "LGBTQ+", "Progressive",
                 "🌈", "🇪🇺")



block_list <- c("ONLYFAN", "SEX", "NUDE", "PORN", "BLONDE", "ADULT FILM")

myDid <- Sys.getenv("BLUESKY_DID_ME")

followerLimit <- 7000

##################################################
# GET SESSION FCT
##################################################
getSession <- function() {
  session <- request("https://bsky.social/xrpc/com.atproto.server.createSession") |> 
    req_method("POST") |> 
    req_body_json(list(
      identifier = Sys.getenv("BLUESKY_APP_USER"),
      password = Sys.getenv("BLUESKY_APP_PASS")
    )) |> req_perform() |> resp_body_json()
  
  return(session)
}


getNotFollwed <- function() {
  
  followers <- bskyr::bs_get_followers(Sys.getenv("BLUESKY_APP_USER"), limit=followerLimit, clean = TRUE)
  
  print(paste("Number of followers : ", nrow(followers)))
  not_followed <- followers %>% filter(is.na(viewer_following))
  not_followed <- unique( not_followed[ , 1:17 ] )
  print(paste("Number of users not followed: ", nrow(not_followed)))
  
  return(not_followed)
}

##################################################
# FOLLOW USER FCT
##################################################

follow_user <- function(did) {
  # Create a post to follow
  post_body <- list(
    "$type" = "app.bsky.graph.follow",
    subject = did,
    createdAt = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%OS6Z")
  )
  
  # Post the post
  # Make the API request

  request <- request("https://bsky.social/xrpc/com.atproto.repo.createRecord") %>%
    req_headers(Authorization = paste("Bearer", session$accessJwt)) %>% 
    req_body_json(list(
      repo = session$did,
      collection = "app.bsky.graph.follow",
      record = post_body))
  
    response <- tryCatch({
    request %>% req_perform()},
    warning = function(war) {print(paste(war))},
    error = function(err) {print(err)}
    )
  
  return(response)
}

##################################################
# FOLLOW FOLLOWERS FCT
##################################################

follow_not_followed <- function(not_followed) {
 if (nrow(not_followed) == 0) {
    print("No users to follow")
    return(0)
  }
  
  for (i in 1:nrow(not_followed)) {
    print(paste("Adding follower: ", not_followed[i,]$did))
    response <- follow_user(not_followed[i,]$did)
    Sys.sleep(sample(1:10, 1))
    print(paste("Status code ", response$status_code))
    print(paste("Followed ", not_followed[i,]$did, " ***** ", not_followed[i,]$display_name))
  }
}

unfollow_actor <- function(did) {
  # Create a post to follow
  post_body <- list(
    "$type" = "app.bsky.graph.unfollow",
    subject = did,
    createdAt = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%OS6Z")
  )
  
  # Post the post
  # Make the API request
  response <- request("https://bsky.social/xrpc/com.atproto.repo.createRecord") %>%
    req_headers(Authorization = paste("Bearer", session$accessJwt)) %>% 
    req_body_json(list(
      repo = session$did,
      collection = "app.bsky.graph.unfollow",
      record = post_body)) %>% 
    req_perform()
  
  return(response)
}

blockUsers <- function(not_followed){
  if (nrow(not_followed) == 0) {
    print("No users to block")
    return(0)
  }
  
  for (i in 1:nrow(not_followed)) {
    print(paste("Blocking user: ", not_followed[i,]$did))
    response <- blockUser(not_followed[i,]$did)
    Sys.sleep(sample(1:10, 1))
    print(paste("Status code ", response$status_code))
    print(paste("Blocked ", not_followed[i,]$did, " ***** ", not_followed[i,]$display_name))
  }  
  
}

blockUser <- function(did){
  # Create a post to follow
  post_body <- list(
    "$type" = "app.bsky.graph.block",
    subject = did,
    createdAt = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%OS6Z")
  )
  
  # Post the post
  # Make the API request
  response <- request("https://bsky.social/xrpc/com.atproto.repo.createRecord") %>%
    req_headers(Authorization = paste("Bearer", session$accessJwt)) %>% 
    req_body_json(list(
      repo = session$did,
      collection = "app.bsky.graph.block",
      record = post_body)) %>% 
    req_perform()
  
  return(response)
  
}


