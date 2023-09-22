library(httr2)

# Create a logged-in API session object
session <- request("https://bsky.social/xrpc/com.atproto.server.createSession") |> 
  req_method("POST") |> 
  req_body_json(list(
    identifier = Sys.getenv("BSKY_USER"),
    password = Sys.getenv("BSKY_PASS")
  )) |> 
  req_perform() |> 
  resp_body_json()

# Do something to generate the text, like grab stuff from an RSS feed or whatever
text <- glue::glue(
  "Three random numbers: {numbers}", 
  numbers = paste0(round(runif(3, min = 1, max = 999 ), 0), collapse = ", ")
)

# Post the post
resp <- request("https://bsky.social/xrpc/com.atproto.repo.createRecord") |> 
  req_method("POST") |> 
  req_headers(Authorization = paste0("Bearer ", session$accessJwt)) |> 
  req_body_json(list(
    repo = session$did,
    collection = "app.bsky.feed.post",
    record = list(
      "$type" = "app.bsky.feed.post",
      text = text,
      createdAt = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%OS6Z"),
      langs = list("en-US")
    )
  ))

# Actually make the request and post the thing
resp |> req_perform()
