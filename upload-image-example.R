library(httr2)
library(ggplot2)

# Create a logged-in API session object
session <- request("https://bsky.social/xrpc/com.atproto.server.createSession") |> 
  req_method("POST") |> 
  req_body_json(list(
    identifier = Sys.getenv("BSKY_USER"),
    password = Sys.getenv("BSKY_PASS")
  )) |> 
  req_perform() |> 
  resp_body_json()

# Send a plot to Bluesky!
random_beta_plot <- function() {
  # Egypt color palette from MetBrewer
  clrs <- c("#dd5129", "#0f7ba2", "#43b284", "#fab255")
  
  shape1 <- round(runif(1, min = 1, max = 10), 1)
  shape2 <- 1 + rpois(1, lambda = 2)
  
  dist_details <- glue::glue("Beta({shape1}, {shape2})")
  
  dist_plot <- ggplot() +
    stat_function(
      geom = "area",
      fun = \(x) dbeta(x, shape1 = shape1, shape2 = shape2),
      fill = sample(clrs, 1)
    ) +
    labs(title = dist_details) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  # There's probably a better way to convert this object to a binary version of
  # a png, but this works
  
  # Create a temporary file
  plot_file <- tempfile()
  
  # Save the plot there
  ggsave(plot_file, dist_plot, width = 4, height = 2.5, device = "png")
  
  # Read that temporary file as a binary object
  plot_bytes <- readBin(plot_file, "raw", file.info(plot_file)$size)
  
  # Get rid of the temporary file
  unlink(plot_file)
  
  # Return a bunch of stuff
  return(list(dist_plot = dist_plot, dist_details = dist_details, raw = plot_bytes))
}

# Create a plot
thing <- random_beta_plot()
thing$dist_plot
thing$dist_details
thing$raw


# Upload image to Bluesky
img_resp <- request("https://bsky.social/xrpc/com.atproto.repo.uploadBlob") |> 
  req_method("POST") |> 
  req_headers(
    `Content-Type` = "img/png",
    Authorization = paste0("Bearer ", session$accessJwt)
  ) |> 
  req_body_raw(thing$raw) |> 
  req_perform()

uploaded_images <- resp_body_json(img_resp)

full_post <- list(
  "$type" = "app.bsky.feed.post",
  text = glue::glue("A randomly generated {x} distribution", x = thing$dist_details),
  createdAt = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%OS6Z"),
  langs = list("en-US"),
  embed = list(
    "$type" = "app.bsky.embed.images",
    images = list(list(
      alt = thing$dist_details,
      image = uploaded_images$blob
    ))
  )
)

# Post the post
resp <- request("https://bsky.social/xrpc/com.atproto.repo.createRecord") |> 
  req_method("POST") |> 
  req_headers(Authorization = paste0("Bearer ", session$accessJwt)) |> 
  req_body_json(list(
    repo = session$did,
    collection = "app.bsky.feed.post",
    record = full_post
  ))

# Actually make the request and post the thing
# resp |> req_dry_run()
resp |> req_perform()
