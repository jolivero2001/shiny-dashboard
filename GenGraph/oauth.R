library(httr)

# OAuth setup --------------------------------------------------------

# Most OAuth applications require that you redirect to a fixed and known
# set of URLs. Many only allow you to redirect to a single URL: if this
# is the case for, you'll need to create an app for testing with a localhost
# url, and an app for your deployed app.

if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://localhost:8100/"
} else {
  # deployed URL
  APP_URL <- "https://servername/path-to-app"
}

# Note that secret is not really secret, and it's fine to include inline
app <- oauth_app("shinygithub",
  key = "51d46f96810d1fd182a2",
  secret = "66eec8782825eeb61007dbef32f91afc9c3587aa",
  redirect_uri = APP_URL
)

# Here I'm using a canned endpoint, but you can create with oauth_endpoint()
api <- oauth_endpoints("github")

# Always request the minimal scope needed. For github, an empty scope
# gives read-only access to public info
scope <- ""

# Shiny -------------------------------------------------------------------

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}



