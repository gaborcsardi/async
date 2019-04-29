## ------------------------------------------------------------------------
library(async)
library(desc)
library(jsonlite)

## ------------------------------------------------------------------------
mirrors <- getCRANmirrors()
mirror_urls <- mirrors$URL[mirrors$CountryCode == "us"]

## ------------------------------------------------------------------------
response_time <- async(function(url) {
  http_head(url)$
    then(http_stop_for_status)$
    then(function(x) setNames(x[["times"]][["total"]], url))$
    catch(error = function(e) setNames(Inf, url))
})

## ------------------------------------------------------------------------
synchronise(response_time(mirror_urls[[1]]))
synchronise(response_time("https://httpbin.org/status/404"))

## ------------------------------------------------------------------------
fastest_urls <- async(function(urls) {
  reqs <- lapply(urls, response_time)
  when_some(10, .list = reqs)$
    then(function(x) sort(unlist(x)))
})

## ------------------------------------------------------------------------
synchronise(fastest_urls(mirror_urls))

## ------------------------------------------------------------------------
default_gh_headers <- function() {
  headers <- c("Accept" = "application/vnd.github.v3+json")
  pat <- Sys.getenv("GITHUB_PAT", "")
  if (pat != "") {
    headers <- c(headers, Authorization = paste("token", pat))
  }
  headers
}

## ------------------------------------------------------------------------
get_gh_description <- async(function(user, repo)  {
  desc_url <- paste0(
    "https://raw.githubusercontent.com/", user, "/", repo,
    "/master/DESCRIPTION")
  http_get(desc_url, headers = default_gh_headers())$
    then(http_stop_for_status)$
    then(function(resp) rawToChar(resp$content))$
    then(function(txt) desc::desc(text = txt))
})

## ------------------------------------------------------------------------
synchronise(get_gh_description("jeroen", "curl"))

## ---- error = TRUE-------------------------------------------------------
synchronise(get_gh_description("jeroen", "curlqwerty"))

## ---- error = TRUE-------------------------------------------------------
synchronise(get_gh_description("git", "git"))

## ------------------------------------------------------------------------
get_gh_sha <- async(function(user, repo) {
  commit_url <- paste0(
    "https://api.github.com/repos/", user, "/", repo, "/git/trees/master")
  http_get(commit_url, headers = default_gh_headers())$
    then(http_stop_for_status)$
    then(function(resp) {
      cdata <- jsonlite::fromJSON(rawToChar(resp$content),
                                  simplifyVector = FALSE)
      cdata$sha
    })
})

## ------------------------------------------------------------------------
synchronise(get_gh_sha("jeroen", "curl"))

## ------------------------------------------------------------------------
get_gh_download_url <- function(user, repo, sha) {
  paste0("https://api.github.com/repos/", user, "/", repo,
         "/zipball/", sha)
}

## ------------------------------------------------------------------------
resolve_gh <- async(function(slug) {
  slug <- strsplit(slug, "/")[[1]]
  user <- slug[[1]]
  repo <- slug[[2]]
  desc <- get_gh_description(user, repo)
  sha  <- get_gh_sha(user, repo)
  when_all(desc = desc, sha = sha)$
    then(function(x) {
      list(
        url = get_gh_download_url(user, repo, x$sha),
        description = x$desc)
    })
})

## ------------------------------------------------------------------------
synchronise(resolve_gh("jeroen/curl"))

## ------------------------------------------------------------------------
resolve_url <- async(function(url) {
  dir.create(tmpdir <- tempfile())
  dest <- file.path(tmpdir,  basename(url))
  http_get(url, file = dest)$
    then(http_stop_for_status)$
    then(function() {
      dest <- normalizePath(dest)
      list(
        url = paste("file://", normalizePath(dest)),
        description = desc::desc(dest))
    })
})

## ------------------------------------------------------------------------
curl20_url <- "https://cloud.r-project.org/src/contrib/Archive/curl/curl_2.0.tar.gz"
synchronise(resolve_url(curl20_url))

## ------------------------------------------------------------------------
res <- synchronise(when_all(
  resolve_gh("jeroen/curl"),
  resolve_gh("ropensci/magick"),
  resolve_url(curl20_url)
))
length(res)

## ------------------------------------------------------------------------
tryCatch(
  res <- synchronise(when_all(
    resolve_gh("jeroen/curl"),
    resolve_gh("ropensci/magick"),
    resolve_url(curl20_url),
    resolve_url("https://httpbin.org/delay/2")
  )),
  error = function(e) e
)

## ------------------------------------------------------------------------
res <- synchronise(when_all(
  resolve_gh("jeroen/curl"),
  resolve_gh("ropensci/magickfoooooobar")$catch(error = function(e) NULL),
  resolve_url(curl20_url),
  resolve_url("https://httpbin.org/status/401")$catch(error = function(e) NULL)
))
res[[1]]$description$get("Package")
res[[2]]
res[[3]]$description$get("Version")
res[[4]]

