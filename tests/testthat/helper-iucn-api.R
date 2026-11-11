skip_if_no_iucn_api_key <- function() {
  key <- trimws(Sys.getenv("IUCN_REDLIST_KEY", unset = ""))
  testthat::skip_if(
    !nzchar(key),
    "IUCN_REDLIST_KEY is not set; skipping the IUCN API test."
  )
}
