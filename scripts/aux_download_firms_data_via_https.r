#!/usr/bin/env Rscript

# deps: install.packages(c("httr2","rvest","xml2"))
suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(xml2)
})

# ---- Config ----
# Read EDL token from env (set in GH Actions secret or your shell)
edl_token <- Sys.getenv("EARTHDATA_TOKEN")

if (!nzchar(edl_token)) {
  stop("EARTHDATA_TOKEN env var is missing.")
}

# Local base dir that already contains many FIRMS .txt files (per system)
# You can override via env; default "data"
local_base <- "source-data/firms-imports/2025/"
local_dir  <- local_base  # single shared directory across systems

# The four systems (Global)
systems <- c(
  "modis-c6.1",
  "suomi-npp-viirs-c2",
  "noaa-20-viirs-c2",
  "noaa-21-viirs-c2"
)

base_url <- "https://nrt3.modaps.eosdis.nasa.gov/archive/FIRMS"

# ---- Helpers ----

# Extract the trailing numeric part just before .txt (returns NA if not matched)
trailing_num <- function(x) {
  m <- regexpr("(\\d+)(?=\\.txt$)", x, perl = TRUE)
  ifelse(m > 0, as.integer(regmatches(x, m)), NA_integer_)
}

# Given a local directory, find the current max trailing number
local_max_num <- function(dir_path) {
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(dir_path, pattern = "\\.txt$", full.names = FALSE)
  if (!length(files)) return(0L)
  nums <- trailing_num(files)
  nums <- nums[!is.na(nums)]
  if (!length(nums)) return(0L)
  max(nums)
}

# GET HTML of a directory listing with auth header
fetch_listing <- function(url, token) {
  req <- request(url) |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_timeout(60)
  resp <- req_perform(req)
  resp_body_string(resp)
}

# Parse listing page and return vector of file names ending with .txt
parse_txt_files <- function(html_text) {
  doc <- read_html(html_text)
  hrefs <- html_attr(html_elements(doc, "a"), "href")
  # most listings include file names in anchor text; filter .txt
  files <- hrefs[grepl("\\.txt$", hrefs, ignore.case = TRUE)]
  basename(files)
}

# Download a single file to dest using httr2 (preserves filename)
download_file <- function(url, dest_dir, token) {
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  fn <- basename(url)
  dest <- file.path(dest_dir, fn)
  if (file.exists(dest)) return(invisible(dest))
  req <- request(url) |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_timeout(120) |>
    req_error(is_error = function(resp) FALSE) # don't throw; let us handle
  resp <- req_perform(req)
  if (resp_status(resp) >= 400) {
    warning(sprintf("Failed %s -> HTTP %s", fn, resp_status(resp)))
    return(invisible(NULL))
  }
  writeBin(resp_body_raw(resp), dest)
  dest
}

# ---- Main ----
all_downloaded <- list()

# Find the most recent file in the data, re-download it (in case partial) and all files published since
max_local <- local_max_num(local_dir)-1
# current local max trailing number
cat("Local dir:", local_dir, " | Max trailing number:", max_local, "\n")

for (sys in systems) {
  cat("\n=== System:", sys, "===\n")
  remote_dir <- sprintf("%s/%s/Global/", base_url, sys)
  local_dir  <- file.path(local_base)


  # fetch and parse remote listing
  listing_html <- fetch_listing(remote_dir, edl_token)
  remote_files <- parse_txt_files(listing_html)

  if (!length(remote_files)) {
    cat("No .txt files visible in listing.\n")
    next
  }

  # keep files with trailing number > max_local
  nums <- trailing_num(remote_files)
  keep <- !is.na(nums) & nums > max_local
  new_files <- remote_files[keep]
  new_nums  <- nums[keep]

  if (!length(new_files)) {
    cat("Up to date. Nothing to download.\n")
    next
  }

  ord <- order(new_nums) # download oldest first
  new_files <- unique(new_files[ord]) # Only download files once

  cat("Downloading", length(new_files), "files...\n")

  downloaded <- character(0)
  for (f in new_files) {
    url <- paste0(remote_dir, f)
    path <- download_file(url, local_dir, edl_token)
    if (!is.null(path)) {
      cat("  ✓ ", basename(path), "\n", sep = "")
      downloaded <- c(downloaded, path)
    } else {
      cat("  ✗ ", f, "\n", sep = "")
    }
  }
  all_downloaded[[sys]] <- downloaded
}

cat("\nDone.\n")
invisible(all_downloaded)
