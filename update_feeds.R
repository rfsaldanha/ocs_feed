# Setup
# setwd("/mnt/data/onedrive/projetos/ocs_messenger/")
setwd("/dados/home/rfsaldanha/ocs_messenger/")

source(file = "collector_inmet.R")
source(file = "write_feed.R")
mun_feeds <- readRDS("mun_feeds.rds")

# Ref time
# ref_time <- as.POSIXct("2025-12-15 00:01")
# ref_time <- readRDS(file = "last_send_time.rds")

# Run collectors
inmet_entries <- collect_inmet(last_n = 10)

# Bind all entries
entries <- tibble::tibble()
entries <- dplyr::bind_rows(entries, inmet_entries)

if (nrow(entries) == 0) {
  stop("No new entries")
}

message(paste(nrow(entries), "entries."))
# Join mun feeds
entries <- dplyr::left_join(entries, mun_feeds, by = "code_muni") |>
  dplyr::select(-code_muni)

# Convert to list
entries <- entries |>
  dplyr::group_by(feed) |>
  dplyr::group_split() |>
  purrr::map(.f = purrr::transpose)

# Write feeds
res <- purrr::map(.x = entries, .f = write_feed, .progress = TRUE)

# Move feed files
fs::file_move(
  path = "feeds/",
  new_path = "/dados/htdocs/shiny.icict.fiocruz.br/feed/"
)

# Save last send time
saveRDS(object = Sys.time(), file = "last_send_time.rds")
