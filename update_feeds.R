cli::cli_h1("Update OCS feeds procedure")
cli::cli_alert_info("Update job start: {lubridate::now()}")

# Setup
cli::cli_alert("Configuring setup environment...")
# setwd("/mnt/data/onedrive/projetos/ocs_feed/")
setwd("/dados/home/rfsaldanha/ocs_feed/")

source(file = "collector_inmet.R")
source(file = "write_feed.R")
mun_feeds <- readRDS("mun_feeds.rds")

# Ref time
# ref_time <- as.POSIXct("2025-12-15 00:01")
# ref_time <- readRDS(file = "last_send_time.rds")

# Run collectors
cli::cli_alert("Running collectors...")
cli::cli_inform("INMET collector...")
inmet_entries <- collect_inmet(last_n = 10)
cli::cli_alert_success("INMET collector done!")

# Bind all entries
cli::cli_alert("Binding all entries...")
entries <- tibble::tibble()
entries <- dplyr::bind_rows(entries, inmet_entries)

if (nrow(entries) == 0) {
  stop("No new entries")
}

cli::cli_inform("{nrow(entries)} entries")

# Join mun feeds
cli::cli_alert("Join municipality feed codes...")
entries <- dplyr::left_join(entries, mun_feeds, by = "code_muni") |>
  dplyr::select(-code_muni)

# Convert to list
cli::cli_alert("Convert to list...")
entries <- entries |>
  dplyr::group_by(feed) |>
  dplyr::group_split() |>
  purrr::map(.f = purrr::transpose)

# Write feeds
cli::cli_alert("Write feed files...")
fs::dir_create(path = "/dados/home/rfsaldanha/ocs_feed/feeds")
res <- purrr::map(.x = entries, .f = write_feed, .progress = TRUE)

# Move feed files to webserver
cli::cli_alert("Move feed files to webserver dir...")
fs::file_delete(path = "/dados/htdocs/shiny.icict.fiocruz.br/ocs/feeds/")
fs::file_create("/dados/htdocs/shiny.icict.fiocruz.br/ocs/feeds/")
fs::file_move(
  path = "feeds/",
  new_path = "/dados/htdocs/shiny.icict.fiocruz.br/ocs/"
)

# Save last send time
# saveRDS(object = Sys.time(), file = "last_send_time.rds")

cli::cli_alert_info("Update job end: {lubridate::now()}")
cli::cli_h1("END")
