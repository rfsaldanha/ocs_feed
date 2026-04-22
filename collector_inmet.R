collect_inmet <- function() {
  # INMET parquet file address
  parquet_url <- "https://inmetalerts.nyc3.digitaloceanspaces.com/inmetalerts.parquet"
  # parquet_url <- "~/Downloads/inmetalerts.parquet"

  # Read data
  res <- arrow::read_parquet(parquet_url) |>
    dplyr::distinct()

  if (nrow(res) == 0) {
    return(NULL)
  }

  # Parse inmet data
  res <- inmetrss::parse_mun(res, text = TRUE)

  # Filter last n entries per municipality
  # res <- res |>
  #   dplyr::group_by(mun_codes) |>
  #   dplyr::arrange(dplyr::desc(sent)) |>
  #   dplyr::slice_head(n = last_n) |>
  #   dplyr::ungroup()

  # Filter current alerts
  res <- res |>
    dplyr::filter(Sys.time() >= sent & Sys.time() <= expires)

  # Prepare message data
  message_df <- res |>
    dplyr::mutate(title = glue::glue("Alerta de {tolower(event)} (INMET)")) |>
    dplyr::mutate(
      message = paste(description, instruction),
      date = sent,
    ) |>
    dplyr::select(
      identifier,
      date,
      event,
      severity,
      code_muni = mun_codes,
      title,
      message
    )

  return(message_df)
}
