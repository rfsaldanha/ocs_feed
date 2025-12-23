collect_monitorar_saude <- function() {
  # Connect to database (read only)
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    "../camsdata/forecast_data/cams_forecast.duckdb",
    read_only = TRUE
  )

  # Tables
  tb_pm25 <- "pm25_mun_forecast"
  tb_o3 <- "o3_mun_forecast"
  tb_iqar <- "iqar_mun_forecast"
  tb_temp <- "temp_mun_forecast"
  tb_uv <- "uv_mun_forecast"
  tb_wind_speed <- "wind_speed_mun_forecast"
  tb_prec <- "prec_mun_forecast"

  # PM2.5
  message_pm25 <- dplyr::tbl(con, tb_pm25) |>
    dplyr::group_by(code_muni) |>
    dplyr::arrange(date) |>
    dplyr::filter(value >= 15) |>
    dplyr::ungroup() |>
    dplyr::arrange(code_muni, date) |>
    dplyr::collect() |>
    dplyr::mutate(date = lubridate::date(date)) |>
    dplyr::select(code_muni, date) |>
    dplyr::distinct() |>
    dplyr::group_by(code_muni) |>
    dplyr::summarise(
      dates = list(as.character(format(date, "%d/%m/%Y (%A)")))
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(code_muni) |>
    dplyr::mutate(dates = paste(unlist(dates), collapse = ", ")) |>
    dplyr::mutate(
      code_muni = as.character(code_muni),
      identifier = uuid::UUIDgenerate(),
      title = "Alerta de PM2.5",
      message = glue::glue(
        "Estima-se que nos próximos sete dias o município apresente concentrações de PM 2.5 acima dos valores recomendados pela Organização Mundial da Saúde (OMS) nas seguinte(s) data(s): {dates}."
      )
    ) |>
    dplyr::select(identifier, code_muni, title, message)

  # O3
  message_o3 <- dplyr::tbl(con, tb_o3) |>
    dplyr::group_by(code_muni) |>
    dplyr::arrange(date) |>
    dplyr::filter(value >= 100) |>
    dplyr::ungroup() |>
    dplyr::arrange(code_muni, date) |>
    dplyr::collect() |>
    dplyr::mutate(date = lubridate::date(date)) |>
    dplyr::select(code_muni, date) |>
    dplyr::distinct() |>
    dplyr::group_by(code_muni) |>
    dplyr::summarise(
      dates = list(as.character(format(date, "%d/%m/%Y (%A)")))
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(code_muni) |>
    dplyr::mutate(dates = paste(unlist(dates), collapse = ", ")) |>
    dplyr::mutate(
      code_muni = as.character(code_muni),
      identifier = uuid::UUIDgenerate(),
      title = "Alerta de O3",
      message = glue::glue(
        "Estima-se que nos próximos sete dias o município apresente concentrações de O3 (ozônio) acima dos valores recomendados pela Organização Mundial da Saúde (OMS) nas seguinte(s) data(s): {dates}."
      )
    ) |>
    dplyr::select(identifier, code_muni, title, message)

  # IQAr
  message_iqar <- dplyr::tbl(con, tb_iqar) |>
    dplyr::group_by(code_muni) |>
    dplyr::arrange(date) |>
    dplyr::filter(value >= 40) |>
    dplyr::ungroup() |>
    dplyr::arrange(code_muni, date) |>
    dplyr::collect() |>
    dplyr::mutate(date = lubridate::date(date)) |>
    dplyr::select(code_muni, date) |>
    dplyr::distinct() |>
    dplyr::group_by(code_muni) |>
    dplyr::summarise(
      dates = list(as.character(format(date, "%d/%m/%Y (%A)")))
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(code_muni) |>
    dplyr::mutate(dates = paste(unlist(dates), collapse = ", ")) |>
    dplyr::mutate(
      code_muni = as.character(code_muni),
      identifier = uuid::UUIDgenerate(),
      title = "Alerta de qualidade do ar (IQAr)",
      message = glue::glue(
        "Estima-se que nos próximos sete dias o município apresente o Índice de Qualidade do Ar (IQAr) com valores acima dos recomendados pelo CONAMA nas seguinte(s) data(s): {dates}."
      )
    ) |>
    dplyr::select(identifier, code_muni, title, message)

  # UV
  message_uv <- dplyr::tbl(con, tb_uv) |>
    dplyr::group_by(code_muni) |>
    dplyr::arrange(date) |>
    dplyr::filter(value >= 6) |>
    dplyr::ungroup() |>
    dplyr::arrange(code_muni, date) |>
    dplyr::collect() |>
    dplyr::mutate(date = lubridate::date(date)) |>
    dplyr::select(code_muni, date) |>
    dplyr::distinct() |>
    dplyr::group_by(code_muni) |>
    dplyr::summarise(
      dates = list(as.character(format(date, "%d/%m/%Y (%A)")))
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(code_muni) |>
    dplyr::mutate(dates = paste(unlist(dates), collapse = ", ")) |>
    dplyr::mutate(
      code_muni = as.character(code_muni),
      identifier = uuid::UUIDgenerate(),
      title = "Alerta de raios ultravioletas",
      message = glue::glue(
        "Estima-se que nos próximos sete dias o município apresente o Índice de Raios Ultravioletas (IUV) com valores acima dos recomendados nas seguinte(s) data(s): {dates}."
      )
    ) |>
    dplyr::select(identifier, code_muni, title, message)

  # Temp 35°C
  message_temp_35 <- dplyr::tbl(con, tb_temp) |>
    dplyr::group_by(code_muni) |>
    dplyr::arrange(date) |>
    dplyr::filter(value >= 35) |>
    dplyr::ungroup() |>
    dplyr::arrange(code_muni, date) |>
    dplyr::collect() |>
    dplyr::mutate(date = lubridate::date(date)) |>
    dplyr::select(code_muni, date) |>
    dplyr::distinct() |>
    dplyr::group_by(code_muni) |>
    dplyr::summarise(
      dates = list(as.character(format(date, "%d/%m/%Y (%A)")))
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(code_muni) |>
    dplyr::mutate(dates = paste(unlist(dates), collapse = ", ")) |>
    dplyr::mutate(
      code_muni = as.character(code_muni),
      identifier = uuid::UUIDgenerate(),
      title = "Alerta de temperaturas acima de 35°C",
      message = glue::glue(
        "Estima-se que nos próximos sete dias o município apresente temperaturas iguais ou acima de 35°C nas seguinte(s) data(s): {dates}."
      )
    ) |>
    dplyr::select(identifier, code_muni, title, message)

  # Temp 5°C
  message_temp_5 <- dplyr::tbl(con, tb_temp) |>
    dplyr::group_by(code_muni) |>
    dplyr::arrange(date) |>
    dplyr::filter(value <= 5) |>
    dplyr::ungroup() |>
    dplyr::arrange(code_muni, date) |>
    dplyr::collect() |>
    dplyr::mutate(date = lubridate::date(date)) |>
    dplyr::select(code_muni, date) |>
    dplyr::distinct() |>
    dplyr::group_by(code_muni) |>
    dplyr::summarise(
      dates = list(as.character(format(date, "%d/%m/%Y (%A)")))
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(code_muni) |>
    dplyr::mutate(dates = paste(unlist(dates), collapse = ", ")) |>
    dplyr::mutate(
      code_muni = as.character(code_muni),
      identifier = uuid::UUIDgenerate(),
      title = "Alerta de temperaturas abaixo de 5°C",
      message = glue::glue(
        "Estima-se que nos próximos sete dias o município apresente temperaturas iguais ou abaixo de 5°C nas seguinte(s) data(s): {dates}."
      )
    ) |>
    dplyr::select(identifier, code_muni, title, message)

  DBI::dbDisconnect(con)

  message_df <- dplyr::bind_rows(
    message_pm25,
    message_o3,
    message_iqar,
    message_uv,
    message_temp_35,
    message_temp_5
  )

  return(message_df)
}
