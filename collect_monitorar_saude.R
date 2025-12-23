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
        "Estima-se que nos próximos sete dias o município apresente concentrações de PM 2.5 acima dos valores recomendados pela Organização Mundial da Saúde (OMS) nas seguinte(s) data(s): {dates}.
        <br>
        <br>
        Recomendações para os serviços de saúde:
        <ul>
        <li>Intensificar a vigilância de atendimentos por asma, DPOC, infecções respiratórias e eventos cardiovasculares.</li>
        <li>Reforçar orientações às equipes da Atenção Primária e serviços de urgência.</li>
        <li>Avaliar a necessidade de ampliação da capacidade assistencial em períodos prolongados de exposição.</li>
        <li>Articular ações intersetoriais com meio ambiente e defesa civil, quando aplicável.</li>
        </ul>
        <br>
        Recomendações para a população:
        <ul>
        <li>Evitar atividades físicas ao ar livre, especialmente crianças, idosos, gestantes e pessoas com doenças respiratórias ou cardíacas.</li>
        <li>Manter ambientes internos ventilados, evitando exposição direta à fumaça e poeira.</li>
        <li>Seguir orientações médicas e procurar atendimento em caso de falta de ar, tosse persistente ou piora de sintomas respiratórios.</li>
        </ul>
        "
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
        "Estima-se que nos próximos sete dias o município apresente concentrações de O3 (ozônio) acima dos valores recomendados pela Organização Mundial da Saúde (OMS) nas seguinte(s) data(s): {dates}.
        <br>
        <br>
        Recomendações para os serviços de saúde:
        <ul>
        <li>Monitorar aumento de atendimentos por irritação ocular, tosse, chiado no peito e crises asmáticas.</li>
        <li>Orientar profissionais de saúde quanto aos efeitos do ozônio, sobretudo em dias quentes e ensolarados.</li>
        <li>Reforçar ações educativas junto à população vulnerável.</li>
        </ul>
        <br>
        Recomendações para a população:
        <ul>
        <li>Evitar exposição prolongada ao ar livre nos horários de maior concentração.</li>
        <li>Reduzir esforços físicos intensos ao ar livre.</li>
        <li>Procurar atendimento de saúde em caso de desconforto respiratório ou agravamento de doenças preexistentes.</li>
        </ul>
        "
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
        "Estima-se que nos próximos sete dias o município apresente o Índice de Qualidade do Ar (IQAr) com valores acima dos recomendados pelo CONAMA nas seguinte(s) data(s): {dates}.
        <br>
        <br>
        Recomendações para os serviços de saúde:
        <ul>
        <li>Ativar planos locais de resposta a eventos ambientais adversos.</li>
        <li>Reforçar comunicação com unidades básicas, UPAs e hospitais.</li>
        <li>Intensificar a vigilância de agravos respiratórios e cardiovasculares.</li>
        </ul>
        <br>
        Recomendações para a população:
        <ul>
        <li>Reduzir o tempo de permanência ao ar livre.</li>
        <li>Evitar exercícios físicos intensos em ambientes externos.</li>
        <li>Manter hidratação adequada e atenção a sintomas respiratórios.</li>
        </ul>
        "
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
        "Estima-se que nos próximos sete dias o município apresente o Índice de Raios Ultravioletas (IUV) com valores acima dos recomendados nas seguinte(s) data(s): {dates}.
        <br>
        <br>
        Recomendações para os serviços de saúde:
        <ul>
        <li>Reforçar orientações preventivas nas unidades de saúde e ações de promoção da saúde.</li>
        <li>Monitorar atendimentos por queimaduras solares e complicações relacionadas ao calor.</li>
        </ul>
        <br>
        Recomendações para a população:
        <ul>
        <li>Evitar exposição direta ao sol entre 10h e 16h.</li>
        <li>Utilizar protetor solar, chapéu, roupas de manga longa e óculos escuros.</li>
        <li>Redobrar cuidados com crianças, idosos e trabalhadores ao ar livre.</li>
        </ul>
        "
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
        "Estima-se que nos próximos sete dias o município apresente temperaturas iguais ou acima de 35°C nas seguinte(s) data(s): {dates}.
        <br>
        <br>
        Recomendações para os serviços de saúde:
        <ul>
        <li>Ativar protocolos de resposta a eventos de temperatura elevada.</li>
        <li>Monitorar casos de insolação, desidratação e agravamento de doenças cardiovasculares e renais.</li>
        <li>Garantir fluxo assistencial para populações vulneráveis (idosos, crianças, pessoas em situação de rua).</li>
        </ul>
        <br>
        Recomendações para a população:
        <ul>
        <li>Beber água com frequência, mesmo sem sede.</li>
        <li>Evitar atividades físicas intensas nos horários mais quentes do dia.</li>
        <li>Permanecer em locais ventilados ou climatizados sempre que possível.</li>
        <li>Procurar atendimento em caso de tontura, fraqueza, confusão mental ou febre.</li>
        </ul>
        "
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
        "Estima-se que nos próximos sete dias o município apresente temperaturas iguais ou abaixo de 5°C nas seguinte(s) data(s): {dates}.
        <br>
        <br>
        Recomendações para os serviços de saúde:
        <ul>
        <li>Reforçar a vigilância de síndromes respiratórias agudas.</li>
        <li>Articular ações com assistência social para proteção de populações vulneráveis.</li>
        <li>Orientar equipes sobre sinais de hipotermia e complicações associadas ao frio.</li>
        </ul>
        <br>
        Recomendações para a população:
        <ul>
        <li>Manter-se aquecido, utilizando roupas adequadas e protegendo extremidades.</li>
        <li>Evitar exposição prolongada ao frio, especialmente à noite e madrugada.</li>
        <li>Procurar atendimento em caso de dificuldade respiratória, sonolência excessiva ou tremores intensos.</li>
        </ul>
        "
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
