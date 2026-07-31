nested_df <- data_set_xco2 |>
  dplyr::mutate(
    date = lubridate::as_date(time)
  ) |>
  dplyr::filter(xco2_quality_flag == 0) |>
  dplyr::group_by(date) |>
  tidyr::nest() |>
  dplyr::mutate(
    nobs=purrr::map(data,nrow)
  )


anomalia_calc <- function(df){
  xco2 <- df$xco2

  anomalia = xco2 - median(xco2)
  return(anomalia)
}

nested_df |>
  dplyr::filter(
    nobs >=5
  ) |>
  #dplyr::ungroup() |>
  dplyr::mutate(
    xco2_anomalia = purrr::map_df(data, function(df)(df$xco2 - median(df$xco2,na.rm = T)))
  )
purrr::map_df


