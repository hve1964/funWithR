berechne_riemann_integral <-
  function(
    funktion = \(x) x,
    untere_grenze = 0.0,
    obere_grenze = 1.0,
    anzahl_intervalle = 1e2L
  ) {
    # hve: So, 08.12.2024
    intervall_breite <-
      ((obere_grenze - untere_grenze) / anzahl_intervalle)

    obere_werte <-
      c(1:anzahl_intervalle) %>%
      purrr::map_dbl(
        .x = .,
        .f = \(x) (untere_grenze + x * intervall_breite),
        .progress = TRUE
      )

    untere_werte <-
      (obere_werte - intervall_breite)

    xsi_werte <-
      untere_werte %>%
      purrr::map2_dbl(
        .x = .,
        .y = obere_werte,
        .f = \(x, y) 0.5 * (x + y),
        .progress = TRUE
      )

    f_von_xsi <-
      xsi_werte %>%
      purrr::map_dbl(
        .x = .,
        .f = funktion,
        .progress = TRUE
      )

    integrand_summanden <-
      (f_von_xsi * intervall_breite)

    integral_wert <-
      integrand_summanden %>%
      sum(x = .)

    return(integral_wert)
  }
