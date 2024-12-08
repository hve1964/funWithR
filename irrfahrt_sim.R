#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Autor: Henk van Elst
# Datum: So, 22.01.2023 / So, 08.12.2024
# Thema: Realisationen von Irrfahrtprozessen
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
irrfahrt_sim <-
  function(
    anzahl = 10L,
    schritte = 100L,
    schrittlaenge = 1.0,
    y0 = 0.0,
    typ = "Gauss",
    sigma = 1.0,
    alpha_1 = 0.11,
    alpha_2 = 0.03,
    df = 3.0
  ) {

    # Zeitwerte:
    zeit <-
      seq(
        from = 0,
        by = schrittlaenge,
        length.out = (schritte + 1)
      )

    # Irrfahrtgenerator (Gauss/ Gosset):
    irrfahrt <-
      function(
        schritte = schritte,
        y0 = y0,
        sigma = sigma,
        typ = typ,
        df = df
      ) {

        y <-
          vector(
            mode = "numeric",
            length = (schritte + 1)
          )

        y[1] <- y0

        if (typ == "Gauss") {

          # Gaussian white noise
          w <-
            stats::rnorm(
              n = schritte,
              mean = 0,
              sd = sigma
            )

        } else if (typ == "Gosset") {

          # Gossetian white noise
          w <-
            stats::rt(
              n = schritte,
              df = df
            )

        } else {
          stop(
            paste0("Fehler! Bitte entweder den Typ 'Gauss' ",
                   "oder den Typ 'Gosset' waehlen.")
          )
        }

        for (i in 2:(schritte + 1)) {
          y[i] <-
            y[i - 1] + w[i - 1]
        }

        return(y)
      }

    # Realisationen von Irrfahrten generieren:
    wertematrix <-
      replicate(
        n = anzahl,
        expr = irrfahrt(
          schritte = schritte,
          y0 = y0,
          sigma = sigma,
          typ = typ,
          df = df
        )
      )

    # Zeitreihendaten-tibble:
    zeitreihendaten <-
      tibble::tibble(zeit = zeit) %>%
      dplyr::bind_cols(
        .,
        tibble::as_tibble(
          x = wertematrix,
          .name_repair = "unique"
        )
      )

    # Quantilslinien (Gauss/ Gosset) berechnen
    #   (koennen auch mit ggplot2::stat_function() eingefuehrt werden):
    if (typ == "Gauss") {

      # Quantilsline 1o:
      quant1o <-
        tibble::tibble(
          x = zeit,
          y = y0 + stats::qnorm(
            p = (1.0 - (alpha_1 / 2)),
            mean = 0,
            sd = (sigma / sqrt(x = schrittlaenge))
          ) * sqrt(x)
        )

      # Quantilsline 1u:
      quant1u <-
        tibble::tibble(
          x = zeit,
          y = y0 - stats::qnorm(
            p = (1.0 - (alpha_1 / 2)),
            mean = 0,
            sd = (sigma / sqrt(x = schrittlaenge))
          ) * sqrt(x)
        )

      # Quantilsline 2o:
      quant2o <-
        tibble::tibble(
          x = zeit,
          y = y0 + stats::qnorm(
            p = (1.0 - (alpha_2 / 2)),
            mean = 0,
            sd = (sigma / sqrt(x = schrittlaenge))
          ) * sqrt(x)
        )

      # Quantilsline 2u:
      quant2u <-
        tibble::tibble(
          x = zeit,
          y = y0 - stats::qnorm(
            p = (1.0 - (alpha_2 / 2)),
            mean = 0,
            sd = (sigma / sqrt(x = schrittlaenge))
          ) * sqrt(x)
        )

    } else if (typ == "Gosset") {

      # Quantilsline 1o:
      quant1o <-
        tibble::tibble(
          x = zeit,
          y = y0 + stats::qt(
            p = (1.0 - (alpha_1 / 2)),
            df = df
          ) * sqrt(x)
        )

      # Quantilsline 1u:
      quant1u <-
        tibble::tibble(
          x = zeit,
          y = y0 - stats::qt(
            p = (1.0 - (alpha_1 / 2)),
            df = df
          ) * sqrt(x)
        )

      # Quantilsline 2o:
      quant2o <-
        tibble::tibble(
          x = zeit,
          y = y0 + stats::qt(
            p = (1.0 - (alpha_2 / 2)),
            df = df
          ) * sqrt(x)
        )

      # Quantilsline 2u:
      quant2u <-
        tibble::tibble(
          x = zeit,
          y = y0 - stats::qt(
            p = (1.0 - (alpha_2 / 2)),
            df = df
          ) * sqrt(x)
        )

    } else {
      stop(
        paste0("Fehler! Bitte entweder den Typ 'Gauss' ",
               "oder den Typ 'Gosset' waehlen.")
      )
    }

    # Visualisierung:
    ausgabe <-
      zeitreihendaten %>%
      tidyr::pivot_longer(
        data = .,
        cols = !zeit
      ) %>%
      magrittr::set_colnames(
        x = .,
        value = c("Zeit", "Name", "Wert")
      ) %>%
      ggplot2::ggplot(
        data = .,
        mapping = ggplot2::aes(
          x = Zeit,
          y = Wert,
          colour = Name
        )
      ) +
      ggplot2::geom_line() +
      ggplot2::geom_line(
        data = quant1o,
        mapping = ggplot2::aes(
          x = x,
          y = y
        ),
        colour = "#00F5FF",
        linewidth = 1.0,
        linetype = "solid"
      ) +
      ggplot2::geom_line(
        data = quant1u,
        mapping = ggplot2::aes(
          x = x,
          y = y
        ),
        colour = "#00F5FF",
        linewidth = 1.0,
        linetype = "solid"
      ) +
      ggplot2::geom_line(
        data = quant2o,
        mapping = ggplot2::aes(
          x = x,
          y = y
        ),
        colour = "#F08080",
        linewidth = 1.0,
        linetype = "solid"
      ) +
      ggplot2::geom_line(
        data = quant2u,
        mapping = ggplot2::aes(
          x = x,
          y = y
        ),
        colour = "#F08080",
        linewidth = 1.0,
        linetype = "solid"
      ) +
      ggplot2::geom_hline(
        yintercept = y0,
        alpha = 0.4
      ) +
      ggplot2::labs(
        title = "Eindimensionaler Irrfahrtprozess",
        subtitle = latex2exp::TeX(
          input = paste0(
            "Typ: ",
            typ,
            "; Realisationen: ",
            anzahl,
            "; Zeitschritte: ",
            schritte,
            "; Zeitschrittlänge: ",
            schrittlaenge,
            "; $\\sigma = $",
            dplyr::if_else(
              condition = typ == "Gauss",
              true = sigma,
              false = 1.0
            ),
            "; Quantile: ",
            (1.0 - (alpha_1 / 2)) * 100,
            " % / ",
            (1.0 - (alpha_2 / 2)) * 100,
            " %"
          ),
          output = "expression"
        )
      ) +
      ggplot2::scale_x_continuous(name = "Zeit / 1") +
      ggplot2::scale_y_continuous(name = "Wert / 1") +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::guides(colour = "none") +
      ggplot2::theme_bw()

    return(ausgabe)
  }

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
