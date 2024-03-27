berechne_fibonacci_zahlen <-
  function(anzahl = 3L) {

    # Pruefung Anzahl zu generierender Zahlen:
    if (anzahl < 3) {
      stop(paste0(
        "Fehler!\n",
        "Gib einen ganzzahligen Wert fuer 'anzahl' groesser oder ",
                  "gleich 3 ein."))
    }

    # Initialisieren eines Vektors (integer):
    zahlen <-
      vector(
        mode = "integer",
        length = anzahl
      )

    zahlen[1:2] <-
      c(1L, 1L)

    # Generieren der Zahlenreihe:
    for (i in 3:anzahl) {
      zahlen[i] <-
        (zahlen[i - 2] + zahlen[i - 1])
    }

    # Ausgabe in tibble-Form, mit Nachbarverhaeltnissen:
    ausgabe <-
      tibble::tibble(
        zahlen = zahlen
      ) %>%
      dplyr::mutate(
        .data = .,
        verhaeltnis_nachbarn = (zahlen / dplyr::lag(x = zahlen, n = 1L))
      )

    return(ausgabe)
  }
