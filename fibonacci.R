fibonacci <-
  function(n) {
    if (n < 3) {
      stop(paste0("Fehler!\n",
                  "Gib einen ganzzahligen Wert fuer n groesser oder ",
                  "gleich 3 ein."))
    }
    
    folge <-
      integer(length = n)
    
    folge[1:2] <-
      c(1L, 1L)
    
    for (i in 3:n) {
      folge[i] <-
        folge[i - 1] + folge[i - 2]
    }
    
    return(folge)
  }
