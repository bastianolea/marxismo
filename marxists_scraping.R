library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)

url <- list()
sesion <- list()
sitio <- list()
enlaces_autor <- list()

url$marx_engels <- "https://www.marxists.org/espanol/m-e/indice.htm"
url$lenin <- "https://www.marxists.org/espanol/lenin/obras/escritos.htm"
url$trotsky <- "https://www.marxists.org/espanol/trotsky/indice.htm"
url$gramsci <- "https://www.marxists.org/espanol/gramsci/index.htm"
url$mao <- "https://www.marxists.org/espanol/mao/indice.htm" #no está citas, https://www.marxists.org/espanol/mao/escritos/libros/librorojo/citas-1.htm
url$luxemburgo <- "https://www.marxists.org/espanol/luxem/index.htm"
url$kollontai <- "https://www.marxists.org/espanol/kollontai/index.htm"


autores <- c("marx_engels", "lenin", "trotsky", "gramsci", "mao", "luxemburgo", "kollontai")

.autor <- "mao"

datos <- list()

#abrir sesión
sesion[[.autor]] <- session(url[[.autor]])

sitio[[.autor]] <- sesion[[.autor]] |> 
  read_html()

#obtener enlaces de autor
enlaces_autor[[.autor]] <- sitio[[.autor]] |> 
  html_elements("a") |> 
  html_attr("href")

#acceder a documento
datos[[.autor]] <- enlaces_autor[[.autor]][11:20] |> 
  set_names() |> 
  map(\(.enlace) {
    message(.enlace)
    
    # .enlace <- enlaces_autor[[.autor]][256] #citas
    # .enlace <- enlaces_autor[[.autor]][13] #citas
    
    inicio <- Sys.time()
    
    documento <- sesion[[.autor]] |> 
      # session_jump_to(enlaces_autor[[.autor]][5]) |> 
      session_jump_to(.enlace) |> 
      read_html() |> 
      try()
    
    if (class(documento)[1] == "try-error") {
      message("error")
      return(NULL)
    }
    
    #determinar si viene el texto en el enlace o no
    parrafos <- documento |> html_elements("p") |> html_text()
    
    parrafos_largos <- tibble(parrafos) |> 
      mutate(nchar = nchar(parrafos)) |> 
      filter(nchar > 140)
    
    doc_n_parrafos <- documento |> html_elements("p") |> length() #cantidad de párrafos (pero hay textos con pocos parrafos pero muy largos)
    doc_nchar_parrafos <- documento |> html_elements("p") |> html_text() |> str_length() #largo de los párrafos
    doc_anclas <- documento |> html_elements("a") |> html_attr("href") |> str_subset("#") |> length() #si hay varios enlaces con "#" (anclas), entonces sí es
    
    #si el texto tiene parrafos largos, scrapear
    if (nrow(parrafos_largos) > 5) {
      scraping <- list()
      
      #título (h1 o h2)
      titulo <- documento |> html_elements("h1") |> html_text()
      scraping$titulo <- ifelse(length(titulo) == 0, 
                                documento |> html_elements("h2") |> html_text() |> paste(collapse = " "), 
                                paste(titulo, collapse = " ")
      )
      
      #texto
      scraping$texto <- parrafos |> str_subset(".{40,}") #mayores a x caracteres
      scraping$enlace <- .enlace
      
      Sys.sleep(Sys.time()-inicio)
      
      return(scraping)
      
    } else {
      #si no tiene parrafos largos, revisar enlaces
      warning(paste(.enlace, "no tiene párrafos largos"))
      # enlaces <- documento |> 
      #   html_elements("a")
      # 
      # enlaces |> html_text() |> str_subset("HTML|html")
    }
  })

enlaces_autor$mao
datos$mao[[3]]
