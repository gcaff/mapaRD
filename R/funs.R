#' Graficar mapa de la Rep. Dom.
#'
#' Grafica el mapa de la Rep. Dom. por nivel administrativo (regional,
#' provincial y municipal).
#'
#' @param nivel nivel territorial/administrativo (\code{"regional"}, \code{"provincial"}, o \code{"municipal"})
#' @param df dataframe con los valores
#' @param var nombre de variable
#' @param na.rm logical que cuando es verdadero, excluye del gráfico los territorios
#' con valores NA
#' @param idName nombre (caracter) de la variable ID con la que se empareja la data.
#' solo admite dos valores: "ID" o "ID2".
#'
#' @return Mapa formato objeto ggplot
#'
#' @examples
#' d <- data.frame(ID = 1:32, x = rnorm(32))
#' ggmapaRD("provincial", df = d, var= "x", idName = "ID2")
#'
#' @export
ggmapaRD <- function(df, var, nivel="provincial", na.rm = FALSE, idName){

  varname <- sym(var)

  # error handling
  if (!is.data.frame(df)){
    stop("df debe ser un data.frame")
  }
  if (!(var %in% colnames(df))){
    stop("df debe contener una columna llamada ID con los identificadores de cada territorio")
  }
  if (!(idName %in% c("ID","ID2"))){
    stop("El argumento 'idName' debe ser 'ID' o 'ID2'")
  }

  # buscar coordenadas del mapa
  rd_spdf <- mapaRD:::buscarMapaRD(nivel=nivel, maptype = "ggplot")

  # unir las coordenadas con la data en df
  if (idName == "ID2"){
    rd_spdf <- rd_spdf %>%
      dplyr::left_join(df, by=c("ID2"="ID"))
  } else {
    rd_spdf <- rd_spdf %>%
      dplyr::left_join(df, by="ID")
  }

  if (na.rm){
    rd_spdf <- rd_spdf %>%
      dplyr::filter(!is.na(!!varname))
  }
  # graficar el mapa
  rd_spdf %>%
    ggplot2::ggplot(ggplot2::aes(x=long, y=lat,group=group)) +
    ggplot2::geom_polygon(ggplot2::aes(fill= !!varname)) +
    ggplot2::coord_map() +
    ggplot2::theme_void()

}

#' Graficar mapa de la Rep. Dom.
#'
#' Grafica el mapa de la Rep. Dom. por nivel administrativo (regional,
#' provincial y municipal).
#'
#' @param nivel nivel territorial/administrativo (\code{"regional"}, \code{"provincial"}, o \code{"municipal"})
#' @param df dataframe con los valores
#' @param var nombre de variable
#' @param na.rm logical que cuando es verdadero, excluye del gráfico los territorios
#' con valores NA
#' @param idName nombre (caracter) de la variable ID con la que se empareja la data.
#' solo admite dos valores: "ID" o "ID2".
#'
#' @return Mapa
#'
#' @examples
#' d <- data.frame(ID = 1:155, x = rnorm(155))
#' mapaRD("municipal", df = d, var= "x", idName = "ID2")
#'
#' @export
mapaRD <- function(df, var, nivel="provincial", na.rm = FALSE, idName){

  varname <- sym(var)

  # error handling
  if (!is.data.frame(df)){
    stop("df debe ser un data.frame")
  }
  if (!(var %in% colnames(df))){
    stop("df debe contener una columna llamada ID con los identificadores de cada territorio")
  }
  if (!(idName %in% c("ID","ID2"))){
    stop("El argumento 'idName' debe ser 'ID' o 'ID2'")
  }

  # buscar coordenadas del mapa
  rd_spdf <- mapaRD:::buscarMapaRD(nivel=nivel, maptype = "base")

  # unir las coordenadas con la data de df
  if (idName=="ID2"){
    rd_spdf@data <- rd_spdf@data %>%
      dplyr::left_join(df, by=c("ID2"="ID"))
  } else {
    rd_spdf@data <- rd_spdf@data %>%
      dplyr::left_join(df, by="ID")
  }

  # si na.rm=TRUE, excluye del grafico los territorios con NAs
  if (na.rm){
    rd_spdf <- rd_spdf %>%
      subset(!is.na(eval(varname)))
  }

  # graficar el mapa
  cartography::choroLayer(spdf = rd_spdf, var = var)

}

#' Graficar mapa interactivo de la Rep. Dom.
#'
#' Grafica un mapa interactivo de la Rep. Dom. por nivel administrativo (regional,
#' provincial y municipal).
#'
#' @param nivel nivel territorial/administrativo (\code{"regional"}, \code{"provincial"}, o \code{"municipal"})
#' @param df dataframe con los valores
#' @param var nombre de variable
#' @param na.rm logical que cuando es verdadero, excluye del gráfico los territorios
#' con valores NA
#'
#' @return Mapa formato objeto plotly
#'
#' @examples
#' d <- data.frame(ID = 1:32, x = rnorm(32))
#' mapaRD_interactivo("provincial", df = d, var= "x", idName = "ID2")
#'
#'
mapaRD_interactivo <- function(df, var, nivel="provincial", na.rm = FALSE, idName){

  # generar mapa formato ggplot
  p_rd <- mapaRD:::ggmapaRD(df = df,
                            var = var,
                            nivel = nivel,
                            #na.rm = na.rm,
                            idName = idName)

  # animar usando plotly
  plotly::ggplotly(p_rd)

}


#' Tabla de toponimia
#'
#' Muestra la tabla de nombres, identificadores, y demás detalles de
#' las divisiones territoriales para cada nivel administrativo.
#'
#' @param nivel nivel territorial/administrativo (\code{"regional"}, \code{"provincial"}, o \code{"municipal"})
#'
#' @return data.frame
#'
#' @examples
#' tabla_toponimia(nivel="provincial")
#'
#' @export
tabla_toponimia <- function(nivel="provincial"){

  # importar tabla toponimia
  load("data/tabla_toponimia.RData")

  # seleccionar tabla de acorde al nivel territorial
  tabla_topon[[nivel]]

}

#' Buscar coordenadas de mapas por division territorial
#'
#' Función interna al paquete que busca en la data las coordenadas de los mapas
#' por territorio
#'
#' @param nivel nivel territorial/administrativo (\code{"regional"}, \code{"provincial"}, o \code{"municipal"})
#' @param sub subconjunto de territorios
#' @param maptype busca el archivo dependiento el tipo de grafico
#'
#' @return None
#'
#' @examples
#' buscarMapaRD(nivel="provincial")
#'
buscarMapaRD <- function(nivel,maptype="ggplot"){

  if (maptype == "ggplot"){
      # cargar data de los mapas
      load("~/wd/R/paquetes/mapaRD/data/mapasRD_spdf_fort.RData")

      # filtrar por nivel territorial
      if (nivel == "regional"){
        spdf <- reg_spdf_fort
      } else if (nivel == "provincial"){
        spdf <- prov_spdf_fort
      } else if (nivel == "municipal"){
        spdf <- mun_spdf_fort
      } else {
        stop("Debe suministrar un nivel territorial: regional, provincial o municipal")
      }
      # agregar aqui codigo para filtrar por provincia/municipio


    } else {
      # cargar data de los mapas
      load("~/wd/R/paquetes/mapaRD/data/mapasRD_spdf.RData")

      # filtrar por nivel territorial
      if (nivel == "regional"){
        spdf <- reg_spdf
      } else if (nivel == "provincial"){
        spdf <- prov_spdf
      } else if (nivel == "municipal"){
        spdf <- mun_spdf
      } else {
        stop("Debe suministrar un nivel territorial: regional, provincial o municipal")
      }
      # agregar aqui codigo para filtrar por provincia/municipio


    }

  spdf
}
