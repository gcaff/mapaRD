
# nivel provincial
# descargamos datos del COVID-19 a la fecha
d <- read.csv("https://gcaff.github.io/covid-rd/data/covid_data_rd.csv")

#eliminamos la fila de no especificados
d <- d%>%
  filter(provincia != "No especificado")

# seleccionar una fecha y renombrar la columna de codigos
dd <- d %>%
  filter(fecha == "16/5/20") %>%
  rename(ID=cod_prov)

# graficar
mapaRD(dd,"casos_acum",nivel="provincial", idName = "ID2")
ggmapaRD(dd,"casos_acum",nivel="provincial", idName = "ID2") +
  ggplot2::scale_fill_gradient(low = "white", high = "red") +
  ggplot2::geom_polygon(alpha=0, color="black") + # agregar bordes a las fronteras
  ggplot2::ggtitle("Casos Acumulados de COVID-19 al 16 de mayo del 2020")


# nivel regional

d <- data.frame(ID=1:10,x=runif(10))
mapaRD(d,"x",nivel="regional", idName = "ID2")
ggmapaRD(d,"x",nivel="regional", idName = "ID2") +
  ggplot2::ggtitle("Población por región, 2010")

p1 <- ggmapaRD(d,"x",nivel="regional", idName = "ID2") +
  ggplot2::ggtitle("Población por región, 2010")

p1 +
  ggplot2::scale_fill_gradient(low = "black", high = "red")

# nivel municipal

d <- data.frame(ID=1:155,x=runif(155))
mapaRD(d,"x",nivel="municipal", idName="ID2")
ggmapaRD(d,"x",nivel="municipal", idName="ID2")

# municipal para el gran santo domingo

IDs_SD <- tabla_toponimia("municipal") %>%
  filter(PROV %in% c("01","32")) %>% # DN y SD
  pull(ID2)

d <- data.frame(ID=IDs_SD,x=runif(length(IDs_SD)))
mapaRD(d,"x",nivel="municipal", idName="ID2", na.rm=T)
ggmapaRD(d,"x",nivel="municipal", idName = "ID2", na.rm=T)

# municipal para santiago

IDs_SANTIAGO <- tabla_toponimia("municipal") %>%
  filter(PROV == "25") %>% # Santiago
  pull(ID2)

d <- data.frame(ID=IDs_SANTIAGO,x=runif(length(IDs_SANTIAGO)))
mapaRD(d,"x",nivel="municipal", idName="ID2", na.rm=T)
ggmapaRD(d,"x",nivel="municipal", idName = "ID2", na.rm=T)

# mapa RD interactivo

mapaRD:::mapaRD_interactivo(dd,"casos_acum",nivel="provincial", idName = "ID2")

