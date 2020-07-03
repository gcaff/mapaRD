
# nivel provincial
d <- read.csv("https://gcaff.github.io/covid-rd/data/covid_data_rd.csv")

d <- d%>%
  filter(provincia != "No especificado")

d$prov_id <- rep(1:32,102)

dd <- d %>%
  filter(fecha == "16/4/20") %>%
  mutate(ID = prov_id)

mapaRD(dd,"casos_acum",nivel="provincial", idName = "ID2")
ggmapaRD(dd,"casos_acum",nivel="provincial", idName = "ID2")

# nivel regional

d <- data.frame(ID=1:10,x=runif(10))
mapaRD(d,"x",nivel="regional", idName = "ID2")
ggmapaRD(d,"x",nivel="regional", idName = "ID2")


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

