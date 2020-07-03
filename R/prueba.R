
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

d <- data.frame(ID=1:15,x=runif(15))
mapaRD(d,"x",nivel="municipal", idName="ID2")
ggmapaRD(d,"x",nivel="municipal", idName = "ID2")


