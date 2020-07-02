
# nivel provincial
d <- read.csv("https://gcaff.github.io/covid-rd/data/covid_data_rd.csv")

d <- d%>%
  filter(provincia != "No especificado")

d$prov_id <- rep(1:32,102)

dd <- d %>%
  filter(fecha == "16/4/20") %>%
  mutate(ID = prov_id)

mapaRD(nivel="provincial",dd,"casos_acum", idName = "ID2")
ggmapaRD(nivel="provincial",dd,"casos_acum", idName = "ID2")

# nivel regional

d <- data.frame(ID=1:10,x=runif(10))
mapaRD(nivel="regional",d,"x", idName = "ID2")
ggmapaRD(nivel="regional",d,"x", idName = "ID2")


# nivel municipal

d <- data.frame(ID=1:155,x=runif(155))
mapaRD(nivel="municipal",d,"x", idName="ID2")
ggmapaRD(nivel="municipal",d,"x", idName="ID2")


