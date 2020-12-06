setwd("C:/Users/njere/OneDrive/proyecto de titulo")
source("./R/packages.R")

datos=openxlsx::read.xlsx("./DATABASES/cv19_regional.xlsx",detectDates=TRUE) %>%
  filter(Fecha<"2020-12-01")
regiones.ordenadas=c("Arica y Parinacota","Tarapaca","Antofagasta",
                     "Atacama","Coquimbo","Valparaiso",
                     "Metropolitana","OHiggins","Maule",
                     "Nuble","Biobio","Araucania","Los Rios",
                     "Los Lagos","Aysen","Magallanes","Total")
datos$Region=factor(datos$Region,levels=regiones.ordenadas)

#nuevos
pdf(file="./IMAGES/graph_chile_situacion_actual_detalle.pdf",width=9,height=5)
  datos %>% filter(Region =="Total") %>%
  select(Fecha,Nuevos.Sintomaticos,Nuevos.Asintomaticos,
         Nuevos.No.Notificados) %>%
  melt(id.vars="Fecha",variable.name="Tipo",value.name="Conteo") %>%
  rename(Mes=Fecha) %>% ggplot() +
  geom_area(aes(Mes,Conteo,fill=Tipo),alpha=I(0.4),color="black",size=0.2) +
  labs(y="Casos confirmados diarios") + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme_classic() + theme(legend.position="bottom") +
  scale_fill_brewer(" ",palette="Blues",direction = -1,
    labels=c("Sintomáticos","Asintomáticos","No Notificados"))
dev.off()
