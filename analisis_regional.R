setwd("C:/Users/njere/OneDrive/proyecto de titulo")
source("./R/packages.R")

datos=openxlsx::read.xlsx("./DATABASES/cv19_regional.xlsx",detectDates=TRUE) %>%
  filter(Fecha<"2020-12-01")
regiones.ordenadas=c("Arica y Parinacota","Tarapaca","Antofagasta",
                     "Atacama","Coquimbo","Valparaiso",
                     "Metropolitana","OHiggins","Maule",
                     "Nuble","Biobio","Araucania",
                     "Los Rios","Los Lagos","Aysen","Magallanes","Total")
datos$Region=factor(datos$Region,levels=regiones.ordenadas)

#puntos de cambio
library(strucchange)

cortes=
  lapply(regiones.ordenadas[-17],
       function(x){breakpoints(Nuevos.Confirmados ~ 1,data=datos %>% filter(Region==x))$breakpoints})
names(cortes)=regiones.ordenadas[-17]

cortes.region=plyr::ldply(cortes, data.frame) %>% rename(Region=.id,Punto.Corte=X..i..) %>% 
  mutate(Region=factor(Region,levels=regiones.ordenadas[-17]))

max.puntos=(table(cortes.region$Region) %>% sort(TRUE))[1]

faltan=dcast(cortes.region,Region~.) %>% rename(Puntos='.') %>% 
  mutate(Puntos=max.puntos-Puntos)

cortes.region=
  rbind(cortes.region,data.frame(Region=rep(faltan$Region,faltan$Puntos),Punto.Corte=0)) %>%
  arrange(Region) %>% mutate(N=rep(paste("Punto",1:max.puntos),16)) %>% 
  mutate(Punto.Corte=ifelse(Punto.Corte==0,NA,as.character(as.Date("2020-03-03")+Punto.Corte-1)))

reshape(cortes.region,idvar="Region", timevar="N",direction = "wide") %>%
  data.frame(row.names = paste0(.$Region)) %>% select(-Region) %>% 
  `colnames<-`(paste("Puntos",1:max.puntos,sep=".")) %>%
  xtable(caption="Fechas en las que se ha registrado un cambio estructural a la serie de casos confirmados diarios por COVID-19",
         label="tab:punto-cambio-regiones",digits=0)

#######
#creacion de paneles
#######

paneles=
  merge(
  data.frame(Region=regiones.ordenadas[-17],Fecha.Min="2020-03-03"),
  cortes.region %>% filter (Punto.Corte!="<NA>") %>% select(-N) %>% 
    group_by(Region) %>% filter(row_number()==1) %>% rename(Fecha.Max=Punto.Corte),"Region")

for(i in 1:(max.puntos-1)){
  paneles=
    paneles %>% 
    rbind(
      merge(cortes.region %>% filter (Punto.Corte!="<NA>") %>% select(-N) %>% 
        group_by(Region) %>% filter(row_number()==i) %>% rename(Fecha.Min=Punto.Corte),
      cortes.region %>% filter (Punto.Corte!="<NA>") %>% select(-N) %>% 
        group_by(Region) %>% filter(row_number()==i+1) %>% rename(Fecha.Max=Punto.Corte),"Region"))
}

paneles=
  paneles %>% 
    rbind(merge(
      paneles %>% group_by(Region) %>% filter(Fecha.Max==max(Fecha.Max)) %>%
        select(-Fecha.Min) %>% rename(Fecha.Min=Fecha.Max) %>% as.data.frame(),
      data.frame(Region=regiones.ordenadas[-17],Fecha.Max=as.character(max(datos$Fecha))),"Region")) %>%
  mutate(Fecha.Min=as.Date(Fecha.Min),Fecha.Max=as.Date(Fecha.Max),
         Region=factor(Region,levels=regiones.ordenadas)) %>% arrange(Region) %>% 
  mutate(Color="red")

paneles[seq(2,nrow(paneles),2),"Color"]="blue"
write.xlsx(paneles,"./DATABASES/segmentos.xlsx")
###############

#detalle regiones
pdf(file="./IMAGES/graph_chile_confirmados_region_1.pdf", width=15,height=15)
datos %>% filter(Region !="Total" & Pos<=8) %>%
  select(Fecha,Region,Nuevos.Sintomaticos,Nuevos.Asintomaticos,
         Nuevos.No.Notificados,Nuevos.Confirmados) %>%
  ggplot() + 
  facet_wrap(Region~.,scales = "free",ncol=2) +
  geom_rect(aes(xmin=Fecha.Min,xmax=Fecha.Max,ymin=-Inf,ymax=Inf,fill=Color),
            data=paneles %>% filter(Region %in% regiones.ordenadas[1:8]),
            alpha=I(0.1)) +
  geom_line(aes(Fecha,Nuevos.Confirmados),alpha=I(0.7)) +
  stat_rollapplyr(aes(Fecha,Nuevos.Confirmados),width = 12,
                  align = "center",colour = "steelblue",size=1) +
  labs(y="",x="Mes",title="Número de casos confirmados por COVID-19 \n (Parte I)") + 
  theme_few() + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme(axis.text = element_text( size = 10 ),
        strip.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size = 17),
        legend.position = "none") 
dev.off()

pdf(file="./IMAGES/graph_chile_confirmados_region_2.pdf", width=15,height=15)
datos %>% filter(Region !="Total" & Pos>8) %>%
  select(Fecha,Region,Nuevos.Sintomaticos,Nuevos.Asintomaticos,
         Nuevos.No.Notificados,Nuevos.Confirmados) %>%
  ggplot() + 
  facet_wrap(Region~.,scales = "free",ncol=2) +
  geom_rect(aes(xmin=Fecha.Min,xmax=Fecha.Max,ymin=-Inf,ymax=Inf,fill=Color),
            data=paneles %>% filter(Region %in% regiones.ordenadas[9:16]),
            alpha=I(0.1)) +
  geom_line(aes(Fecha,Nuevos.Confirmados),alpha=I(0.7)) +
  stat_rollapplyr(aes(Fecha,Nuevos.Confirmados),width = 12,
                  align = "center",colour = "steelblue",size=1) +
  labs(y="",x="Mes",title="Número de casos confirmados por COVID-19 \n (Parte II)") + 
  theme_few() + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme(axis.text = element_text( size = 10 ),
        strip.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size = 17),
        legend.position = "none") 
dev.off()

#fallecidos
pdf(file="./IMAGES/graph_chile_fallecidos_region_1.pdf", width=15,height=15)
datos %>% filter(Region !="Total" & Pos<=8) %>%
  ggplot() + 
  facet_wrap(Region~.,scales = "free",ncol=2) +
  geom_area(aes(Fecha,Casos.Fallecidos),fill="red",alpha=I(0.3)) +
  geom_line(aes(Fecha,Nuevos.Confirmados),alpha=I(0.5)) +
  labs(y="",x="Mes",title="Número de casos fallecidos por COVID-19 acumulados \n (Parte I)") + 
  theme_few() + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme(axis.text = element_text( size = 10 ),
        strip.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size = 17)) 
dev.off()

pdf(file="./IMAGES/graph_chile_fallecidos_region_2.pdf", width=15,height=15)
datos %>% filter(Region !="Total" & Pos>8) %>%
  ggplot() + 
  facet_wrap(Region~.,scales = "free",ncol=2) +
  geom_area(aes(Fecha,Casos.Fallecidos),fill="red",alpha=I(0.3)) +
  geom_line(aes(Fecha,Nuevos.Confirmados),alpha=I(0.5)) +
  labs(y="",x="Mes",title="Número de casos fallecidos por COVID-19 acumulados \n (Parte II)") + 
  theme_few() + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme(axis.text = element_text( size = 10 ),
        strip.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size = 17)) 
dev.off()

#pacientes uci
pdf(file="./IMAGES/graph_chile_uci_region_1.pdf", width=15,height=15)
datos %>% filter(Region !="Total" & Pos<=8) %>%
  ggplot() + 
  facet_wrap(Region~.,scales = "free",ncol=2) +
  geom_area(aes(Fecha,Pacientes.UCI),fill="blue",alpha=I(0.3)) +
  geom_line(aes(Fecha,Nuevos.Confirmados),alpha=I(0.5)) +
  labs(y="",x="Mes",title="Número de pacientes internados en UCI por COVID-19 \n (Parte I)") + 
  theme_few() + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme(axis.text = element_text( size = 10 ),
        strip.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size = 17)) 
dev.off()

pdf(file="./IMAGES/graph_chile_uci_region_2.pdf", width=15,height=15)
datos %>% filter(Region !="Total" & Pos>8) %>%
  ggplot() + 
  facet_wrap(Region~.,scales = "free",ncol=2) +
  geom_area(aes(Fecha,Pacientes.UCI),fill="blue",alpha=I(0.3)) +
  geom_line(aes(Fecha,Nuevos.Confirmados),alpha=I(0.5)) +
  labs(y="",x="Mes",title="Número de pacientes internados en UCI por COVID-19 \n (Parte II)") + 
  theme_few() + scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  theme(axis.text = element_text( size = 10 ),
        strip.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size = 17)) 
dev.off()

#tasa incidencia
mapa.regiones=generar_regiones()
mapa.regiones=mapa.regiones %>%
  mutate(codigo_region=c(2,3,4,5,6,8,9,11,12,14,15,16,7,13,1,10),
         cen.x=gCentroid(as(mapa.regiones$geometry,Class="Spatial"),byid = TRUE)$x,
         cen.y=gCentroid(as(mapa.regiones$geometry,Class="Spatial"),byid = TRUE)$y)

desviaciones=c(1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1)

pdf(file="./IMAGES/graph_chile_incidencia_acumulada.pdf", width=7,height=9)
merge(mapa.regiones, 
      select(datos,Region,Pos,Fecha,Poblacion,Casos.Confirmados),
      by.x="codigo_region", by.y="Pos") %>%
  mutate(Incidencia.Acumulada=round(Casos.Confirmados/Poblacion*10000,2),
         Etiqueta=paste(Region,"\n",round(Incidencia.Acumulada,3))) %>%
  arrange(Fecha) %>% filter(Fecha==last(Fecha)) %>%
  ggplot() + xlim(c(-85,-57)) +
  geom_sf(aes(fill=cut(Incidencia.Acumulada,breaks=5))) + theme_void() +
  scale_fill_manual("",values=c("chartreuse4","yellow","orange","firebrick1")) +
  geom_label_repel(aes(x=cen.x,y=cen.y,label=paste(Etiqueta)),size = 5,
                   min.segment.length = 0,nudge_x=8*desviaciones) +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5,size = 17))
#ggtitle("Tasa de Incidencia Acumulada") +
dev.off()

