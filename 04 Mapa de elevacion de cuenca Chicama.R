#------------------------------------------------------------------------
require(pacman)
pacman::p_load(RColorBrewer, ggspatial, raster,colorspace, ggpubr, sf,openxlsx)
#------------------------------------------------------------------------
Peru               <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Cuencas_peru       <- st_read ("SHP/Cuencas_peru.shp")  
Rio_libe           <- st_read ("SHP/RIOS_LA_LIBERTAD_geogpsperu_SuyoPomalia_931381206.shp")  
Rio_caja           <- st_read ("SHP/RIOS_CAJAMARCA_geogpsperu_SuyoPomalia_931381206.shp")  
Cuencas_peru       <- st_transform(Cuencas_peru ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio_caja           <- st_transform(Rio_caja ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio_libe           <- st_transform(Rio_libe  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Cuenca_Chicama     <- subset(Cuencas_peru , NOMB_UH_N5  == "Chicama")
Cuencas_rios1      <- st_intersection(Rio_caja, Cuenca_Chicama)
Cuencas_rios       <- st_intersection(Rio_libe, Cuenca_Chicama)
Cuencas_peru_c     <- cbind(Cuencas_peru, st_coordinates(st_centroid(Cuencas_peru$geometry)))

dem = raster("raster/ASTGTM_S08W080_dem.tif")
dem2 = raster("raster/ASTGTM_S08W079_dem.tif")
DEM_total<- raster::merge(dem, dem2,dem)

Cuenca_Chicama_alt     <- crop(DEM_total, Cuenca_Chicama)
Cuenca_Chicama_alt     <- Cuenca_Chicama_alt   <- mask(Cuenca_Chicama_alt  , Cuenca_Chicama)
plot(Cuenca_Chicama_alt )

dem.p          <-  rasterToPoints(Cuenca_Chicama_alt )
df             <-  data.frame(dem.p)
colnames(df) = c("lon", "lat", "alt")

aps            = terrain(Cuenca_Chicama_alt , opt = "aspect", unit= "degrees")
dem.pa          <-  rasterToPoints(aps  )
df_a            <-  data.frame(dem.pa)
#------------------------------------------------------------------------
Data     <- read.xlsx("Excel/Embrete.xlsx", sheet="Hoja2") 
Data[1,1] <- "MAPA DE ELEVACION DE \nLA CUENCA, \nChicama"
Data[2,1] <- "Elaboradpor: \nGorky Florez Castillo"
Data[3,1] <- "Escala: Indicadas"
Data[4,1] <- "Sistemas de Coordenadas UTM \nZona 18S \nDatum:WGS84"
colnames(Data ) <- c("Mapa elaborado \nen  RStudio")
Tabla.p <- ggtexttable(Data, rows = NULL,theme =ttheme( base_size =6, "lBlackWhite"))

Data1     <- read.xlsx("Excel/Embrete.xlsx", sheet="Hoja1") 
Tabla.p1 <- ggtexttable(Data1, rows = NULL,theme =ttheme( base_size =6, "mBlue"))
#-----------------------------------------------------------------------

A <-ggplot()+ 
  geom_raster(data = df, aes(lon,lat,fill = alt),alpha=0.75) + 
  geom_raster(data = df_a, aes(x=x, y=y, alpha=aspect),fill="gray20")+
  scale_alpha(guide=FALSE,range = c(0,1.00))   +
  scale_fill_distiller(palette   = "RdYlBu",name="Elevacion \n(m.s.n.m)",
                       labels = c("[1 - 270] ","[270-400]", "[400-700]", "[700-1200]", "[1200-1700]",
                                  "[1700-2200]", "[2200-3500]", "[3500-3700]", "[3700-4100]", "[4100-4286]"),
                       breaks = c(0, 270, 400,700,1200,1700,2200,3500,3700,4100))+
  theme_bw()+coord_equal()+
  guides(fill = guide_legend(title.position = "top",direction = "vertical",
                             title.theme = element_text(angle = 0, size = 9, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Elevacion \n(m.s.n.m)'))+
  geom_sf(data=Cuencas_rios, color="blue", size=0.3)+
  geom_sf(data=Cuencas_rios1, color="blue", size=0.3)+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(legend.position = c(0.85,0.20),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        legend.background = element_blank(),
        plot.title=element_text(color="#666666", size=12, vjust=1.25,  family="Raleway",  hjust = 0.5),
        legend.box.just = "left",
        legend.text = element_text(size=9,face=2),
        legend.title = element_text(size=9,face=2))+
  guides(fill = guide_legend(nrow = 5, ncol=2))+
  annotation_custom(ggplotGrob(Tabla.p ),
                    xmin = -79.2, xmax = -79,ymin = -8.2,ymax = -8)+
  annotate(geom = "text", x = -78.7, y = -8.18, 
           label = "Quieres desarrollar mapas de este tipo \n inscribete a \nAPRENDE R DESDE CERO PARA SIG ", 
           fontface = "italic", color = "black", size = 3)

B <-ggplot()+
  geom_sf(data= Peru, fill="white")+
  geom_sf(data= Cuenca_Chicama, fill="red")+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "black", size= 1))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering (),
                         height = unit(0.8, "cm"),# tamaño altura
                         width = unit(0.8, "cm"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -78, y = -17, 
           label = "Mapa de Macrolocalizacion", 
           fontface = "italic", color = "black", size = 3)

C <-ggplot()+
  geom_sf(data= Peru, fill=NA)+
  geom_sf(data= Cuenca_Chicama, fill="gray")+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "black", size= 1))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering (),
                         height = unit(0.8, "cm"),# tamaño altura
                         width = unit(0.8, "cm"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -78.8, y = -8, 
           label = "Mapa Departamental", 
           fontface = "italic", color = "black", size = 2)+
  annotate(geom = "text", x = -78.8, y = -7.6, 
           label = "Mapa Departamental", 
           fontface = "italic", color = "black", size = 2)+
  coord_sf(xlim = c( -79.3,-78.1 ), ylim = c( -8.5,-6.9),expand = FALSE)



D <-ggplot()+
  geom_raster(data = df, aes(lon,lat,fill = alt), show.legend = F)+
  scale_fill_distiller(palette   = "RdYlBu",name="Elevacion \n(m.s.n.m)",
                       labels = c("[1 - 270] ","[270-400]", "[400-700]", "[700-1200]", "[1200-1700]",
                                  "[1700-2200]", "[2200-3500]", "[3500-3700]", "[3700-4100]", "[4100-4286]"),
                       breaks = c(0, 270, 400,700,1200,1700,2200,3500,3700,4100))+
  theme_bw()+coord_equal()+
  guides(fill = guide_legend(title.position = "top",direction = "vertical",
                             title.theme = element_text(angle = 0, size = 9, colour = "black"),
                             barheight = .5, barwidth = .95,
                             title.hjust = 0.5, raster = FALSE,
                             title = 'Elevacion \n(m.s.n.m)'))+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(legend.position = c(0.65,0.15),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        legend.background = element_blank(),
        plot.title=element_text(color="#666666", size=12, vjust=1.25,  family="Raleway",  hjust = 0.5),
        legend.box.just = "left",
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia 
        legend.text = element_text(size=8,face=2),
        legend.title = element_text(size=8,face=2))+
  guides(fill = guide_legend(nrow = 5, ncol=2))


E <-ggplot()+ 
  geom_raster(data = df, aes(lon,lat,fill = alt), show.legend = F) + 
  geom_raster(data = df_a, aes(x=x, y=y, alpha=aspect), show.legend = F)+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "Greys"), 
                       na.value = 'white')+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_bw()


Fa <-ggplot() +
  coord_equal(xlim = c(0, 28), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(A), xmin = 0, xmax = 20, ymin = 4, ymax = 20)+
  annotation_custom(ggplotGrob(B), xmin = 19.5, xmax = 23.5, ymin = 13.5, ymax = 20) +
  annotation_custom(ggplotGrob(C), xmin = 23.5, xmax = 28, ymin = 13.5, ymax = 20) +
  annotation_custom(ggplotGrob(D), xmin = 19.5, xmax = 28, ymin = 8, ymax = 14) +
  annotation_custom(ggplotGrob(E), xmin = 19.5, xmax = 28, ymin = 2, ymax = 8) +
  annotation_custom(ggplotGrob(Tabla.p1 ),
                    xmin = 4, xmax = 10,ymin = 0,ymax = 4)+
  theme_bw()+
  labs(title="MAPA DE ELEVACIONES CUENCA CHICAMA", 
       subtitle="Elevacion con aspecto con ggplot", 
       caption="Fuente: https://www.geogpsperu.com/2018/08/descargar-imagenes-aster-gdem-aster.html", 
       color=NULL)


ggsave(plot = Fa ,"Mpas/Chicama_elevacion.png", units = "cm", width = 30,height = 22, dpi = 900) 





