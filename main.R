### MAPA ELEICAO PRESIDENTE ###

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(readxl,geobr,tidyverse,sf)



dados <- read_excel("data/eleicaoPresCidade.xlsx",sheet = "Planilha3")

est <- read_state(code_state = "all", year=2010)
mun <- read_municipality(code_muni="all", year=2010)

mun <- left_join(mun, dados, by= c("code_muni" = "id_municipio"))

mun2 <-mun %>% mutate(perda = PT_2014-PT_2018)


## Plot do mapa 2014
ggplot()+ 
  geom_sf(data=mun, aes(fill= PercPT2014),color='transparent')+
  geom_sf(fill='transparent',color='#F6F9FF',data=est, size=.5)+
  scale_fill_gradient2(low = "#0C4196", mid = "white", high = "#E62339", midpoint = .5)+
  theme_void()+
  theme(panel.background = element_rect(fill = '#F6F9FF', colour = '#F6F9FF'))+
  theme(legend.position = 'none',legend.title = element_blank(), )

ggsave(
  "mapa2014.png",
  plot = last_plot(),
  device = "png",
  path = "export",
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

## plot do mapa 2018
ggplot()+ 
  geom_sf(data=mun, aes(fill= PercPT2018),color='transparent')+
  geom_sf(fill='transparent',color='#F6F9FF',data=est, size=.5)+
  scale_fill_gradient2(low = "#297D1C", mid = "white", high = "#E62339", midpoint = .5)+
  theme_void()+
  theme(panel.background = element_rect(fill = '#F6F9FF', colour = '#F6F9FF'))+
  theme(legend.position = 'none',legend.title = element_blank(), )

ggsave(
  "mapa2018.png",
  plot = last_plot(),
  device = "png",
  path = "export",
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

##
#ggplot() + geom_sf(data=mun2, aes(fill= perda),color = 'transparent')+
#  scale_fill_gradient2(low = "#297D1C", mid = "white", high = "#E62339", midpoint = 0)
