### MAPA ELEICAO PRESIDENTE ###

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(readxl,geobr,tidyverse,sf,rChoiceDialogs,feather)


##
dados <- read_excel("data/eleicaoPresCidade.xlsx",sheet = "Planilha3")

if (!file.exists("data/est.Rdata")){
  est <- read_state(code_state = "all", year=2010)
  save(est, file = "data/est.Rdata")
} else {
  load("data/est.Rdata")
}
if (!file.exists("data/mun.Rdata")){
  mun <- read_municipality(code_muni="all", year=2010)
  save(mun, file = "data/mun.Rdata")
} else {
  load("data/mun.Rdata")
}

#
mun2 <- left_join(mun, dados, by= c("code_muni" = "id_municipio"))

#mun2 <-mun %>% mutate(perda = PT_2014-PT_2018)


choosepath <- function() {
  if (interactive()) {
  
    path <- rchoose.dir() 
  } else {
  
    path <- "export"
  }
  return(path)
}

path <- choosepath()

## Plot do mapa 2014
ggplot()+ 
  geom_sf(data=mun2, aes(fill= PercPT2014),color='transparent')+
  geom_sf(fill='transparent',color='#F6F9FF',data=est, size=.5)+
  scale_fill_gradient2(low = "#0C4196", mid = "white", high = "#E62339", midpoint = .5)+
  theme_void()+
  theme(panel.background = element_rect(fill = '#F6F9FF', colour = '#F6F9FF'))+
  theme(legend.position = 'none',legend.title = element_blank(), )

ggsave(
  "mapa2014.png",
  plot = last_plot(),
  device = "png",
  path = path,
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
  geom_sf(data=mun2, aes(fill= PercPT2018),color='transparent')+
  geom_sf(fill='transparent',color='#F6F9FF',data=est, size=.5)+
  scale_fill_gradient2(low = "#297D1C", mid = "white", high = "#E62339", midpoint = .5)+
  theme_void()+
  theme(panel.background = element_rect(fill = '#F6F9FF', colour = '#F6F9FF'))+
  theme(legend.position = 'none',legend.title = element_blank(), )

ggsave(
  "mapa2018.png",
  plot = last_plot(),
  device = "png",
  path = path,
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
