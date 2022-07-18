
# Removing sci notation
options(scipen=999)
# loading libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(graphics)
theme_set(theme_bw())

#######################
## Exploratory analysis
#######################

## Dispersion and correlation matrix

panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y, method = "spearman"), digits=2)
  text(0.5, 0.5, r, cex = 1)
}
temp <- dados[dados$Microregiao=="RIO DE JANEIRO",c(13,8:11)]
n = length(temp)
variavel = names(temp)
jpeg("dispersao_riodejaneiro.jpg",width=7, height=5,units="in",res=144)
par(mar=c(2,2,2,2),mfrow=c(1,1))
pairs(temp, upper.panel=panel.smooth, lower.panel=panel.cor, cex.labels = 1)
dev.off()

temp <- dados[dados$Microregiao=="BELEM",c(13,8:11)]
n = length(temp)
variavel = names(temp)
jpeg("dispersao_belem.jpg",width=7, height=5,units="in",res=144)
par(mar=c(2,2,2,2),mfrow=c(1,1))
pairs(temp, upper.panel=panel.smooth, lower.panel=panel.cor,cex.labels = 1)
dev.off()

corrplot::corrplot.mixed(cor(dados[c(13,8:11)], method = "spearman"))

emp <- dados[dados$Microregiao=="BRASILIA",c(13,8:11)]
n = length(temp)
variavel = names(temp)
jpeg("dispersao_brasilia.jpg",width=7, height=5,units="in",res=144)
par(mar=c(2,2,2,2),mfrow=c(1,1))
pairs(temp, upper.panel=panel.smooth, lower.panel=panel.cor,cex.labels = 1)
dev.off()

temp <- dados[dados$Microregiao=="CAMPO GRANDE",c(13,8:11)]
n = length(temp)
variavel = names(temp)
jpeg("dispersao_campogrande.jpg",width=7, height=5,units="in",res=144)
par(mar=c(2,2,2,2),mfrow=c(1,1))
pairs(temp, upper.panel=panel.smooth, lower.panel=panel.cor,cex.labels = 1)
dev.off()

temp <- dados[dados$Microregiao=="CURITIBA",c(13,8:11)]
n = length(temp)
variavel = names(temp)
jpeg("dispersao_curitiba.jpg",width=7, height=5,units="in",res=144)
par(mar=c(2,2,2,2),mfrow=c(1,1))
pairs(temp, upper.panel=panel.smooth, lower.panel=panel.cor,cex.labels = 1)
dev.off()

temp <- dados[dados$Microregiao=="FORTALEZA",c(13,8:11)]
n = length(temp)
variavel = names(temp)
jpeg("dispersao_fortaleza.jpg",width=7, height=5,units="in",res=144)
par(mar=c(2,2,2,2),mfrow=c(1,1))
pairs(temp, upper.panel=panel.smooth, lower.panel=panel.cor,cex.labels = 1)
dev.off()

temp <- dados[dados$Microregiao=="MANAUS",c(13,8:11)]
n = length(temp)
variavel = names(temp)
jpeg("dispersao_manaus.jpg",width=7, height=5,units="in",res=144)
par(mar=c(2,2,2,2),mfrow=c(1,1))
pairs(temp, upper.panel=panel.smooth, lower.panel=panel.cor,cex.labels = 1)
dev.off()

temp <- dados[dados$Microregiao=="PORTO ALEGRE",c(13,8:11)]
n = length(temp)
variavel = names(temp)
jpeg("dispersao_portoalegre.jpg",width=7, height=5,units="in",res=144)
par(mar=c(2,2,2,2),mfrow=c(1,1))
pairs(temp, upper.panel=panel.smooth, lower.panel=panel.cor,cex.labels = 1)
dev.off()

temp <- dados[dados$Microregiao=="SALVADOR",c(13,8:11)]
n = length(temp)
variavel = names(temp)
jpeg("dispersao_salvador.jpg",width=7, height=5,units="in",res=144)
par(mar=c(2,2,2,2),mfrow=c(1,1))
pairs(temp, upper.panel=panel.smooth, lower.panel=panel.cor,cex.labels = 1)
dev.off()

temp <- dados[dados$Microregiao=="SAO PAULO",c(13,8:11)]
n = length(temp)
variavel = names(temp)
jpeg("dispersao_saopaulo.jpg",width=7, height=5,units="in",res=144)
par(mar=c(2,2,2,2),mfrow=c(1,1))
pairs(temp, upper.panel=panel.smooth, lower.panel=panel.cor,cex.labels = 1)
dev.off()

rm(temp,n,variavel,panel.cor)

## scatter plots of all regions by temperature variable

# Temp_med
jpeg("dispersao_Temp_med.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=Temp_med, y=Casos)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2,scales="free")
ggsave("dispersao_Temp_med.jpg")
dev.off()

jpeg("dispersao_Temp_min.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=Temp_min, y=Casos)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2,scales="free")
dev.off()

# Temp_max
jpeg("dispersao_Temp_max.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=Temp_max, y=Casos)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2,scales="free")
dev.off()

# Temp_amp
jpeg("dispersao_Temp_amp.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=Temp_amp, y=Casos)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2,scales="free")
dev.off()
