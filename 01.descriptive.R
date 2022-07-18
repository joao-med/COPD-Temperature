# Removing sci notation
options(scipen=999)
# loading libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
theme_set(theme_bw())

#######################
## Descriptive analysis
#######################

# general daily distribution of variables
summary(dados)
dados$Casos %>% sum
# Death daily distribution by microregion
tab_Casos <- dados %>%
  group_by(Microregiao) %>% 
  summarize(Min=min(Casos),
            Q1=quantile(Casos,0.25),
            Q2=quantile(Casos,0.5),
            Q3=quantile(Casos,0.75),
            Max=max(Casos),
            Media=mean(Casos),
            DP=sd(Casos),
            Soma=sum(Casos))

# saving table
tab_Casos <- as.data.frame(tab_Casos)
write.table(tab_Casos,"tab_Casos.csv",sep=";",dec=".",row.names = F)
# Mean Temperature daily distribution by microregion
tab_Temp_med <- dados %>%
  group_by(Microregiao) %>% 
  summarize(Min=min(Temp_med),
            Q1=quantile(Temp_med,0.25),
            Q2=quantile(Temp_med,0.5),
            Q3=quantile(Temp_med,0.75),
            Max=max(Temp_med),
            Media=mean(Temp_med),
            DP=sd(Temp_med))
# saving table
tab_Temp_med <- as.data.frame(tab_Temp_med)
write.table(tab_Temp_med,"tab_Temp_med.csv",sep=";",dec=".",row.names = F)
# Minimum Temperature daily distribution by microregion
tab_Temp_min <- dados %>%
  group_by(Microregiao) %>% 
  summarize(Min=min(Temp_min),
            Q1=quantile(Temp_min,0.25),
            Q2=quantile(Temp_min,0.5),
            Q3=quantile(Temp_min,0.75),
            Max=max(Temp_min),
            Media=mean(Temp_min),
            DP=sd(Temp_min))
tab_Temp_min <- as.data.frame(tab_Temp_min)
write.table(tab_Temp_min,"tab_Temp_min.csv",sep=";",dec=".",row.names = F)
# Maximum Temperature daily distribution by microregion
tab_Temp_max <- dados %>%
  group_by(Microregiao) %>% 
  summarize(Min=min(Temp_max),
            Q1=quantile(Temp_max,0.25),
            Q2=quantile(Temp_max,0.5),
            Q3=quantile(Temp_max,0.75),
            Max=max(Temp_max),
            Media=mean(Temp_max),
            DP=sd(Temp_max))
# saving table
tab_Temp_max <- as.data.frame(tab_Temp_max)
write.table(tab_Temp_max,"tab_Temp_max.csv",sep=";",dec=".",row.names = F)
# Amplitude Temperature daily distribution by microregion
tab_Temp_amp <- dados %>%
    group_by(Microregiao) %>% 
    summarize(Min=min(Temp_amp),
              Q1=quantile(Temp_amp,0.25),
              Q2=quantile(Temp_amp,0.5),
              Q3=quantile(Temp_amp,0.75),
              Max=max(Temp_amp),
              Media=mean(Temp_amp),
               DP=sd(Temp_amp))
# saving table
tab_Temp_amp <- as.data.frame(tab_Temp_amp)
write.table(tab_Temp_amp,"tab_Temp_amp.csv",sep=";",dec=".",row.names = F)

# daily plots
jpeg("grafico_Casos.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=Data, y=Casos),col="blue") +
  geom_line(alpha=0.4, size = 0.2) +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2, scales = "free")
dev.off()

jpeg("grafico_Temp_med.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=Data, y=Temp_med),col="blue") +
  geom_point(alpha=0.1, size = 0.2) +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2)
dev.off()

jpeg("grafico_Temp_min.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=Data, y=Temp_min),col="blue") +
  geom_line() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2)
dev.off()

jpeg("grafico_Temp_max.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=Data, y=Temp_max),col="blue") +
  geom_line() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2)
dev.off()

jpeg("grafico_Temp_amp.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=Data, y=Temp_amp),col="blue") +
  geom_line() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2)
dev.off()

# monthly boxplots
jpeg("grafico_Casos_boxplot_mes.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=as.factor(Mes), y=Casos)) +
  geom_boxplot() +
  facet_wrap(~Microregiao,ncol=2,scale ="free")
dev.off()


jpeg("grafico_Temp_med_boxplot_mes.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=as.factor(Mes), y=Temp_med)) +
  geom_boxplot() +
  facet_wrap(~Microregiao,ncol=2) +
  labs(x="Mês", y="Temperatura média (ºC)")
dev.off()

jpeg("grafico_Temp_min_boxplot_mes.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=as.factor(Mes), y=Temp_min)) +
  geom_boxplot() +
  facet_wrap(~Microregiao,ncol=2)
dev.off()

jpeg("grafico_Temp_max_boxplot_mes.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=as.factor(Mes), y=Temp_max)) +
  geom_boxplot() +
  facet_wrap(~Microregiao,ncol=2)
dev.off()

jpeg("grafico_Temp_amp_boxplot_mes.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados,aes(x=as.factor(Mes), y=Temp_amp)) +
  geom_boxplot() +
  facet_wrap(~Microregiao,ncol=2)
dev.off()

# average daily distribution by month and microregion 
dados_micro_mes <- dados %>%
  group_by(Microregiao,Mes) %>%
  summarise(Casos=mean(Casos),
            Temp_max=mean(Temp_max),
            Temp_med=mean(Temp_med),
            Temp_min=mean(Temp_min),
            Temp_amp=mean(Temp_amp))
dados_micro_mes <- as.data.frame(dados_micro_mes)

# Monthly average of daily cases plot
jpeg("grafico_Casos_media_mes.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados_micro_mes,aes(x=Mes, y=Casos),col="blue") +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2, scales = "free")
dev.off()

# Monthly average of daily temperature
names(dados_micro_mes)
temp <- melt(dados_micro_mes[,c(1,2,4,5,6,7)], id=c("Microregiao","Mes"))
names(temp) <- c("Microrregião","Mês","Variável","Temperatura")
jpeg("grafico_Temp_media_mes.jpg",width=10, height=10,units="in",res=144)
ggplot(data=temp,aes(x=Mês, y=Temperatura,col=Variável)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Microrregião,ncol=2) +
  scale_x_continuous(breaks = c(1:12))
dev.off()
rm(temp)

# Annual distribution by microregion
dados_micro_ano <- dados %>%
  group_by(Microregiao,Ano) %>%
  summarise(Casos = sum(Casos),
            Pop = max(Pop),
            Temp_max = max(Temp_max),
            Temp_med=mean(Temp_med),
            Temp_min=min(Temp_min),
            Temp_amp=mean(Temp_amp)) %>%
  mutate(Taxa=round(Casos/Pop*100000,1))
dados_micro_ano <- as.data.frame(dados_micro_ano)

# Annual plots of cases, popupation and rate
jpeg("grafico_Casos_ano.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados_micro_ano,aes(x=Ano, y=Casos),col="blue") +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2)
dev.off()

jpeg("grafico_Pop_ano.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados_micro_ano,aes(x=Ano, y=Pop),col="blue") +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2)
dev.off()

jpeg("grafico_Taxa_ano.jpg",width=10, height=10,units="in",res=144)
ggplot(data=dados_micro_ano,aes(x=Ano, y=Taxa),col="blue") +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Microregiao,ncol=2)
dev.off()

# Average annual daily temperature plots
names(dados_micro_ano)
temp <- melt(dados_micro_ano[,c(1,2,5,6,7,8)], id=c("Microregiao","Ano"))
names(temp) <- c("Microrregião","Ano","Variável","Temperatura")
jpeg("grafico_Temp_ano.jpg",width=10, height=10,units="in",res=144)
ggplot(data=temp,aes(x=Ano, y=Temperatura,col=Variável)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Microrregião,ncol=2)
dev.off()
rm(temp)

# tables for each variable by year
tab_Casos_ano <- dcast(dados_micro_ano, Microregiao ~ Ano, value.var="Casos")
write.table(tab_Casos_ano,"tab_Casos_ano.csv",sep=";",dec=".",row.names = F)

tab_Pop_ano <- dcast(dados_micro_ano, Microregiao ~ Ano, value.var="Pop")
write.table(tab_Pop_ano,"tab_Pop_ano.csv",sep=";",dec=".",row.names = F)

tab_Taxa_ano <- dcast(dados_micro_ano, Microregiao ~ Ano, value.var="Taxa")
write.table(tab_Taxa_ano,"tab_Taxa_ano.csv",sep=";",dec=".",row.names = F)

tab_Temp_med_ano <- dcast(dados_micro_ano, Microregiao ~ Ano, value.var="Temp_med")
write.table(tab_Temp_med_ano,"tab_Temp_med_ano.csv",sep=";",dec=".",row.names = F)

tab_Temp_max_ano <- dcast(dados_micro_ano, Microregiao ~ Ano, value.var="Temp_max")
write.table(tab_Temp_max_ano,"tab_Temp_max_ano.csv",sep=";",dec=".",row.names = F)

tab_Temp_min_ano <- dcast(dados_micro_ano, Microregiao ~ Ano, value.var="Temp_min")
write.table(tab_Temp_min_ano,"tab_Temp_min_ano.csv",sep=";",dec=".",row.names = F)

tab_Temp_amp_ano <- dcast(dados_micro_ano, Microregiao ~ Ano, value.var="Temp_amp")
write.table(tab_Temp_amp_ano,"tab_Temp_amp_ano.csv",sep=";",dec=".",row.names = F)

