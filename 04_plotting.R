library(tidyverse)
source("00.findmin.R")
library(lubridate)
library(mgcv)
library(splines)
library(stats)
library(dlnm)
library(patchwork)
source("00.attrdl.R")
options(scipen=999)
theme_set(theme_classic())

############### English #########################

# Deaths and average temperature distribution -----------------------------

deaths <- ggplot(data=dados,aes(x=Data, y=Casos)) +
  geom_point(alpha=0.1, size = 0.2,color = "grey") +
  geom_smooth(color="black") +
  ylab("Daily deaths")+
  xlab("")+
  facet_wrap(~Microregiao,ncol=2, scales = "free")+
  ggtitle('A')

avg_temp <- ggplot(data=dados,aes(x=Data, y=Temp_med)) +
  geom_point(alpha=0.1, size = 0.2,color = "grey") +
  geom_smooth(color="black") +
  xlab("")+
  ylab('Average temperature ºC')+
  facet_wrap(~Microregiao,ncol=2)+
  ggtitle('B')

deaths+avg_temp
ggsave("figures/fig1_eng.jpeg", 
       dpi = 300, width = 5000, 
       height = 3000, units = "px")

# Saving models -----------------------------------------------------------

for (i in dados$Microregiao %>% unique){
  print(i)
  temp_data <- dados %>% filter(Microregiao == i)
  temp = temp_data$Temp_med
  temp_data$tempo <- 1:8036
  ### DLNM MODELS
  # temperature quantis
  round(quantile(temp, probs = c(0,0.01,0.025,0.1,0.25,0.5,0.75,0.9,0.975,0.99,1)),1)
  P0 = round(quantile(temp,probs=0),1)
  P1 = round(quantile(temp,probs=0.01),1)
  P2.5 = round(quantile(temp,probs=0.025),1)
  P10 = round(quantile(temp,probs=0.1),1)
  P25 = round(quantile(temp,probs=0.25),1)
  P50 = round(quantile(temp,probs=0.5),1)
  P75 = round(quantile(temp,probs=0.75),1)
  P90 = round(quantile(temp,probs=0.9),1)
  P97.5 = round(quantile(temp,probs=0.975),1)
  P99 = round(quantile(temp,probs=0.99),1)
  P100 = round(quantile(temp,probs=1),1)
  ## Creating cross-basis objects for Temperature (lags up to 14 days)
  
  cb <- crossbasis(temp, 
                   lag=21, 
                   argvar=list(fun="ns",df=5),
                   arglag=list(fun="poly",degree=4))
  ## adjusting the GAM / DLMN model with negative binomial distribution
  
  mod <- gam(Casos~cb+ns(tempo,22*8)+Dia,family= nb,data= temp_data)
  
  ## ESTIMATING THE MINIMUM RISK TEMPERATURE
  # Ref: Tobías et al., 2017.
  
  MMT <- findmin(cb,
                 mod,
                 from= P1,
                 to= P99)
  save(MMT,file = paste0("output/",i,"_MMT.RData"))
  
  ## Prediction model with minimum risk temperature as reference
  pred <- crosspred(basis=cb,
                    model=mod,
                    from=P0,
                    to=P100,
                    by=0.1,
                    cumul=TRUE,
                    cen=MMT)
  save(pred,file = paste0("output/","pred_",i,".RData"))
  
  ##check point 
  print(paste0(i," done"))
}

#data for plotting
files <- list.files("output", full.names = T, pattern = "pred")
names <- files %>% 
  str_remove("output/pred_") %>% 
  str_remove(".RData")
MMT_files <- list.files("output",full.names = T, pattern = "MMT")


# Accumulated Risk by city ------------------------------------------------
jpeg("figures/fig2_eng.jpeg", res = 300, width = 3000, height= 3500)
par(mar=c(1,1,1,1), mfrow=c(5,2), mai = c(0.7,0.7,0.5,0.2))
for (i in 1:10){
  name <- names[i]
  print(name)
  load(files[i])
  load(MMT_files[i])
  temp_data <- dados %>% filter(Microregiao == name)
  temp = temp_data$Temp_med
  temp_data$tempo <- 1:8036
  # temperature quantis
  round(quantile(temp, probs = c(0,0.01,0.025,0.1,0.25,0.5,0.75,0.9,0.975,0.99,1)),1)
  P0 = round(quantile(temp,probs=0),1)
  P1 = round(quantile(temp,probs=0.01),1)
  P2.5 = round(quantile(temp,probs=0.025),1)
  P10 = round(quantile(temp,probs=0.1),1)
  P25 = round(quantile(temp,probs=0.25),1)
  P50 = round(quantile(temp,probs=0.5),1)
  P75 = round(quantile(temp,probs=0.75),1)
  P90 = round(quantile(temp,probs=0.9),1)
  P97.5 = round(quantile(temp,probs=0.975),1)
  P99 = round(quantile(temp,probs=0.99),1)
  P100 = round(quantile(temp,probs=1),1)
  ## rugged and line plot
  plot(pred,"overall",ylab="RR (lag 0-21)",xlab="Temperature (ºC)",
       axes=T,lwd=1.5, main = paste0(name %>% str_to_title()),cex=0.75)
  rug(temp_data$Temp_med)
  abline(v=MMT)

  }
dev.off()

# Plot RR at specific temperature for lag range (non-cumulative) -COLD--TEMP---
jpeg("figures/figS2_eng.jpeg", res = 300, width = 3000, height= 3500)
par(mar=c(4,4,4,4),mfrow=c(5,4))
for (i in 1:10){
  name <- names[i]
  print(name)
  load(files[i])
  load(MMT_files[i])
  temp_data <- dados %>% filter(Microregiao == name)
  temp = temp_data$Temp_med
  temp_data$tempo <- 1:8036
  # temperature quantis
  round(quantile(temp, probs = c(0,0.01,0.025,0.1,0.25,0.5,0.75,0.9,0.975,0.99,1)),1)
  P0 = round(quantile(temp,probs=0),1)
  P1 = round(quantile(temp,probs=0.01),1)
  P2.5 = round(quantile(temp,probs=0.025),1)
  P10 = round(quantile(temp,probs=0.1),1)
  P25 = round(quantile(temp,probs=0.25),1)
  P50 = round(quantile(temp,probs=0.5),1)
  P75 = round(quantile(temp,probs=0.75),1)
  P90 = round(quantile(temp,probs=0.9),1)
  P97.5 = round(quantile(temp,probs=0.975),1)
  P99 = round(quantile(temp,probs=0.99),1)
  P100 = round(quantile(temp,probs=1),1)
  
  plot(pred,"slices",var=as.character(P2.5),lty=1,cumul=F,ylab="RR", 
       main=paste(name %>% str_to_title(),P2.5,"ºC","(P2.5)"))
  plot(pred,"slices",var=as.character(P10),lty=1,cumul=F,ylab="RR", 
       main=paste(name %>% str_to_title(),P10,"ºC","(P10)"))
}
dev.off()

# Plot RR at specific temperature for lag range (non-cumulative) -WARM--TEMP---
jpeg("figures/figS3_eng.jpeg", res = 300, width = 3000, height= 3500)
par(mar=c(4,4,4,4),mfrow=c(5,4))
for (i in 1:10){
  name <- names[i]
  print(name)
  load(files[i])
  load(MMT_files[i])
  temp_data <- dados %>% filter(Microregiao == name)
  temp = temp_data$Temp_med
  temp_data$tempo <- 1:8036
  # temperature quantis
  round(quantile(temp, probs = c(0,0.01,0.025,0.1,0.25,0.5,0.75,0.9,0.975,0.99,1)),1)
  P0 = round(quantile(temp,probs=0),1)
  P1 = round(quantile(temp,probs=0.01),1)
  P2.5 = round(quantile(temp,probs=0.025),1)
  P10 = round(quantile(temp,probs=0.1),1)
  P25 = round(quantile(temp,probs=0.25),1)
  P50 = round(quantile(temp,probs=0.5),1)
  P75 = round(quantile(temp,probs=0.75),1)
  P90 = round(quantile(temp,probs=0.9),1)
  P97.5 = round(quantile(temp,probs=0.975),1)
  P99 = round(quantile(temp,probs=0.99),1)
  P100 = round(quantile(temp,probs=1),1)
  
  plot(pred,"slices",var=as.character(P90),lty=1,cumul=F,ylab="RR", 
       main=paste(name %>% str_to_title(),P90,"ºC","(P90)"))
  plot(pred,"slices",var=as.character(P97.5),lty=1,cumul=F,ylab="RR", 
       main=paste(name %>% str_to_title(),P97.5,"ºC","(P97.5)"))
}
dev.off()

# Sensitivity analysis ----------------------------------------------------

theme_set(theme_classic())
for (i in dados$Microregiao %>% unique){
  print(i)
  sens_city <- table6 %>% filter(Region == i) 
  sens_city_model1 <- sens_city %>% filter(model == 1)
  # Plot
  sens_city_simple <- sens_city %>% 
    ggplot(aes(x = model, y = RR))+
    geom_point()+
    geom_errorbar(aes(ymin = `IC min`, ymax = `IC max`),
                  width = 0.8)+
    scale_x_continuous(breaks = seq(1,12))+
    facet_wrap(~name )+
    ggtitle(paste0(i %>% str_to_title()))
  
  # Comparative with model 1
  p <- sens_city_simple + 
    geom_hline(data = sens_city_model1, 
               aes(yintercept = RR))+
    geom_hline(data = sens_city_model1, 
               aes(yintercept = `IC min`), color = "grey", linetype = "dashed")+
    geom_hline(data = sens_city_model1, 
               aes(yintercept = `IC max`), color = "grey", linetype = "dashed")
  
  assign(paste0(i) %>% str_remove_all(" "),p)
}

MANAUS+BELEM+FORTALEZA+SALVADOR+
  RIODEJANEIRO+SAOPAULO+
  CURITIBA+PORTOALEGRE+
  CAMPOGRANDE+BRASILIA
ggsave("figures/figS1_eng.jpeg", dpi = 300, width = 5000, height = 3000, units = "px")



############### Portuguese #########################

# Deaths and average temperature distribution -----------------------------

# jpeg("grafico_Casos.jpg",width=10, height=10,units="in",res=144)
deaths <- ggplot(data=dados,aes(x=Data, y=Casos)) +
  geom_point(alpha=0.1, size = 0.2,color = "grey") +
  geom_smooth(color="black") +
  ylab("Mortes diárias")+
  xlab("")+
  facet_wrap(~Microregiao,ncol=2, scales = "free")+
  ggtitle('A')


# jpeg("grafico_Temp_med.jpg",width=10, height=10,units="in",res=144)
avg_temp <- ggplot(data=dados,aes(x=Data, y=Temp_med)) +
  geom_point(alpha=0.1, size = 0.2,color = "grey") +
  geom_smooth(color="black") +
  xlab("")+
  ylab('Temperatura média (ºC)')+
  facet_wrap(~Microregiao,ncol=2)+
  ggtitle('B')

deaths+avg_temp
ggsave("figures/fig1_pt.jpeg", dpi = 300, width = 5000, height = 3000, units = "px")

# Accumulated Risk by city ------------------------------------------------
jpeg("figures/fig2_pt.jpeg", res = 300, width = 3000, height= 3500)
par(mar=c(1,1,1,1), mfrow=c(5,2), mai = c(0.7,0.7,0.5,0.2))

for (i in 1:10){
  name <- names[i]
  print(name)
  load(files[i])
  load(MMT_files[i])
  temp_data <- dados %>% filter(Microregiao == name)
  temp = temp_data$Temp_med
  temp_data$tempo <- 1:8036
  # temperature quantis
  round(quantile(temp, probs = c(0,0.01,0.025,0.1,0.25,0.5,0.75,0.9,0.975,0.99,1)),1)
  P0 = round(quantile(temp,probs=0),1)
  P1 = round(quantile(temp,probs=0.01),1)
  P2.5 = round(quantile(temp,probs=0.025),1)
  P10 = round(quantile(temp,probs=0.1),1)
  P25 = round(quantile(temp,probs=0.25),1)
  P50 = round(quantile(temp,probs=0.5),1)
  P75 = round(quantile(temp,probs=0.75),1)
  P90 = round(quantile(temp,probs=0.9),1)
  P97.5 = round(quantile(temp,probs=0.975),1)
  P99 = round(quantile(temp,probs=0.99),1)
  P100 = round(quantile(temp,probs=1),1)
  ##check point 
  print(paste0(i," done"))
  
  
  ## rugged and line plot
  plot(pred,"overall",ylab="RR (lag 0-21)",xlab="Temperatura (ºC)",
       axes=T,lwd=1.5, main = paste0(name %>% str_to_title()),cex=0.75)
  rug(temp_data$Temp_med)
  abline(v=MMT)
}

dev.off()

# Plot RR at specific temperature for lag range (non-cumulative) -COLD--TEMP---
jpeg("figures/figS2_pt.jpeg", res = 300, width = 3000, height= 3500)
par(mar=c(4,4,4,4),mfrow=c(5,4))
for (i in 1:10){
  name <- names[i]
  print(name)
  load(files[i])
  load(MMT_files[i])
  temp_data <- dados %>% filter(Microregiao == name)
  temp = temp_data$Temp_med
  temp_data$tempo <- 1:8036
  # temperature quantis
  round(quantile(temp, probs = c(0,0.01,0.025,0.1,0.25,0.5,0.75,0.9,0.975,0.99,1)),1)
  P0 = round(quantile(temp,probs=0),1)
  P1 = round(quantile(temp,probs=0.01),1)
  P2.5 = round(quantile(temp,probs=0.025),1)
  P10 = round(quantile(temp,probs=0.1),1)
  P25 = round(quantile(temp,probs=0.25),1)
  P50 = round(quantile(temp,probs=0.5),1)
  P75 = round(quantile(temp,probs=0.75),1)
  P90 = round(quantile(temp,probs=0.9),1)
  P97.5 = round(quantile(temp,probs=0.975),1)
  P99 = round(quantile(temp,probs=0.99),1)
  P100 = round(quantile(temp,probs=1),1)
  
  plot(pred,"slices",var=as.character(P2.5),lty=1,cumul=F,ylab="RR", 
       main=paste(name %>% str_to_title(),P2.5,"ºC","(P2.5)"))
  plot(pred,"slices",var=as.character(P10),lty=1,cumul=F,ylab="RR", 
       main=paste(name %>% str_to_title(),P10,"ºC","(P10)"))
}
dev.off()

# Plot RR at specific temperature for lag range (non-cumulative) -WARM--TEMP---
jpeg("figures/figS3_pt.jpeg", res = 300, width = 3000, height= 3500)
par(mar=c(4,4,4,4),mfrow=c(5,4))
for (i in 1:10){
  name <- names[i]
  print(name)
  load(files[i])
  load(MMT_files[i])
  temp_data <- dados %>% filter(Microregiao == name)
  temp = temp_data$Temp_med
  temp_data$tempo <- 1:8036
  # temperature quantis
  round(quantile(temp, probs = c(0,0.01,0.025,0.1,0.25,0.5,0.75,0.9,0.975,0.99,1)),1)
  P0 = round(quantile(temp,probs=0),1)
  P1 = round(quantile(temp,probs=0.01),1)
  P2.5 = round(quantile(temp,probs=0.025),1)
  P10 = round(quantile(temp,probs=0.1),1)
  P25 = round(quantile(temp,probs=0.25),1)
  P50 = round(quantile(temp,probs=0.5),1)
  P75 = round(quantile(temp,probs=0.75),1)
  P90 = round(quantile(temp,probs=0.9),1)
  P97.5 = round(quantile(temp,probs=0.975),1)
  P99 = round(quantile(temp,probs=0.99),1)
  P100 = round(quantile(temp,probs=1),1)
  
  plot(pred,"slices",var=as.character(P90),lty=1,cumul=F,ylab="RR", 
       main=paste(name %>% str_to_title(),P90,"ºC","(P90)"))
  plot(pred,"slices",var=as.character(P97.5),lty=1,cumul=F,ylab="RR", 
       main=paste(name %>% str_to_title(),P97.5,"ºC","(P97.5)"))
}

dev.off()

# Sensitivity analysis ----------------------------------------------------

theme_set(theme_classic())
for (i in dados$Microregiao %>% unique){
  print(i)
  sens_city <- table6 %>% filter(Region == i) 
  sens_city_model1 <- sens_city %>% filter(model == 1)
  sens_city_model1 %>% mutate(name = name %>% 
                                str_replace('percentile', 'percentil'))
  # Plot
  sens_city_simple <- sens_city %>% 
    ggplot(aes(x = model, y = RR))+
    geom_point()+
    xlab("Modelo")+
    geom_errorbar(aes(ymin = `IC min`, ymax = `IC max`),
                  width = 0.8)+
    scale_x_continuous(breaks = seq(1,12))+
    facet_wrap(~name )+
    ggtitle(paste0(i %>% str_to_title()))
  
  # Comparative with model 1
  p <- sens_city_simple + 
    geom_hline(data = sens_city_model1, 
               aes(yintercept = RR))+
    geom_hline(data = sens_city_model1, 
               aes(yintercept = `IC min`), color = "grey", linetype = "dashed")+
    geom_hline(data = sens_city_model1, 
               aes(yintercept = `IC max`), color = "grey", linetype = "dashed")
  
  assign(paste0(i) %>% str_remove_all(" "),p)
}

MANAUS+BELEM+FORTALEZA+SALVADOR+
  RIODEJANEIRO+SAOPAULO+
  CURITIBA+PORTOALEGRE+
  CAMPOGRANDE+BRASILIA
ggsave("figures/figS1_pt.jpeg", dpi = 300, width = 5000, height = 3000, units = "px")
