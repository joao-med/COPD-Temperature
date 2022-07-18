library(tidyverse)
library(xlsx)
source("findmin.R")
library(lubridate)
library(mgcv)
library(splines)
library(stats)
library(dlnm)
source("attrdl.R")
options(scipen=999)


# Generating tables, models and sensitivity analysis in loop --------------

# Creating table 1
# Data regarding descriptive statistics of each city
big_table1 <- tibble()
for (i in dados$Microregiao %>% unique){
  temp_data <- dados %>% filter(Microregiao == i)
  mean_population <- temp_data %>%
    group_by(Ano) %>% 
    summarise(pop = Pop %>% unique()) %>% 
    summarise(Mean_Population = mean(pop))
  deaths_total <- temp_data$Casos %>% sum()
  annual_death_mean <- round(deaths_total/22)
  average_annual_rate <- round((sum(temp_data$Casos)/22)/mean(temp_data$Pop, na.rm =T)*100000,2)
  distribution <- round(summary(temp_data$Casos)) %>%
    as.vector() %>%
    as.tibble %>%
    t()
  colnames(distribution) <-  
    c("Min", "Percentile_25", "Median", "Mean", "Percentile_75", "Max")
  distribution <- distribution %>% as.tibble
  average_daily_rate <- mean((temp_data$Casos)/(temp_data$Pop)*100000)
  
  lil_table1 <- tibble(variables = i,
                       mean_population,
                       deaths_total, 
                       annual_death_mean,
                       distribution,
                       average_annual_rate,
                       average_daily_rate)
  
  big_table1 <- bind_rows(lil_table1, big_table1)
}

table1 <- big_table1 %>% t()

# Creating table 2
# Data regarding descriptive statistics of temperature of each city
big_table2 <- tibble()

for (i in dados$Microregiao %>% unique) {
  temp_data <- dados %>% filter(Microregiao == i)
  temp = temp_data$Temp_med
  cities <- as.vector(dados$Microregiao %>% unique)
  
  # average temperature values
  round(mean(temp),1)
  
  # temperature quantis
  summary_temp <- round(summary(temp_data$Temp_med),1) %>%
    as.vector() %>%
    as.tibble %>%
    t()
  colnames(summary_temp) <-  
    c("Min", "Percentile_25", "Median", "Mean", "Percentile_75", "Max")
  summary_temp <- summary_temp %>% as.tibble
  
  P0 <-  round(quantile(temp,probs=0),1) %>% tibble("0%" = .)
  P1 <-  round(quantile(temp,probs=0.01),1) %>% tibble("1%" = .)
  P2.5 = round(quantile(temp,probs=0.025),1) %>% tibble("2.5%" = .)
  P10 = round(quantile(temp,probs=0.1),1) %>% tibble("10%" = .)
  P25 = round(quantile(temp,probs=0.25),1) %>% tibble("25%" = .)
  P50 = round(quantile(temp,probs=0.5),1) %>% tibble("50%" = .)
  P75 = round(quantile(temp,probs=0.75),1) %>% tibble("75%" = .)
  P90 = round(quantile(temp,probs=0.9),1) %>% tibble("90%" = .)
  P97.5 = round(quantile(temp,probs=0.975),1) %>% tibble("97.5%" = .)
  P99 = round(quantile(temp,probs=0.99),1) %>% tibble("99%" = .)
  P100 = round(quantile(temp,probs=1),1) %>% tibble("100%" = .)
  
  lil_table2 <- tibble(i,
                       Minimum = summary_temp$Min,
                       P0,
                       P1,
                       P2.5,
                       P10,
                       P25,
                       Median = summary_temp$Median,
                       Mean = summary_temp$Mean,
                       P50,
                       P75,
                       P90,
                       P97.5,
                       P99,
                       P100,
                       Max = summary_temp$Max)
  
  big_table2 <- bind_rows(lil_table2, big_table2)
}
table2 <- big_table2 %>% t()

# Creating table 3 and MMT table
# data regarding RR of each temperature span analyzed and MMT 

MMT_city <- tibble()
big_table3 <- tibble()

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
  
  mod <- gam(Casos~cb+
               ns(tempo,22*8)+
               Dia,
             family= nb,
             data= temp_data)
  res.mod <- resid(mod, type = "deviance")
  
  ## ESTIMATING THE MINIMUM RISK TEMPERATURE
  # Ref: Tobías et al., 2017.
  
  MMT <- findmin(cb,
                 mod,
                 from= P1,
                 to= P99)
  
  lil_MMT_city <- tibble(city = i, MMT)
  MMT_city <- bind_rows(lil_MMT_city, MMT_city)
  
  
  ## Prediction model with minimum risk temperature as reference
  pred <- crosspred(basis=cb,
                    model=mod,
                    from=P0,
                    to=P100,
                    by=0.1,
                    cumul=TRUE,
                    cen=MMT)
  
  ## RR accumulated total for the entire period for percentiles 2.5/10/90/97.5
  percentile_2.5 <- round(cbind(pred$allRRfit,
                                pred$allRRlow,
                                pred$allRRhigh)[paste0(as.character(P2.5)),],2) %>% 
    tibble(percentile_2.5 = .)
  percentile_10 <- round(cbind(pred$allRRfit,
                               pred$allRRlow,
                               pred$allRRhigh)[as.character(P10),],2) %>%
    tibble(percentile_10 = .)
  percentile_90 <- round(cbind(pred$allRRfit,
                               pred$allRRlow,
                               pred$allRRhigh)[as.character(P90),],2) %>%
    tibble(percentile_90 = .)
  percentile_97.5 <- round(cbind(pred$allRRfit,
                                 pred$allRRlow,
                                 pred$allRRhigh)[as.character(P97.5),],2)%>%
    tibble(percentile_97.5 = .)
  
  
  lil_RR_n_CI <- bind_cols(variables = c("RR", "IC min", "IC max"),
                           percentile_2.5,
                           percentile_10,
                           percentile_90,
                           percentile_97.5,
                           Region = i)
  
  RR_n_CI <- lil_RR_n_CI %>% pivot_longer(cols = c(percentile_2.5, 
                                                   percentile_10,
                                                   percentile_90,
                                                   percentile_97.5)) %>% 
    pivot_wider(names_from = variables, values_from = value)
  big_table3 <- bind_rows(big_table3, RR_n_CI)
}

table3 <- big_table3
MMT_table <- MMT_city


# Creating table 4
# data regarding AR of each temperature span analyzed

percentiles <- list(c(P0,P2.5),
                    c(P2.5,P10),
                    c(P10,MMT),
                    c(MMT,P90),
                    c(P90,P97.5),
                    c(P97.5,P100))
conditions <- list("Frio extremo","Frio moderado","Frio leve",
                   "Calor leve","Calor moderado","Calor extremo")

big_table4 <- tibble()

for (a in dados$Microregiao %>% unique){
  print(a)
  temp_data <- dados %>% filter(Microregiao == a)
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
  
  mod <- gam(Casos~cb+
               ns(tempo,22*8)+
               Dia,
             family= nb,
             data= temp_data)
  
  ## ESTIMATING THE MINIMUM RISK TEMPERATURE
  # Ref: Tobías et al., 2017.
  
  MMT <- findmin(cb,
                 mod,
                 from= P1,
                 to= P99)
  
  for (b in 1:6){
    print(b)
    c = conditions[[b]]
    d = percentiles[[b]]
    
    AR <- round(attrdl(temp,
                       cb,
                       temp_data$Casos,
                       mod,
                       type="af",
                       cen=MMT,
                       range=c(d),
                       dir="forw")*100,1) %>% as_tibble()
    
    sim <- attrdl(temp,
                  cb,
                  temp_data$Casos,
                  mod,
                  type="af",
                  cen=MMT,
                  range=c(d),
                  sim=T,
                  nsim=1000,
                  dir="forw")
  
    
    CI <- round(quantile(sim,c(2.5,97.5)/100)*100,1) %>% 
      as_tibble()
    
    tib_var <- tibble(variables = c("AR","CI_inf","CI_sup"))
    tib_value <- bind_rows(AR, CI)
    
    lil_tible4 <- bind_cols(tib_var, tib_value, city = a, condition = c)
    
    big_table4 <- bind_rows(big_table4,
                            lil_tible4)
    
  }}

table4 <- big_table4 %>% 
  pivot_wider(names_from = "variables", values_from = "value")

# Creating table 5
# data regarding AR of each temperature span analyzed

percentiles <- list(c(P0,P2.5),
                    c(P2.5,P10),
                    c(P10,MMT),
                    c(MMT,P90),
                    c(P90,P97.5),
                    c(P97.5,P100))
conditions <- list("Frio extremo","Frio moderado","Frio leve",
                   "Calor leve","Calor moderado","Calor extremo")

big_table5 <- tibble()

for (a in dados$Microregiao %>% unique){
  print(a)
  temp_data <- dados %>% filter(Microregiao == a)
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
  
  mod <- gam(Casos~cb+
               ns(tempo,22*8)+
               Dia,
             family= nb,
             data= temp_data)
  res.mod <- resid(mod, type = "deviance")
  
  ## ESTIMATING THE MINIMUM RISK TEMPERATURE
  # Ref: Tobías et al., 2017.
  
  MMT <- findmin(cb,
                 mod,
                 from= P1,
                 to= P99)
  for (b in 1:6){
    print(b)
    c = conditions[[b]]
    d = percentiles[[b]]
    
    AN <- round(attrdl(temp,
                       cb,
                       temp_data$Casos,
                       mod,
                       type="an",
                       cen=MMT,
                       range=c(d),
                       dir="forw")) %>% as_tibble()
    
    sim <- attrdl(temp,
                  cb,
                  temp_data$Casos,
                  mod,
                  type="an",
                  cen=MMT,
                  range=c(d),
                  sim=T,
                  nsim=1000,
                  dir="forw")
    
    CI <- round(quantile(sim,c(2.5,97.5)/100)) %>% 
      as_tibble()
    
    tib_var <- tibble(variables = c("AN","CI_inf","CI_sup"))
    tib_value <- bind_rows(AN, CI)
    
    lil_tible5 <- bind_cols(tib_var, tib_value, city = a, condition = c)
    
    big_table5 <- bind_rows(big_table5,
                            lil_tible5)
    
  }}
table5 <- big_table5 %>% 
  pivot_wider(names_from = "variables", values_from = "value")

# Creating table 6
# data regarding AR of each temperature span based on city and different models for sensitivity analysis

# Changing cb
# models 1-7
sen_table_1_7 <- tibble()
for (i in dados$Microregiao %>% unique){
  
  print(i)
  temp_data <- dados %>% filter(Microregiao == i)
  temp_data$tempo <- 1:8036

  temp = temp_data$Temp_med
  
  cb1 <- crossbasis(temp,
                    lag=21,
                    argvar=list(fun="ns",df=5),
                    arglag=list(fun="poly",degree=4)) #1
  # modifying the number of df
  cb2 <- crossbasis(temp,lag=21,
                    argvar=list(fun="ns",df=3),
                    arglag=list(fun="poly",degree=4)) #2
  cb3 <- crossbasis(temp,lag=21,
                    argvar=list(fun="ns",df=7),
                    arglag=list(fun="poly",degree=4)) #3
  cb4 <- crossbasis(temp,lag=21,
                    argvar=list(fun="ns",df=5),
                    arglag=list(fun="poly",degree=2)) #4
  cb5 <- crossbasis(temp,lag=21,
                    argvar=list(fun="ns",df=5),
                    arglag=list(fun="poly",degree=6)) #5
  # modifying the number of lags
  cb6 <- crossbasis(temp,lag=14,
                    argvar=list(fun="ns",df=5),
                    arglag=list(fun="poly",degree=4)) #6
  cb7 <- crossbasis(temp,lag=28,
                    argvar=list(fun="ns",df=5),
                    arglag=list(fun="poly",degree=4)) #7
  ## original gam / dlnm model
  
  for (a in 1:7){
    print(a)
    
    ## Prediction model with minimum risk temperature as reference
    P0 = round(quantile(temp,probs=0),1)
    P1 = round(quantile(temp,probs=0.01),1)
    P2.5 = round(quantile(temp,probs=0.025),1)
    P10 = round(quantile(temp,probs=0.1),1)
    P90 = round(quantile(temp,probs=0.9),1)
    P97.5 = round(quantile(temp,probs=0.975),1)
    P99 = round(quantile(temp,probs=0.99),1)
    P100 = round(quantile(temp,probs=1),1)
    
    cb <- get(paste0("cb",a))
    mod <- gam(Casos~cb+ns(tempo,22*8)+Dia,family=nb,data=temp_data) #1
    
    MMT <- findmin(cb,
                   mod,
                   from=P1,
                   to=P99)
    pred <- crosspred(cb,
                      mod,
                      from=P0,
                      to=P100,
                      by=0.1,
                      cumul=TRUE, 
                      cen=MMT)
    
    ## RR accumulated total for the entire period for percentiles 2.5/10/90/97.5 
    percentile_2.5 <- round(cbind(pred$allRRfit,
                                  pred$allRRlow,
                                  pred$allRRhigh)[as.character(P2.5),],2) %>% 
      tibble(percentile_2.5 = .)
    percentile_10 <- round(cbind(pred$allRRfit,
                                 pred$allRRlow,
                                 pred$allRRhigh)[as.character(P10),],2) %>%
      tibble(percentile_10 = .)
    percentile_90 <- round(cbind(pred$allRRfit,
                                 pred$allRRlow,
                                 pred$allRRhigh)[as.character(P90),],2) %>%
      tibble(percentile_90 = .)
    percentile_97.5 <- round(cbind(pred$allRRfit,
                                   pred$allRRlow,
                                   pred$allRRhigh)[as.character(P97.5),],2)%>%
      tibble(percentile_97.5 = .)
    
    
    lil_RR_n_CI <- bind_cols(variables = c("RR", "IC min", "IC max"),
                             percentile_2.5,
                             percentile_10,
                             percentile_90,
                             percentile_97.5,
                             Region = i,
                             model = a)
    
    RR_n_CI <- lil_RR_n_CI %>% pivot_longer(cols = c(percentile_2.5, 
                                                     percentile_10,
                                                     percentile_90,
                                                     percentile_97.5)) %>% 
      pivot_wider(names_from = variables, values_from = value)
    sen_table_1_7 <- bind_rows(sen_table_1_7, RR_n_CI)
  }
}

# Changing mod
# Models 8-10
sen_table_8_10 <- tibble()
for (i in dados$Microregiao %>% unique){
  
  print(i)
  temp_data <- dados %>% filter(Microregiao == i)
  temp_data$tempo <- 1:8036
  temp = temp_data$Temp_med
  
  cb <- crossbasis(temp,
                   lag=21,
                   argvar=list(fun="ns",df=5),
                   arglag=list(fun="poly",degree=4))#1
  # modifying df of time spline
  mod8 <- gam(Casos~cb+ns(tempo,22*4)+Dia,family=nb,data=temp_data) #8
  mod9 <- gam(Casos~cb+ns(tempo,22*12)+Dia,family=nb,data=temp_data) #9
  # adding population to model
  mod10 <- gam(Casos~cb+ns(tempo,22*8)+Dia+offset(log(Pop)),family=nb,data=temp_data) #10
  
  for (a in c(8:10)){
    print(a)
    
    ## Prediction model with minimum risk temperature as reference
    P0 = round(quantile(temp,probs=0),1)
    P1 = round(quantile(temp,probs=0.01),1)
    P2.5 = round(quantile(temp,probs=0.025),1)
    P10 = round(quantile(temp,probs=0.1),1)
    P90 = round(quantile(temp,probs=0.9),1)
    P97.5 = round(quantile(temp,probs=0.975),1)
    P99 = round(quantile(temp,probs=0.99),1)
    P100 = round(quantile(temp,probs=1),1)
    
    mod <- get(paste0("mod",a))
    
    MMT <- findmin(cb,
                   mod,
                   from=P1,
                   to=P99)
    pred <- crosspred(cb,
                      mod,
                      from=P0,
                      to=P100,
                      by=0.1,
                      cumul=TRUE, 
                      cen=MMT)
    
    ## RR accumulated total for the entire period for percentiles 2.5/10/90/97.5 
    percentile_2.5 <- round(cbind(pred$allRRfit,
                                  pred$allRRlow,
                                  pred$allRRhigh)[as.character(P2.5),],2) %>% 
      tibble(percentile_2.5 = .)
    percentile_10 <- round(cbind(pred$allRRfit,
                                 pred$allRRlow,
                                 pred$allRRhigh)[as.character(P10),],2) %>%
      tibble(percentile_10 = .)
    percentile_90 <- round(cbind(pred$allRRfit,
                                 pred$allRRlow,
                                 pred$allRRhigh)[as.character(P90),],2) %>%
      tibble(percentile_90 = .)
    percentile_97.5 <- round(cbind(pred$allRRfit,
                                   pred$allRRlow,
                                   pred$allRRhigh)[as.character(P97.5),],2)%>%
      tibble(percentile_97.5 = .)
    
    
    lil_RR_n_CI <- bind_cols(variables = c("RR", "IC min", "IC max"),
                             percentile_2.5,
                             percentile_10,
                             percentile_90,
                             percentile_97.5,
                             Region = i,
                             model = a)
    
    RR_n_CI <- lil_RR_n_CI %>% pivot_longer(cols = c(percentile_2.5, 
                                                     percentile_10,
                                                     percentile_90,
                                                     percentile_97.5)) %>% 
      pivot_wider(names_from = variables, values_from = value)
    sen_table_8_10 <- bind_rows(sen_table_8_10, RR_n_CI)
  }
}


# Changing temp
# Models 11-12
sen_table_11_12 <- tibble()
for (i in dados$Microregiao %>% unique){
  print(i)
  temp_data <- dados %>% filter(Microregiao == i)
  temp_data$tempo <- 1:8036

  temp11 <-  temp_data$Temp_min #11
  temp12 <-  temp_data$Temp_max #12
  
  for (a in c(11:12)){
    print(a)
    temp = get(paste0("temp",a))
    
    cb <- crossbasis(temp,
                     lag=21,
                     argvar=list(fun="ns",df=5),
                     arglag=list(fun="poly",degree=4))#1
    ## original gam / dlnm model
    mod <- gam(Casos~cb+ns(tempo,22*8)+Dia,family=nb,data=temp_data)#1

    
    ## Prediction model with minimum risk temperature as reference
    P0 = round(quantile(temp,probs=0),1)
    P1 = round(quantile(temp,probs=0.01),1)
    P2.5 = round(quantile(temp,probs=0.025),1)
    P10 = round(quantile(temp,probs=0.1),1)
    P90 = round(quantile(temp,probs=0.9),1)
    P97.5 = round(quantile(temp,probs=0.975),1)
    P99 = round(quantile(temp,probs=0.99),1)
    P100 = round(quantile(temp,probs=1),1)
    
    MMT <- findmin(cb,
                   mod,
                   from=P1,
                   to=P99)
    pred <- crosspred(cb,
                      mod,
                      from=P0,
                      to=P100,
                      by=0.1,
                      cumul=TRUE, 
                      cen=MMT)
    
    ## RR accumulated total for the entire period for percentiles 2.5/10/90/97.5 
    percentile_2.5 <- round(cbind(pred$allRRfit,
                                  pred$allRRlow,
                                  pred$allRRhigh)[as.character(P2.5),],2) %>% 
      tibble(percentile_2.5 = .)
    percentile_10 <- round(cbind(pred$allRRfit,
                                 pred$allRRlow,
                                 pred$allRRhigh)[as.character(P10),],2) %>%
      tibble(percentile_10 = .)
    percentile_90 <- round(cbind(pred$allRRfit,
                                 pred$allRRlow,
                                 pred$allRRhigh)[as.character(P90),],2) %>%
      tibble(percentile_90 = .)
    percentile_97.5 <- round(cbind(pred$allRRfit,
                                   pred$allRRlow,
                                   pred$allRRhigh)[as.character(P97.5),],2)%>%
      tibble(percentile_97.5 = .)
    
    
    lil_RR_n_CI <- bind_cols(variables = c("RR", "IC min", "IC max"),
                             percentile_2.5,
                             percentile_10,
                             percentile_90,
                             percentile_97.5,
                             Region = i,
                             model = a)
    
    RR_n_CI <- lil_RR_n_CI %>% pivot_longer(cols = c(percentile_2.5, 
                                                     percentile_10,
                                                     percentile_90,
                                                     percentile_97.5)) %>% 
      pivot_wider(names_from = variables, values_from = value)
    sen_table_11_12 <- bind_rows(sen_table_11_12, RR_n_CI)
  }
}

table6 <- bind_rows(sen_table_1_7,sen_table_8_10,sen_table_11_12)

# saving all tables
write.xlsx(table1, "table1.xlsx")
write.xlsx(table2, "table2.xlsx")
write.xlsx(table3, "table3.xlsx")
write.xlsx(table4, "table4.xlsx")
write.xlsx(table5, "table5.xlsx")
write.xlsx(table6, "table6.xlsx")
write.xlsx(MMT_table, "MMT_table.xlsx")

