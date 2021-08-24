require(tidyverse)
require(rio)
require(nnet)

rm(list = ls())

dados <- import('base.xlsx')

dados <- dados %>%  mutate(
  Diagnostico_2 = case_when(
    DiagnosticoNumerico == "1" ~ "Cntl",
    DiagnosticoNumerico == "2" ~ "CCLA",
    DiagnosticoNumerico == "3" ~ "CCLAMD",
    DiagnosticoNumerico == "4" ~ "CCLNA",
    DiagnosticoNumerico == "5" ~ "CCLSE",
    DiagnosticoNumerico == "6" ~ "DA",
    DiagnosticoNumerico == "7" ~ "DNA"),
  Sexo_2 = case_when(
    Sexo == "1" ~ "Homem",
    Sexo == "2" ~ "Mulher"),
  Diabetes_2 = case_when(
    Diabetes == "0" ~ "Não",
    Diabetes == "1" ~ "Sim"),
  Hipertensao_2 = case_when(
    Hipertensao == "0" ~ "Não",
    Hipertensao == "1" ~ "Sim"),
  Depressao_2 = case_when(
    Depressao == "0" ~ "Não",
    Depressao == "1" ~ "Sim"),
  profissao_2 = case_when(
    profissao == "0" ~ "Não",
    profissao == "1" ~ "Sim"))

dados$Escolaridade_2 <- ifelse(dados$Escolaridade == 0,'0 anos de estudo',
                               ifelse(dados$Escolaridade > 0 & dados$Escolaridade < 5, '1 a 4 anos',
                                      ifelse(dados$Escolaridade > 4 & dados$Escolaridade < 9,'5 a 8 anos',
                                             ifelse(dados$Escolaridade > 8 & dados$Escolaridade < 12, '9 a 11 anos','12 ou mais anos'))))

levels(dados$Diagnostico_2) <- c("Cntl","CCLA","CCLAMD","CCLNA","CCLSE","DA","DNA")
levels(dados$Escolaridade_2) <- c('0 anos de estudo','1 a 4 anos','5 a 8 anos','9 a 11 anos','12 ou mais anos')




dados_2 <- dados %>% filter(!is.na(Idade)) %>% filter(!is.na(Sexo_2)) %>% filter(!is.na(Escolaridade_2)) %>% filter(!is.na(profissao_2)) %>% 
  filter(!is.na(Diabetes_2)) %>% filter(!is.na(Hipertensao_2)) %>% filter(!is.na(Depressao_2)) #389 observaçoes

dados_2$Diagnostico_2 <- relevel(as.factor(dados_2$Diagnostico_2), ref = 'Cntl')

nrow(dados_2)

mod_r <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2, data = dados_2)

mod_1 <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2 + Diabetes_2 + Hipertensao_2 + Depressao_2, data = dados_2) #7covariaveis
mod_2 <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2 + Diabetes_2 , data = dados_2) #5covariaveis
mod_3 <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2 + Hipertensao_2 , data = dados_2) #5covariaveis
mod_4 <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2 + Depressao_2, data = dados_2) #5covariaveis
mod_5 <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2 + Diabetes_2 + Hipertensao_2, data = dados_2) #6covariaveis
mod_6 <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2 + Diabetes_2 + Depressao_2, data = dados_2) #6covariaveis
mod_7 <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2 + Hipertensao_2 + Depressao_2, data = dados_2) #6covariaveis
mod_8 <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2 + Diabetes_2 * Depressao_2, data = dados_2) 

# ANALISE DE DEVIANCE

## Mod_r
qchisq(0.95,385) < mod_r$deviance & mod_r$deviance < qchisq(0.99,385)

## Mod1
qchisq(0.95,382) < mod_1$deviance & mod_1$deviance < qchisq(0.99,382)

## Mod2
qchisq(0.95,384) < mod_2$deviance & mod_2$deviance < qchisq(0.99,384)

## Mod3
qchisq(0.95,384) < mod_3$deviance & mod_3$deviance < qchisq(0.99,384)

## Mod4
qchisq(0.95,384) < mod_4$deviance & mod_4$deviance < qchisq(0.99,384)

## Mod5
qchisq(0.95,383) < mod_5$deviance & mod_5$deviance < qchisq(0.99,383)

## Mod6
qchisq(0.95,383) < mod_6$deviance & mod_6$deviance < qchisq(0.99,383)

## Mod7
qchisq(0.95,383) < mod_7$deviance & mod_7$deviance < qchisq(0.99,383)

## Nenhum dos modelos tem deviance moderada

##Estatistica de pearson generalizada

##Mod_r
x2p_r <- sum(residuals(mod_r,type = 'pearson')^2)
pv_r <- 1 - pchisq(x2p_r,385)

##Mod_1
x2p_1 <- sum(residuals(mod_1,type = 'pearson')^2)
pv_1 <- 1 - pchisq(x2p_1,382)

##Mod_2
x2p_2 <- sum(residuals(mod_2,type = 'pearson')^2)
pv_2 <- 1 - pchisq(x2p_2,384)

##Mod_3
x2p_3 <- sum(residuals(mod_3,type = 'pearson')^2)
pv_3 <- 1 - pchisq(x2p_3,384)

##Mod_4
x2p_4 <- sum(residuals(mod_4,type = 'pearson')^2)
pv_4 <- 1 - pchisq(x2p_4,384)

##Mod_5
x2p_5 <- sum(residuals(mod_5,type = 'pearson')^2)
pv_5 <- 1 - pchisq(x2p_5,383)

##Mod_6
x2p_6 <- sum(residuals(mod_6,type = 'pearson')^2)
pv_6 <- 1 - pchisq(x2p_6,383)

##Mod_7
x2p_7 <- sum(residuals(mod_7,type = 'pearson')^2)
pv_7 <- 1 - pchisq(x2p_7,383)

epg <- data.frame(Mod_r = c(x2p_r,pv_r),
                  Mod_1 = c(x2p_1,pv_1),
                  Mod_2 = c(x2p_2,pv_2),
                  Mod_3 = c(x2p_3,pv_3),
                  Mod_4 = c(x2p_4,pv_4),
                  Mod_5 = c(x2p_5,pv_5),
                  Mod_6 = c(x2p_6,pv_6),
                  Mod_7 = c(x2p_7,pv_7))

View(epg)

## O menor valor da estatistica de pearson generalizada foi para o modelo com todas as covariaveis

## Escore z e p-valor para os coeficientes estimados

z <- summary(mod_6)$coefficients/summary(mod_6)$standard.errors

p <- (1 - pnorm(abs(z), 0, 1)) * 2


## Analise de Residuos 

rP <- mod_1$residuals
rCD <- residuals(mod_1, type = 'deviance')

plot(x=mod_1$fitted.values,y = rP,type = 'p',xlab = 'Valores ajustado', ylab = 'Residuo de Pearson')
abline(h=0, col = 'red')

plot(x=mod_1$fitted.values,y = rCD,type = 'p',xlab = 'Valores ajustado', ylab = 'Residuo Componente do desvio')
abline(h=0, col = 'red')

## Teste de qualidade do ajuste

chisq.test(dados_2$Diagnostico_2, predict(mod_1))

require(lmtest)

lrtest(mod_1, "Diabetes_2")

lrtest(mod_1, "Hipertensao_2")

lrtest(mod_1, "Depressao_2")


dados_3 <- import('base_descritivas.xlsx')

dados_3 <- dados_3 %>%  mutate(
  Diagnostico_2 = case_when(
    DiagnosticoNumerico == "1" ~ "Cntl",
    DiagnosticoNumerico == "2" ~ "CCLA",
    DiagnosticoNumerico == "3" ~ "CCLAMD",
    DiagnosticoNumerico == "4" ~ "CCLNA",
    DiagnosticoNumerico == "5" ~ "CCLSE",
    DiagnosticoNumerico == "6" ~ "DA",
    DiagnosticoNumerico == "7" ~ "DNA"),
  Sexo_2 = case_when(
    Sexo == "1" ~ "Homem",
    Sexo == "2" ~ "Mulher"),
  Diabetes_2 = case_when(
    Diabetes == "0" ~ "Não",
    Diabetes == "1" ~ "Sim"),
  Hipertensao_2 = case_when(
    Hipertensao == "0" ~ "Não",
    Hipertensao == "1" ~ "Sim"),
  Depressao_2 = case_when(
    Depressao == "0" ~ "Não",
    Depressao == "1" ~ "Sim"),
  profissao_2 = case_when(
    profissao == "0" ~ "Não",
    profissao == "1" ~ "Sim"))

dados_3$Escolaridade_2 <- ifelse(dados_3$Escolaridade == 0,'0 anos de estudo',
                               ifelse(dados_3$Escolaridade > 0 & dados_3$Escolaridade < 5, '1 a 4 anos',
                                      ifelse(dados_3$Escolaridade > 4 & dados_3$Escolaridade < 9,'5 a 8 anos',
                                             ifelse(dados_3$Escolaridade > 8 & dados_3$Escolaridade < 12, '9 a 11 anos','12 ou mais anos'))))

dados_3 <- dados_3 %>% filter(!is.na(Idade)) %>% filter(!is.na(Sexo_2)) %>% filter(!is.na(Escolaridade_2)) %>% filter(!is.na(profissao_2)) %>% 
  filter(!is.na(Diabetes_2)) %>% filter(!is.na(Hipertensao_2)) %>% filter(!is.na(Depressao_2)) #389 observaçoes

dados_3$Diagnostico_2 <- relevel(as.factor(dados_3$Diagnostico_2), ref = 'Cntl')

mod_r_3 <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2, data = dados_3)

mod_1_3 <- multinom(Diagnostico_2 ~ Idade + Sexo_2 + Escolaridade_2 + profissao_2 + `Mattis-Atencao`, data = dados_3)


















