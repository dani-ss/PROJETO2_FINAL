## Projeto 2 - Laboratório de Estatística
## Cliente: Monica Vieira Costa

rm(list=ls())

# Pacotes ========

if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}
if(!require(diplyr)){install.packages("diplyr");library(diplyr)}
if(!require(haven)){install.packages("haven");require(haven)}
if(!require(jmv)){install.packages("jmv");require(jmv)}
if(!require(lubridate)){install.packages("lubridate");require(lubridate)}
if(!require(gmodels)){install.packages("gmodels");require(gmodels)}

# Funções =======

tab_freq <- function(x) {
  # Tabela com frequências absoluta e relativa (em %)
  tab <- cbind( table(x), prop.table(table(x))*100 )
  colnames(tab) <- c("N", "%")
  return(tab)
}

basic = function(x,more=F) {
  stats = list()
  
  clean.x = x[!is.na(x)]
  
  #stats$N = length(x)
  #stats$NAs = stats$N-length(clean.x)
  stats$N_validos = round(length(clean.x),3)
  stats$Média = round(mean(clean.x),3)
  stats$D.P = round(sd(clean.x),3)
  stats$Mín. = round(min(clean.x),3)
  stats$Q1 = round(fivenum(clean.x)[2],3)
  stats$Q2= round(fivenum(clean.x)[3],3)
  stats$Q3 = round(fivenum(clean.x)[4],3)
  stats$Máx. = round(max(clean.x),3)
  t1= unlist(stats)
  names(t1)= c("N válidos", "Média", "D.P.","Mín.", "1ºQ", "2ºQ", "3ºQ","Máx.")
  t1
}

grafico_dis<- function(bd, var){
  a <- freq(bd[[var]])
  a$cat <- row.names(a)
  b <- a %>% ggplot(aes(x = cat, y = n, label = paste(n))) + 
    geom_col(fill = 'darkblue') + 
    geom_text(vjust = -0.5) +
    labs(x = '',y = '', title = var)
  return(b)
}

# Lendo o banco de dados =======

diagnostico <- readxl::read_xlsx("Projeto2/dados_projeto2.xlsx",sheet = 1)
baseline <- readxl::read_xlsx("Projeto2/dados_projeto2.xlsx",sheet = 2)

# Manipulação do banco de dados ====

diagnostico$cod_paciente = factor(diagnostico$cod_paciente)
diagnostico$depressao = factor(diagnostico$depressao)

baseline$cod_paciente = factor(baseline$cod_paciente)
baseline$n_diagostico = factor(baseline$n_diagostico)
baseline$depressao = factor(baseline$depressao)

baseline <- baseline %>%  mutate(
  Diagnostico_2 = case_when(
    DiagnosticoNumerico == "1" ~ "Cntl",
    DiagnosticoNumerico == "2" ~ "CCLA",
    DiagnosticoNumerico == "3" ~ "CCLAMD",
    DiagnosticoNumerico == "4" ~ "CCLNA",
    DiagnosticoNumerico == "5" ~ "CCLSE",
    DiagnosticoNumerico == "6" ~ "DA",
    DiagnosticoNumerico == "7" ~ "DNA"),
  n_diagostico2 = case_when(
    n_diagostico == "1" ~ "Controle",
    n_diagostico == "2" ~ "CCL Amnéstico",
    n_diagostico == "3" ~ "CCL Amnéstico de Múltiplos Domínios",
    n_diagostico == "4" ~ "CCL Não-amnéstico",
    n_diagostico == "5" ~ "CCL sem especificação",
    n_diagostico == "6" ~ "Demência por doença de Alzheimer",
    n_diagostico == "7" ~ "Demência não Alzheimer"),
  sexo = case_when(
    sexo == "1" ~ "Homem",
    sexo == "2" ~ "Mulher"),
  diabetes = case_when(
    diabetes == "0" ~ "Não",
    diabetes == "1" ~ "Sim"),
  hipertensao = case_when(
    hipertensao == "0" ~ "Não",
    hipertensao == "1" ~ "Sim"),
  depressao = case_when(
    depressao == "0" ~ "Não",
    depressao == "1" ~ "Sim"),
  profissao = case_when(
    profissao == "0" ~ "Não",
    profissao == "1" ~ "Sim"))

discretas <- dados %>% select(diagnostico,DiagnosticoNumerico,DiagnosticoNumerico_2,ano.atendimento,Diabetes,Hipertensao,Depressao,`VOC-Total`,RAVEN.Total,Pfeffer.Pac.Total,Pfeffer.Cuid.Total)
continuas <- dados %>% select(diagnostico,DiagnosticoNumerico,Diabetes,Hipertensao,Depressao,`Mattis-Atencao`,MattisConstrucao_bruto,`Mattis–Conceituacao_bruto`,`5Dig.1B.Tempo`,escore_inibitorio,FV.Animais.T,FVF.S.Total.A,Ravlt.A6,Ravlt.A7,MMSE.Total,Mattis_total_bruto,FAB,Digitos.Direto.Acertos,Digitos.Inverso.Acertos,`TN-LIN.Total`)

# Análise descritiva ====

  # Análise descritiva do banco de dados ----
summary(diagnostico)
str(diagnostico)
glimpse(diagnostico)

summary(baseline)
str(baseline)
glimpse(baseline)

  # Frequencias ----
descriptives(baseline, vars = vars(idade, escolaridade), freq = TRUE)

rbind(tab_freq(baseline$n_diagostico2),
      tab_freq(baseline$sexo),
      tab_freq(baseline$diabetes),
      tab_freq(baseline$hipertensao),
      tab_freq(baseline$depressao),
      tab_freq(baseline$profissao))

  # Análise descritiva por diagnóstico ----
CrossTable(baseline$n_diagostico2, baseline$sexo)
CrossTable(baseline$n_diagostico2, baseline$diabetes)
CrossTable(baseline$n_diagostico2, baseline$hipertensao)
CrossTable(baseline$n_diagostico2, baseline$depressao)
CrossTable(baseline$n_diagostico2, baseline$profissao)

summary(baseline$idade)
summary(baseline$escolaridade)

  # variaveis discretas ----
vars <- colnames(discretas)

grafico_dis(bd = discretas, var = vars[1])
grafico_dis(bd = discretas, var = vars[2])
grafico_dis(bd = discretas, var = vars[3])
grafico_dis(bd = discretas, var = vars[4])
grafico_dis(bd = discretas, var = vars[5])
grafico_dis(bd = discretas, var = vars[6])
grafico_dis(bd = discretas, var = vars[7])
grafico_dis(bd = discretas, var = vars[8])
grafico_dis(bd = discretas, var = vars[9])
grafico_dis(bd = discretas, var = vars[10])

summary(discretas)


