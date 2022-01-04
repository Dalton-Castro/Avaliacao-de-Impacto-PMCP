#Importando bibliotecas
library(Synth)
library(dplyr)

#Importando a base de dados:
data <- read.csv2('dados_controle_sintetico.csv', 
                  header = T, sep=',' , stringsAsFactors = FALSE,
                  encoding="UTF-8")

#excluindo a primeira coluna
data <- data[,c(-1)]

#codigo dos municípios na donor pool
codigos_donor_pool <- read.csv2('codigos_municipios_controle.csv', 
                               header = TRUE, sep=',' , 
                               stringsAsFactors = FALSE,
                               encoding="UTF-8")
codigos_donor_pool <- as.numeric(codigos_donor_pool$Códigos.dos.Municípios)
codigos_donor_pool <- codigos_donor_pool[-(which (codigos_donor_pool == 2605905))]
codigos_donor_pool <- codigos_donor_pool[-(which (codigos_donor_pool == 2612471))]


#Transformando todas as colunas em numeric como o pacote Synth exige

nomes <- data$nome_do_municipio
data <- data %>% mutate_if(is.character,as.numeric)
data <- data %>% mutate_if(is.integer,as.numeric)
data$nome_do_municipio <- nomes

#Preparando os dados

dataprep.out_proficiencia_media_matematica <- dataprep(
  foo = data,
  predictors = c("porcentagem_de_docentes_com_formacao_adequada_no_fundamental",
                 "porcentagem_de_alunos_do_ensino_fundamental_em_escolas_com_laboratorio_de_informatica",
                 "porcentagem_de_alunos_do_ensino_fundamental_em_escolas_com_acesso_a_internet"),
  predictors.op = "mean",
  time.predictors.prior = 2013:2017,
  special.predictors = list(
    list("proficiencia_media_matematica", seq(2008,2017), "mean")),
  dependent = "proficiencia_media_matematica",
  unit.variable = "codigo_do_municipio",
  unit.names.variable = "nome_do_municipio",
  time.variable = "ano",
  treatment.identifier = 1,
  controls.identifier = codigos_donor_pool,
  time.optimize.ssr = 2008:2017,
  time.plot = 2008:2018)


#Rodando o Modelo:

synth.out_proficiencia_media_matematica <- synth(data.prep.obj = dataprep.out_proficiencia_media_matematica, 
                                                 method = "All")

#Plotando Gráficos

path.plot(synth.res = synth.out_proficiencia_media_matematica, 
          dataprep.res = dataprep.out_proficiencia_media_matematica,
          Ylab = "Proficiência Média", Xlab = "ano",
          Ylim = c(150, 210), Legend = c("Média dos Municípios Tratados",
                                         "Município Médio Sintético"), Legend.position = "bottomright")

gaps.plot(synth.res = synth.out_proficiencia_media_matematica, 
          dataprep.res = dataprep.out_proficiencia_media_matematica,
          Ylab = "gap entre as proficiências médias", Xlab= "Ano",
          Ylim = c(-2,2), Main = NA)


#Refazendo o processo utilizando a variação das proficiências médias:


#Preparando os dados

dataprep.out_variacao_proficiencia_media_matematica <- dataprep(
  foo = data,
  predictors = c("porcentagem_de_docentes_com_formacao_adequada_no_fundamental",
                 "porcentagem_de_alunos_do_ensino_fundamental_em_escolas_com_laboratorio_de_informatica",
                 "porcentagem_de_alunos_do_ensino_fundamental_em_escolas_com_acesso_a_internet"),
  predictors.op = "mean",
  time.predictors.prior = 2013:2017,
  special.predictors = list(
    list("variacao_proficiencia_media_matematica", seq(2009,2017), "mean")),
  dependent = "variacao_proficiencia_media_matematica",
  unit.variable = "codigo_do_municipio",
  unit.names.variable = "nome_do_municipio",
  time.variable = "ano",
  treatment.identifier = 1,
  controls.identifier = codigos_donor_pool,
  time.optimize.ssr = 2009:2017,
  time.plot = 2009:2018)


#Rodando o Modelo:

synth.out_variacao_proficiencia_media_matematica <- synth(data.prep.obj = dataprep.out_variacao_proficiencia_media_matematica, 
                                                 method = "All")

#Plotando Gráficos

path.plot(synth.res = synth.out_variacao_proficiencia_media_matematica, 
          dataprep.res = dataprep.out_variacao_proficiencia_media_matematica,
          Ylab = "Proficiência Média", Xlab = "ano",
          Ylim = c(-25, 25), Legend = c("Variação Anual dos Municípios Tratados",
                                         "Variação Anual do Município Médio Sintético"), Legend.position = "bottomright")

gaps.plot(synth.res = synth.out_variacao_proficiencia_media_matematica, 
          dataprep.res = dataprep.out_variacao_proficiencia_media_matematica,
          Ylab = "gap entre as proficiências médias", Xlab= "Ano",
          Ylim = c(-5,5), Main = NA)
