#Tarefa prática 1 - pré-processamento de dados
#Grupo 2 - Juliana, Mateus e Rodrigo
#Definindo o diretório e carregando o DataSet

setwd("C:/Users/User/Downloads")
df <- read.csv("C:/Users/User/Downloads/ds_salaries.csv")

#Pacotes carregados
library(readr)
library(psych)
library(data.table)

#Visualização inicial dos dados

head(df)
tail(df)

#Exercicio 1 -  Gerarando estatísticas descritiva

summary(df)

describeBy(df)

# Exercicio 2 - Técnica de Amostragem
# Bootstrap
bootstrap=transpose(sample(transpose(df), size = 600, replace = TRUE))
View(bootstrap)

#Exercicio 3 - Técnica de discretização 
#Categorizando a coluna de salários em USD em faixas salariais: muito alto, medio, baixo e muito baixo
#Nova coluna criada no df de faixa salarial

df$faixa_salarial<-rep(NA,length(df$salary_in_usd))
summary(df$salary_in_usd)
sd(df$salary_in_usd,na.rm=T)
df$faixa_salarial[df$salary_in_usd<summary(df$salary_in_usd)[2]]<-'Salario muito baixo'
df$faixa_salarial[summary(df$salary_in_usd)[2]<=df$salary_in_usd&df$salary_in_usd<summary(df$salary_in_usd)[3]]<-'Salario baixo'
df$faixa_salarial[summary(df$salary_in_usd)[3]<=df$salary_in_usd&df$salary_in_usd<summary(df$salary_in_usd)[5]]<-'Salario medio'
df$faixa_salarial[df$salary_in_usd>=summary(df$salary_in_usd)[5]]<-'Salario alto'

# Exercicio 4 - Técnica de Feature Engineering
# Criando de uma nova variável para indicar se trabalha no mesmo país em que mora (presencial), ou se trabalha remotamente
df$trabalho_residencia <- ifelse(
  df$employee_residence != df$company_location, 
  "Trabalha Remoto", "Trabalha Presencial")
