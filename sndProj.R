# Flavio Braga e Silva Junior (Fbsj)
# Lucas Asafe Virginio do Nascimento (Lavn)


#setwd("~/Documents/projetoEstatistica")
library(dplyr)
#library(tidyverse)


planilha <- read.csv(file = 'Detalhes.csv', header = TRUE)

#------------------------------------------------------------------------------------------------------------------

#Funcao para obter modas(questoes 2 e 3)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#------------------------------------------------------------------------------------------------------------------

#Funcao pra pegar o max e min album pelo ano dado
getMaxMinYear <- function(year){
  DF2 <- planilha[planilha$Ano == year,]
  maximo <- max(DF2$Qnt..de.Albuns.Vendidos)
  minimo <- min(DF2$Qnt..de.Albuns.Vendidos)
  nameMax <- subset(DF2, DF2$Qnt..de.Albuns.Vendidos == maximo)
  nameMin <- subset(DF2, DF2$Qnt..de.Albuns.Vendidos == minimo)
  
  #DFMax <- DF2[DF2$Qnt..de.Albuns.Vendidos == maximo,]
  #DFMin <- DF2[DF2$Qnt..de.Albuns.Vendidos == minimo,]
  
  cat(paste("O album mais vendido do ano escolhido eh: ", nameMax$Album, "Do artista", nameMax$Artista, "\n"))
  cat(paste("O album menos vendido do ano escolhido eh: ", nameMin$Album, "Do artista", nameMin$Artista, "\n"))
}

#------------------------------------------------------------------------------------------------------------------

#Funcao para pegar uniques na tabela sexta questao
getUniques <- function(){
  df5 <- planilha[! planilha$Artista %in% unique(planilha[duplicated(planilha$Artista), "Artista"]), ]
  df6 <- subset(df5, select = c(1, 3))
  
  print(df6)
}

#------------------------------------------------------------------------------------------------------------------

getArtist <- function(){
  df123 <- planilha[planilha$Artista %in% planilha$Artista[duplicated(planilha$Artista)],]
  DF2 <- df123 %>% group_by(Artista) %>%
    summarise(scoring_sd = sd(Qnt..de.Albuns.Vendidos))
  
 menorDesvio <- min(DF2$scoring_sd)
 DF2 <- DF2[DF2$scoring_sd == menorDesvio,]
 
  cat(paste("O artista com menor desvio é: ", DF2$Artista))
}

#------------------------------------------------------------------------------------------------------------------
getApareceMais <- function(){
  indx <- tail(names(sort(table(planilha$Artista))),3)
  vector = c()
  for (note in indx){
    df10 <- planilha[planilha$Artista == note,]
    valoresSomados <- transform(df10, Total= ave(Qnt..de.Albuns.Vendidos, Artista, FUN=sum))[-2]
    vector <- c(vector, valoresSomados$Total[1])
  }
  
  newDf <- data.frame(indx, vector)
  newDf[order(newDf$vector, decreasing = TRUE),]  
}
getApareceMais()

#------------------------------------------------------------------------------------------------------------------

getRepeated <- function(){
  df3 <- planilha[planilha$Artista %in% planilha$Artista[duplicated(planilha$Artista)],]
  valoresSomados <- transform(df3, Total= ave(Qnt..de.Albuns.Vendidos, Artista, FUN=sum))[-2]
  print(valoresSomados)
}
getRepeated()

#------------------------------------------------------------------------------------------------------------------

createHist <- function(empresa){
  df10 <- planilha[planilha$Empresa == empresa,]
  df2018 <- df10[df10$Ano == 2018,]
  df2019 <- df10[df10$Ano == 2019,]

  
  #year <- c(2018, 2019)
  #albunslancados <- c()
  
  #histDf <- data.frame(year, albunslancados)
  print(df10$Ano)
}



#Questão 1 - printar o dataframe exatamente como ele esta
print(planilha, quote = TRUE, row.names(FALSE))

#Questão 2 - Encontre a média, o desvio padrão e a moda das vendas do total de álbuns 
cat(paste("\nA media das vendas do total de álbuns eh : ", mean(planilha$Qnt..de.Albuns.Vendidos), "\nO desvio padrao das vendas do total de álbuns eh: ", sd(planilha$Qnt..de.Albuns.Vendidos), "\nA moda eh: ", getmode(planilha$Qnt..de.Albuns.Vendidos)))

#Questão 3 - Faça uma função que retorna os nomes dos artistas que lançaram álbuns nos dois anos


#Questão 4 - Faça uma função que retorne qual artista possui o menor desvio padrão nas vendas 
getArtist()


#Questão 5 - Faça uma função que retorne o nome do álbum que mais vendeu e o que menos vendeu ao dar um ano de lançamento
getMaxMinYear(2018)

#Questão 6 - Faça uma função que retorne uma lista com os artistas que só apareceram uma vez na planilha, indicando também o ano que cada um aparece.
getUniques()

#Questão 10 - Faça uma função que ao receber o nome de uma empresa, cria um histograma onde mostra a frequência de álbuns lançados pela empresa de acordo com o ano. 
createHist('Stone Music')