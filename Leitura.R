library(moments)
library(corrgram)
library(pear)
require(zoo)
entrada = data.frame(read.csv(choose.files(), sep=";", dec=","))
qtd_ano = length(entrada[,1])/12
##############ENTRADA E INSTRUÇÕES REFERENTES AS INFORMAÇÕES BRUTAS################
#Entrada de dados bruta com informações mensais coletadas ao longo dos anos. 
#Este arquivo deve estar num formato específico (.csv) em que as colunas são separadas 
#por ";" e os decimais por ",". Neste arquivo devem haver apenas 2 colunas,
#a primeira referente ao mês, que deve possuir cabeçalho nomeado "MES" e a segunda
#referente a vazão mensal nomeada "VAZAO" correspondente ao mês da mesma linha.
#A coluna "MES" deve obedecer algumas regras para que os dados sejam lidos corretamente:
#-Os dados devem estar em ordem cronológica
#-Todos os anos devem estar completos em questão de meses
#-Os últimos 4 caracteres da informação devem se referir ao ano em questão
#Da coluna "VAZAO" só é necessário que os valores estejam separando decimais por ","


#Exemplo correto:
#
# MES;VAZAO
# jan/1968;450,7
# fev/1968;430,8
# mar/1968;403,5
# abr/1968;364,9
# mai/1968;329,4
# jun/1968;317,7
# jul/1968;340,0
# ago/1968;335,6
# set/1968;362,1
# out/1968;415,5
# nov/1968;423,7
# dez/1968;450,7





div_anos<-function(sH) #Esta função pega os dados brutos e retorna uma tabela ano x mes
{
  qtd_ano = length(sH[,1])/12 #Calcula, baseado no número de linhas a quantidade de anos registrados no arquivo
  serie_hist = matrix(sH$VAZAO, qtd_ano,byrow = TRUE)#Quebra o dataframe em 38 partes(anos) e cada parte é convertida numa linha da nova tabela
  
  anos = as.character(sH$MES)
  anos = substr(anos, nchar(anos)-4+1, nchar(anos))
  anos = unique(anos)
  anos = sort(anos)#Pega a coluna de datas e interpreta quais os anos foram analisados baseados nos ultimos 4 caracteres da informação da coluna "MES"
  
  row.names(serie_hist)= anos
  colnames(serie_hist)=c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")
  serie_hist=as.data.frame(serie_hist)#Nomeia linhas e colunas e converte a matriz em um dataframe
  
  boxplot(serie_hist)
  
  plot(anos,apply(serie_hist,1,sum),type="b")

  return(serie_hist)
}

tabela_refinada = div_anos(entrada) #Função "quebra" o vetor de entrada de todos os meses em um dataframe cujas colunas se referem aos meses e as linhas aos anos

relatorio_estatistico<-function(tabela_anual)
{
  Medias = apply(tabela_anual,2,mean)
  Desvio_Padrao = apply(tabela_anual,2,sd)
  Assimetria = skewness(tabela_anual)
  Indice_Kurt = kurtosis(tabela_anual)
  
  Relatorio = data.frame(Medias,Desvio_Padrao,Assimetria,Indice_Kurt) #Relatório com propriedades importantes como Média, Desvio Padrão, Assimetria e Indice Kurotsis 
  return(Relatorio)  
}

Relatorio = relatorio_estatistico(tabela_refinada)

Time_serie= ts(entrada$VAZAO,start=c(1,1) ,end=c(qtd_ano,12),deltat = 1/12,class="ts") 
CorrelacaoSazonal= peacf(Time_serie,5)
CorrelacaoSazonal
CorrelacaoSazonalParcial= pepacf(Time_serie,5)
CorrelacaoSazonalParcial

ACF_N = acf(Time_serie)
ACF_N
ACF_Parcial=pacf(Time_serie)
ACF_Parcial
 
plot(entrada, type="b")
