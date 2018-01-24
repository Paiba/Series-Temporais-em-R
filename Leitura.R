##############ENTRADA E INSTRUÇÕES REFERENTES AS INFORMAÇÕES BRUTAS################

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



library(moments)
library(corrgram)
library(pear)

#LEITURA DE ARQUIVOS DE ENTRADA E VARIÁVEIS ÚTEIS#

entrada = data.frame(read.csv(choose.files(), sep=";", dec=",")) #Leitura de dados históricos mensais
serie_sintetica = data.frame(read.csv(choose.files(), header = F,sep=";", dec=",")) #Leitura de série sintética gerada para essa mesma bacia
qtd_ano = length(entrada[,1])/12 #Quantidade de anos nos dados históricos baseado no arquivo de entrada


##################FUNÇÕES####################

#Esta função pega os dados brutos e retorna uma tabela ano x mes, além disso plota 2 gráficos necessários na análise
div_anos<-function(sH)
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

#Pega os dados de entrada de vazão mensal e gera um relatório de estatísticas com as médias mensais, desvio padrão, assimetria e coeficiente de kurtosis
relatorio_estatistico<-function(tabela_anual)
{
  Medias = apply(tabela_anual,2,mean)
  Desvio_Padrao = apply(tabela_anual,2,sd)
  Assimetria = skewness(tabela_anual)
  Indice_Kurt = kurtosis(tabela_anual)
  
  Relatorio = data.frame(Medias,Desvio_Padrao,Assimetria,Indice_Kurt) 
  return(Relatorio)  
}

#Desagrega os dados sintéticos utilizando estatísticas e dados históricos
soma_harm<-function(a) #Função auxiliar para a função 'desagrega' dado um inteiro 'a' faz a soma 'div' = [1 + (1/2) + (1/3) + ... + (1/a)]
{
  div=0
  for(i in 1:a)
  {
    div=div+(1/i)
  }
  return(div)
}

desagrega<-function(serieSint,serieDadosHist)
{
  
  Anuais = data.frame(V1=apply(serieDadosHist,1,sum))#Dados as vazões mensais, calcula as vazões anuais
  
  delta_i=abs(rep(serieSint$V1[1],length(Anuais))-Anuais)#Faz um vetor da diferença(delta_i) do primeiro dado sintético referente a vazão anual com todos os anos históricos( |X1-xi| )
  
  Tabela = cbind(Anuais,delta_i$V1)#Faz uma tabela que relaciona ano, vazão anual histórica e diferença(delta_i)
  Tabela = Tabela[order(Tabela$delta_i),]#Ordena de forma crescente de delta_i
  
  K = floor(sqrt(length(delta_i$V1)))
  
  div=soma_harm(K)
  
  cwm=rep(0,K)
  for(i in 1:K)
  {
    if(i==1){
      cwm[i]=(1/div)
    }
    else
      cwm[i]=cwm[i-1]+(1/i)/div
  }
  
  a=sample(cwm,1)
  b=match(a,cwm)
  
  print(cwm)
  print(a)
  print(b)
  return(Tabela)
}











#######################APLICAÇÕES###########################

#Aplicação da primeira função(div_anos)
tabela_refinada = div_anos(entrada) #Função "quebra" o vetor de entrada de todos os meses em um dataframe cujas colunas se referem aos meses e as linhas aos anos

#Aplicação da segunda função(relatorio_estatistico)
Relatorio = relatorio_estatistico(tabela_refinada)

#Aplicação da terceira função(desagrega)

Deltai=desagrega(serie_sintetica,tabela_refinada)

#Aplicações diversas que gram gráficos, posteriormente provavelmente serão colocadas em outras funções
###########################################
#Time_serie= ts(entrada$VAZAO,start=c(1,1) ,end=c(qtd_ano,12),deltat = 1/12,class="ts") 

#CorrelacaoSazonal= peacf(Time_serie,5)
#CorrelacaoSazonal
#CorrelacaoSazonalParcial= pepacf(Time_serie,5)
#CorrelacaoSazonalParcial

#ACF_N = acf(Time_serie)
#ACF_N
#ACF_Parcial=pacf(Time_serie)
#ACF_Parcial
 
#plot(entrada, type="b")
###################################################