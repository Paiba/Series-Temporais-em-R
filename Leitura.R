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
require(tcltk)
#LEITURA DE ARQUIVOS DE ENTRADA E VARIÁVEIS ÚTEIS#

entrada = data.frame(read.csv(tk_choose.files(), sep=";", dec=",")) #Leitura de dados históricos mensais
serie_sintetica = data.frame(read.csv(tk_choose.files(), header = F,sep=";", dec=",")) #Leitura de série sintética gerada para essa mesma bacia
qtd_ano_hist = length(entrada[,1])/12 #Quantidade de anos nos dados históricos baseado no arquivo de entrada
qtd_ano_des = length(serie_sintetica[,1])



##################FUNÇÕES####################

#Esta função pega os dados brutos e retorna uma tabela ano x mes, além disso plota 2 gráficos necessários na análise
div_anos<-function(sH)
{
  qtd_ano_hist = length(sH[,1])/12 #Calcula, baseado no número de linhas a quantidade de anos registrados no arquivo
  serie_hist = matrix(sH$VAZAO, qtd_ano_hist,byrow = TRUE)#Quebra o dataframe em 38 partes(anos) e cada parte é convertida numa linha da nova tabela
  
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

#############Desagrega de forma não paramétrica os dados sintéticos utilizando estatísticas e dados históricos

desagrega_np<-function(serieSint,serieDadosHist)
{
  desagregado_final = data.frame()#Cria um data frame vazio que será nossa série anual 
  
  ##################PRIMEIRA ITERAÇÃO DE DESAGREGAÇÃO##########################
  
  Anuais = data.frame(V1=apply(serieDadosHist,1,sum))#Dados as vazões mensais, calcula as vazões anuais
  delta_i = abs(rep(serieSint$V1[1],length(Anuais))-Anuais)#Faz um vetor da diferença(delta_i) do primeiro dado sintético referente a vazão anual com todos os anos históricos( |X1-xi| )
  
  x=delta_i$V1
  Tabela = cbind(Anuais,delta_i$V1)#Faz uma tabela que relaciona ano, vazão anual histórica e diferença(delta_i)
  Tabela = Tabela[order(Tabela$delta_i),]#Ordena de forma crescente de delta_i
  
  ############CÁLCULO DE K E DO CWM####################
  K = floor(sqrt(length(delta_i$V1)))
  
  div=sum(1/1:K)
  
  cwm=rep(0,K)
  for(i in 1:K)
  {
    if(i==1){
      cwm[i]=(1/div)
    }
    else
      cwm[i]=cwm[i-1]+(1/i)/div
  }
  ################
  
  
  random = runif(1) #Escolhe número aleatório no vetor de pesos cumulativos(cwm) e armazena na variavel 'random'
  posicao = which.min(abs(cwm - random)) #Armazena na variavel 'posicao' a posição do número escolhido no vetor cwm
  candidato = rownames(Tabela)[posicao] #Armazena o ano candidato a desagrgação na variavel 'candidato'
 
  
  desagregado = serieDadosHist[candidato,]*(serieSint$V1[1]/(apply(serieDadosHist[candidato,],1,sum)))
  desagregado = c(desagregado[1,],TOTAL=sum(desagregado[1,]))
  
  desagregado_final = rbind(desagregado_final,desagregado)

  ############FIM DA DESAGREGAÇÃO DO ANO 1##############
  Tabela = Tabela[order(row.names(Tabela)),]
  ############DESAGREGAÇÃO DOS OUTROS ANOS###########################
  for (j in 2:qtd_ano_des)
  {
    ########### CÁLCULO DO DELTA_i###########
    
    fi_1 = 1/var(Anuais[2:qtd_ano_hist,1])
    fi_2 = 1/var(serieDadosHist$DEZ[2:qtd_ano_hist])
    
    for(i in 2:qtd_ano_hist)
    {
      delta_i[i,1] = sqrt(fi_1*(serieSint$V1[j]-Anuais[i,1])^2 + fi_2*(desagregado_final$DEZ[j-1]-serieDadosHist$DEZ[i-1])^2)
    }
   
    Tabela[,2] = delta_i

    Tabela = Tabela[order(Tabela$delta_i),]
    
    random = runif(1)
    posicao = which.min(abs(cwm - random))
    candidato = rownames(Tabela)[posicao]
    
    desagregado = serieDadosHist[candidato,]*(serieSint$V1[j]/(apply(serieDadosHist[candidato,],1,sum)))
    desagregado = c(desagregado[1,],TOTAL=sum(desagregado[1,]))
    
    desagregado_final = rbind(desagregado_final,desagregado)
    
    
  }
  
  return(desagregado_final)
}

############Desagrega multiplas vezes (k vezes)################

desagrega_mult<-function(serieSint,serieDadosHist,k){
  lista<-list()
  if(k>0){
    for (i in 1:k)
      {
        lista[[i]]=desagrega_np(serieSint,serieDadosHist)
      }
  }
  return(lista)
}










#######################APLICAÇÕES###########################

#Aplicação da primeira função(div_anos)
tabela_refinada = div_anos(entrada) #Função "quebra" o vetor de entrada de todos os meses em um dataframe cujas colunas se referem aos meses e as linhas aos anos

#Aplicação da segunda função(relatorio_estatistico)
Relatorio = relatorio_estatistico(tabela_refinada)

#Aplicação da terceira função(desagrega)

Desagregado=desagrega_mult(serie_sintetica,tabela_refinada,10)

#Aplicações diversas que gram gráficos, posteriormente provavelmente serão colocadas em outras funções
###########################################
#Time_serie= ts(entrada$VAZAO,start=c(1,1) ,end=c(qtd_ano_hist,12),deltat = 1/12,class="ts") 

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
