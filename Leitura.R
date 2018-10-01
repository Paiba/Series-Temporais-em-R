##############ENTRADA E INSTRU??ES REFERENTES AS INFORMA??ES BRUTAS################

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



require(moments)
require(corrgram)
require(pear)
require(gridExtra)
require(knitr)
require(tcltk)#Pacote incluido para rodar em windows e linux (fun??o tk_choose.files())

#LEITURA DE ARQUIVOS DE ENTRADA E VARI?VEIS ?TEIS#
input_1 = tk_choose.files()#Arquivo de s?rie hist?rica
input_2 = tk_choose.files()#Arquivo de vaz?es sin?ticas

entrada = data.frame(read.csv(input_1, sep=";", dec=",")) #Leitura de dados hist?ricos mensais
serie_sintetica = data.frame(read.csv(input_2, header = F,sep=";", dec=",")) #Leitura de s?rie sint?tica gerada para essa mesma s?rie


serie_sintetica_padronizada= apply(log(serie_sintetica), 2, function(x) (x-mean(x))/(sd(x)))
M_serie_sint = apply(log(serie_sintetica), 2, function(x) (mean(x)))
SD_serie_sint = apply(log(serie_sintetica), 2, function(x) (sd(x)))
qtd_ano_hist = length(entrada[,1])/12 #Quantidade de anos nos dados hist?ricos baseado no arquivo de entrada
qtd_ano_des = length(serie_sintetica[,1])

if(qtd_ano_des<qtd_ano_hist){
  print("Aviso: A s?rie hist?rica ? maior que a sint?tica!")
}


####Fun??o auxiliar que transforma uma lista num grande dataframe####
lista_df<-function(lista){
  df = data.frame()
  for(i in 1:length(lista)){
    if (i==1){
      df = lista[[i]]
    }
    else{
      df = rbind(df, as.data.frame(lista[[i]]))
    }
  }
  return(df)
}

#####Padroniza??o de df############

##Padroniza os dados anuais de uma dataframe com informa??es mensais
padroniza_df<-function(df){
  df_mod = apply(df,1,sum)
  df_mod = as.data.frame(df_mod)
  df_padronizada= apply(log(df_mod), 2, function(x) (x-mean(x))/(sd(x)))
  return(df_padronizada)
}
