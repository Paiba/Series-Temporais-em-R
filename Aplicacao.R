data_sim =  format(Sys.time(), "Simulacao  - %F %Hh%M") #Nome da pasta criada para a simula??o
#######################APLICAÇÕES###########################

#Aplicação da primeira função(div_mensais)
SeriesDadosHist = div_mensais(entrada) #Função "quebra" o vetor de entrada de todos os meses em um dataframe cujas colunas se referem aos meses e as linhas aos anos
SeriesDadosHist_padronizada = apply(log(SeriesDadosHist),2, function(x) (x-mean(x))/(sd(x)))
#Aplicação da segunda função(relatorio_estatistico)
Relatorio = relatorio_estatistico(SeriesDadosHist)
MAX_E_MIN_HIST = max_min_serie_1(SeriesDadosHist)
#Aplicação da desagregação não paramétrica

Desagregado1 = desagrega_mult_np()


#Aplicação da quinta função(relatorio_desag)

Erro1 = Erros_Desag_Hist_MAPE_1(Desagregado1)
Erro1 = Corrige_MAPE_1(Erro1)

Erro2 = Erros_Desag_Hist_MAPE_2(Desagregado1)
#Aplicação da sexta função
ACF_DES = acf_desag(Desagregado1)
ACF_DES_MED = acf_desag_med(Desagregado1)

#Aplicações diversas que gram gráficos, posteriormente provavelmente serão colocadas em outras funções
###########################################
Time_serie = ts(entrada$VAZAO,start=c(1,1) ,end=c(qtd_ano_hist,12),deltat = 1/12,class="ts") 

CorrelacaoSazonal = peacf(Time_serie,12, plot = F)

#CorrelacaoSazonalParcial = pepacf(Time_serie,12)
#CorrelacaoSazonalParcial

#ACF_N = acf(Time_serie)
#ACF_N
#ACF_Parcial = pacf(Time_serie)
#ACF_Parcial

#plot(entrada, type="b")
###################################################


Parametro_Hist = desag_param_info()
ParametroP_Hist = desag_param_info_padronizado()


DesagregadoP = desag_param_mult(Parametro_Hist)
for(i in 1:input_3){
  DesagregadoP[[i]] = abs(DesagregadoP[[i]])
}

DesagregadoPP = desag_param_mult_pad(ParametroP_Hist)

######Estatisticas NP
est1np = Estatisticas_Serie_1(Desagregado1)
est2np =  Estatisticas_Serie_2(Desagregado1)

err1np = Erros_Desag_Hist_MAPE_1(Desagregado1)
err2np = Erros_Desag_Hist_MAPE_2(Desagregado1)

#####Estatisticas P
est1p = Estatisticas_Serie_1(DesagregadoP)
est2p =  Estatisticas_Serie_2(DesagregadoP)

err1p = Erros_Desag_Hist_MAPE_1(DesagregadoP)
err2p = Erros_Desag_Hist_MAPE_2(DesagregadoP)

#####Estatisticas PP
est1pp = Estatisticas_Serie_1(DesagregadoPP)
est2pp =  Estatisticas_Serie_2(DesagregadoPP)

err1pp = Erros_Desag_Hist_MAPE_1(DesagregadoPP)
err2pp = Erros_Desag_Hist_MAPE_2(DesagregadoPP)

#teste = padroniza_df(SeriesDadosHist[,-1])
#print(teste)
########Variáveis Resultantes relevantes##########

#SeriesDadosHist -> Tabela com dados referentes a série histórica, as linhas são os anos as colunas são os meses
#Relatorio -> Dados estatísticos da série histórica
#Desagregado1 -> Lista de séris sintéticas desagregadas
#ERRO -> Erro em porcentagem das séries sintéticas desagregadas em relação a série histórica
#ACF_DES -> Acf calculado de todas as séries desagregadas





























###########CRIAÇÃO DE PASTAS###################

dir.create(file.path("./", format(Sys.time(), data_sim)))
#Pasta raiz dos outputs
#Pasta raiz dos outputs
#Pasta raiz dos outputs
#Pasta raiz dos outputs

pasta = paste(getwd(),"/",data_sim,"/",sep = "")

#Sub pastas de arquivos úteis que possam ser utilizados em futuras rotinas
#e arquivos visualmente melhores para apresentação de dados
pasta_uteis = paste(pasta,"/Arquivos uteis/",sep = "")
dir.create(file.path(pasta_uteis))
pasta_visual = paste(pasta,"/Arquivos de apresentacao/",sep = "")
dir.create(file.path(pasta_visual))
#INFO RAIZ
info_txt = paste("Esta simulação rodou utilizando os arquivos",input_1,"como dados históricos e",input_2,"como dados anuais sintéticos a serem desagregados")
write(info_txt, file = paste(pasta,"info.txt"))
#Sub pastas separadas por interesse
#####################

#####UTEIS#######
#Dados Históricos
pasta_hist_u = paste(pasta_uteis,"/Dados Historicos/",sep = "")
dir.create(file.path(pasta_hist_u))
write.csv2(SeriesDadosHist, file = paste(pasta_hist_u,"dados_historicos.csv",sep = ""))
write.csv2(Relatorio, file = paste(pasta_hist_u,"relatorio_estatistico_historico.csv",sep = ""))

###Desagregação não paramétrica
pasta_desagnp_u = paste(pasta_uteis,"/Desagregado nao parametrico/",sep = "")
dir.create(file.path(pasta_desagnp_u))
for(i in 1:length(Desagregado1)){
  write.csv2(Desagregado1[[i]], file = paste(pasta_desagnp_u,i,".csv",sep = ""))
}

####Desagregação parametrica
pasta_desagp_u = paste(pasta_uteis,"/Desagregado Parametrico/",sep = "")
dir.create(file.path(pasta_desagp_u))
for(i in 1:length(DesagregadoP)){
  write.csv2(DesagregadoP[[i]], file = paste(pasta_desagp_u,i,".csv",sep = ""))
}
#########VISUAL##########
#Dados Históricos

pasta_hist_v = paste(pasta_visual,"/Dados Historicos/",sep = "")
dir.create(file.path(pasta_hist_v))
#pdf(file = paste(pasta_hist_v,"dados_historicos.pdf"),height=16, width=15)
#grid.table(SeriesDadosHist)
#dev.off()
#pdf(file = paste(pasta_hist_v,"relatorio_estatistico_historico.pdf"),height=10, width=15)
#grid.table(Relatorio)
#dev.off()


###Desagregação não paramétrica
pasta_desagnp_v = paste(pasta_visual,"/Desagregado nao parametrico/",sep = "")
dir.create(file.path(pasta_desagnp_v))
#for(i in 1:length(Desagregado1)){
#  pdf(file = paste(pasta_desagnp_v,i,".pdf",sep = ""), height=16, width=23)#49x12, mas as colunas são mais largas que as linhas
#  grid.table(Desagregado1[[i]])
#  dev.off()
#}
####Desagregação parametrica
#pasta_desagp_v = paste(pasta_visual,"/Desagregado Parametrico/",sep = "")
#dir.create(file.path(pasta_desagp_v))




#pdf("test.pdf", height=11, width=10)
#grid.table(SeriesDadosHist)

#PMIX = c(Erro2$sd,Erro2$mean,Erro2$coef_Var,Erro2$kurtosis,Erro2$skewness,Erro2$hurst[1,1],Erro2$hurst[2,1])
#PAR = c(Erro2$sd,Erro2$mean,Erro2$coef_Var,Erro2$kurtosis,Erro2$skewness,Erro2$hurst[1,1],Erro2$hurst[2,1])
#barplot(as.matrix(total), beside=T, col = topo.colors(2))
#legend("bottomleft", inset=.02, title=NULL,rownames(total),fill=topo.colors(2),
 #      cex=0.8)