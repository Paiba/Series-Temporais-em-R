##################FUNÇÕES####################
input_3 = scan(what = integer(), nmax = 1)#N?mero de s?ries sint?ticas de tamanho igual a hist?rica a ser desagregado

#Esta função pega os dados brutos e retorna uma tabela ano x mes, 
#além disso plota 2 gráficos necessários na análise
div_mensais<-function(sH)
{
  qtd_ano_hist = length(sH[,1])/12                        #Calcula, baseado no número de linhas a quantidade de anos registrados no arquivo
  serie_hist = matrix(sH$VAZAO, qtd_ano_hist,byrow = TRUE)#Quebra o dataframe em qtd_anos_hist partes(anos) e cada parte é convertida numa linha da nova tabela
  
  anos = as.character(sH$MES)
  anos = substr(anos, nchar(anos)-4+1, nchar(anos))
  anos = unique(anos)
  anos = sort(anos)                                       #Pega a coluna de datas e interpreta quais os anos foram analisados baseados nos ultimos 4 caracteres da informação da coluna "MES"
  
  row.names(serie_hist)= anos
  colnames(serie_hist)=c("JAN","FEV","MAR","ABR","MAI","JUN","JUL","AGO","SET","OUT","NOV","DEZ")
  serie_hist=as.data.frame(serie_hist)                    #Nomeia linhas e colunas e converte a matriz em um dataframe
  ### !!! ainda tem que melhorar os gráficos, título, etc
  boxplot(serie_hist,ylab = "Vaz�es M�dias Mensais (m�/s)")
  
  #plot(anos,apply(serie_hist,1,sum),type="b",main="Vaz�es Anuais",xlab = "Ano", ylab="Vaz�es (m�/s)")
  
  return(serie_hist)
}

#Pega os dados de entrada de vazão mensal e gera um relatório de estatísticas com as médias mensais, desvio padrão, assimetria e coeficiente de kurtosis
relatorio_estatistico<-function(tabela_anual)
{
  Medias = apply(tabela_anual,2,mean)
  Desvio_Padrao = apply(tabela_anual,2,sd)
  #Assimetria = skewness(tabela_anual)  #aqui
  Assimetria = apply(tabela_anual,2,skewness)
  #Indice_Kurt = kurtosis(tabela_anual) #aqui
  Indice_Kurt = apply(tabela_anual,2,kurtosis)
  Coef_Var = (Desvio_Padrao/Medias)
  Relatorio = data.frame(Medias,Desvio_Padrao,Assimetria,Indice_Kurt,Coef_Var) 
  return(Relatorio)  
}


#########################Relatório estatístico das séries desagregadas##############################

Estatisticas_Serie_1<-function(serie){
  
  estatisticas = list()
  if(class(serie)=="list"){
    estatisticas = list()
    for(i in 1:length(serie)){
      estatisticas[[i]] = Estatisticas_Serie_1(as.data.frame(serie[[i]]))
    }
  }
  else
  {
    estatisticas$sd = apply(serie,2,sd)
    estatisticas$mean = apply(serie,2,mean)
    estatisticas$skewness = apply(serie,2,skewness)
    estatisticas$kurtosis = apply(serie,2,kurtosis)
    estatisticas$coef_Var = as.vector(apply(serie,2,sd))/as.vector(apply(serie,2,mean))
    estatisticas$hurst = rbind(Mensal = Indice_Hurst_Mensal(serie),Anual = Indice_Hurst(apply(serie,1,sum)))
    
  }
  return(estatisticas)
}

Estatisticas_Serie_2<-function(serie){
  df = lista_df(serie)
  Estatisticas_Serie_1(df)
}


###################
##ERRO MAPE
Erros_Desag_Hist_MAPE_1<-function(serie_sint){
  ERROS = list()
  historica = Estatisticas_Serie_1(SeriesDadosHist)
  sintetica = Estatisticas_Serie_1(serie_sint)
  
  if(class(serie_sint)=="list"){
    for(i in 1:length(serie_sint)){
      ERROS[[i]] = Erros_Desag_Hist_MAPE_1(as.data.frame(serie_sint[[i]]))
    }
  }
  else
  {
    ERROS$sd = (historica$sd - sintetica$sd)/historica$sd
    ERROS$sd = mean(ERROS$sd)
  
    ERROS$mean = (historica$mean - sintetica$mean)/historica$mean
    ERROS$mean = mean(ERROS$mean)*100
  
    ERROS$skewness = (historica$skewness - sintetica$skewness)/historica$skewness
    ERROS$skewness = mean(ERROS$skewness)*100
  
    ERROS$kurtosis =  (historica$kurtosis - sintetica$kurtosis)/historica$kurtosis
    ERROS$kurtosis = mean(ERROS$kurtosis)*100
    
    ERROS$coef_Var = (historica$coef_Var - sintetica$coef_Var)/historica$coef_Var
    ERROS$coef_Var = mean(ERROS$coef_Var)*100
  
    ERROS$hurst =  ((historica$hurst - sintetica$hurst)/historica$hurst)*100
  }
  return(ERROS)
}

Corrige_MAPE_1<-function(a){
  correto = list()
  tam = length(a)
  SD=0;MEAN=0;SKEWNESS=0;KURTOSIS=0;COEF_VAR=0;HURST=0;
  
  for(i in 1:tam)
  {
    SD = SD + getElement(a[[i]],"sd")
    MEAN = MEAN + getElement(a[[i]],"mean")
    SKEWNESS = SKEWNESS + getElement(a[[i]],"skewness")
    KURTOSIS = KURTOSIS + getElement(a[[i]],"kurtosis")
    COEF_VAR = COEF_VAR + getElement(a[[i]],"coef_Var")
    HURST = HURST + getElement(a[[i]],"hurst")
  }
  correto$sd = SD/tam
  correto$mean = MEAN/tam
  correto$skewness = SKEWNESS/tam
  correto$kurtosis = KURTOSIS/tam
  correto$coef_var = COEF_VAR/tam
  correto$hurst =  HURST/tam
  
  return(correto)
}
####dados corridos

Erros_Desag_Hist_MAPE_2<-function(serie_sint){
  df = lista_df(serie_sint)
  Erros_Desag_Hist_MAPE_1(df)
}

Indice_Hurst<-function(serie){
  N = length(serie)
  soma_ac<-vector();
  dif_media<-vector();
  media_serie<-mean(serie); 
  sd_serie<-sd(serie); 
  dif_media[1]<-(serie[1]-media_serie[1])^2;
  soma_ac[1]<-0;
  for(i in 2:(N)){
    soma_ac[i]<-soma_ac[i-1]+(serie[i]-media_serie);
    dif_media[i]<-(serie[i]-media_serie)^2;
  }
  #dif_media;
  soma_ac[N]<-0;
  #soma_ac;
  soma_dif_media<-sum(dif_media);
  Range<-max(soma_ac)-min(soma_ac);
  Desvio_D<-(N^(-0.5))*(soma_dif_media^0.5); 
  Range_escalonado<-Range/Desvio_D;
  Hurst<-log(Range_escalonado)/(log((N)/2)); 
  return(Hurst);
}#Para dados anuais, recebe uma coluna com as somas anuais

Indice_Hurst_Mensal<-function(serie){
  vetor = 0
  rownames(serie) = NULL
  colnames(serie) = NULL
  for(i in 1:length(serie[,1])){
    if(i==1){
      vetor = (serie[1,])
    }
    else{
      vetor = cbind(vetor,(serie[i,]))
    }
  }
  vetor = t(vetor)
  return(Indice_Hurst(vetor))
}#Para dados mensais, Entrada é um dataframe ano X mes


#########################Relatório comparativos de ACF da série histórica x sintética##############################
transform_des<-function(Desagreg){
  aux=matrix(t(as.matrix(Desagreg[,-13])), ncol=1, nrow=nrow(Desagreg)*(ncol(Desagreg)-1), byrow = FALSE)
  return (aux)
}

acf_desag<-function(elemento){
  tam = length(elemento)
  
  listaACF=list()
  
  for(i in 1:tam){
    aux=transform_des(elemento[[i]])
    
    Timeserie = ts(aux,start=c(1,1) ,end=c(qtd_ano_hist,12),deltat = 1/12,class="ts")
    Correlacao_Sazonal = peacf(Timeserie,12, plot = F)
    listaACF[[i]]=Correlacao_Sazonal$acf
    
  }
  return(listaACF)
}

acf_desag_med<-function(elemento){
  t = acf_desag(elemento)
  a = data.frame() 
  for(i in 1:length(t))
  {
    if(i==1)
      a=t[[i]]
    else
      a = a+as.data.frame(t[[i]])
  }
  a=a/length(t)
  return(a)
}

##########################Maximos e Minimos############################

#Faz o maximo e o minimo da serie considerando separadamente series sintéticas diferentes
max_min_serie_1<-function(serie){
  maxmin=0
  if(class(serie)=="list")
  {
    maxmin = list()
    for(i in 1:length(serie)){
      maxmin[[i]] = max_min_serie_1(as.data.frame(serie[[i]]))
    }
  }
  else{
    max_ = apply(serie,2,max)
    min_ = apply(serie,2,min)
    maxmin = rbind(max_,min_)
  }
  return(maxmin)
}
#Concatena as series e define min e max global
max_min_serie_2<-function(serie){
  df = lista_df(serie)
  max_min_serie_1(df)
}


#############Desagrega de forma não paramétrica os dados sintéticos utilizando estatísticas e dados históricos
###Método proposto por LEE, T., SALAS, J.D., PRAIRIE, J. (2010) DOI: 10.1029/2009WR007761

desagrega_np<-function(serieSint)
{
  desagregado_final = data.frame()#Cria um data frame vazio que será nossa série anual 
  
  ##################PRIMEIRA ITERAÇÃO DE DESAGREGAÇÃO##########################
  
  Anuais = data.frame(V1=apply(SeriesDadosHist,1,sum))#Dados as vazões mensais, calcula as vazões anuais
  Anuais_1 = rownames(Anuais)[1]
  primeiro_ano = rep(serieSint[1,1],length(Anuais))
  delta_i = abs(primeiro_ano-Anuais)#Faz um vetor da diferença(delta_i) do primeiro ano sintético referente a vazão anual com todos os anos históricos( |X1-xi| )
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
  candidato = rownames(Tabela)[posicao] #Armazena o ano candidato a desagregação na variavel 'candidato'

  
  desagregado = SeriesDadosHist[candidato,]*(serieSint$V1[1]/(apply(SeriesDadosHist[candidato,],1,sum)))
  #desagregado = c(desagregado[1,],TOTAL=sum(desagregado[1,]))
  
  desagregado_final = rbind(desagregado_final,desagregado)
  
  ############FIM DA DESAGREGAÇÃO DO ANO 1##############
  Tabela = Tabela[order(row.names(Tabela)),]
  ############DESAGREGAÇÃO DOS OUTROS ANOS###########################
  for (j in 2:(nrow(serieSint)))
  {
    ########### CÁLCULO DO DELTA_i###########
    
    fi_1 = 1/var(Anuais[2:qtd_ano_hist,1])
    fi_2 = 1/var(SeriesDadosHist$DEZ[2:qtd_ano_hist])
    
    for(i in 2:qtd_ano_hist)
    {
      delta_i[i,1] = sqrt(fi_1*(serieSint$V1[j]-Anuais[i,1])^2 + fi_2*(desagregado_final$DEZ[j-1]-SeriesDadosHist$DEZ[i-1])^2)
    }
    
    Tabela[,2] = delta_i
    Tabela = Tabela[!(row.names(Tabela) %in% Anuais_1),]
    Tabela = Tabela[order(Tabela$delta_i),]
    
    random = runif(1)
    posicao = which.min(abs(cwm - random))
    candidato = rownames(Tabela)[posicao]
    
    desagregado = SeriesDadosHist[candidato,]*(serieSint$V1[j]/(apply(SeriesDadosHist[candidato,],1,sum)))
    #desagregado = c(desagregado[1,],TOTAL=sum(desagregado[1,]))
    
    desagregado_final = rbind(desagregado_final,desagregado)
    
    
  }
  rownames(desagregado_final) = NULL
  return(desagregado_final)
}

############Desagrega multiplas vezes (n vezes)################

desagrega_mult_np<-function(){
  if(qtd_ano_des<(input_3*qtd_ano_hist)){
    print("Quantidade de séries inválida")
    break
  }
  
  aux = tail(serie_sintetica,qtd_ano_hist*input_3)
  sintetica_quebrada = split(aux,rep(1:input_3,each=qtd_ano_hist))
  sintetica_quebrada = lapply(sintetica_quebrada, "rownames<-",NULL)
  
  lista<-list()
  if(input_3>0){
    for (i in 1:input_3)
    {
      lista[[i]] = desagrega_np(as.data.frame(sintetica_quebrada[i]))
    }
  }
  return(lista)
}

####################Desagregação Paramétrica##################################

##TABELAS DE ENTRADA QUE SÃO O INPUT DAS FUNÇÕES DE DESAGREGAÇÃO PARAMETRICA
desag_param_info<-function(){
  Info=list()
  for (i in 1:11){
    if (i==1)
      Info[[i]] = cbind(SeriesDadosHist[,i],SeriesDadosHist[,12],apply(SeriesDadosHist[,-1],1,sum),apply(SeriesDadosHist,1,sum))
    else if(i<11)
      Info[[i]] = cbind(SeriesDadosHist[,i],SeriesDadosHist[,i-1],apply(SeriesDadosHist[,((i+1):12)],1,sum),apply(SeriesDadosHist[,(i:12)],1,sum))
    else
      Info[[i]] = cbind(SeriesDadosHist[,i],SeriesDadosHist[,i-1],SeriesDadosHist[,12],apply(SeriesDadosHist[,(i:12)],1,sum))
  }
  return(Info)
}

desag_param_info_padronizado<-function(){
  Info=list()
  for (i in 1:11){
    if (i==1)
      Info[[i]] = cbind(SeriesDadosHist_padronizada[,i],SeriesDadosHist_padronizada[,12],padroniza_df(SeriesDadosHist[,-1]),padroniza_df(SeriesDadosHist))
    else if(i<11)
      Info[[i]] = cbind(SeriesDadosHist_padronizada[,i],SeriesDadosHist_padronizada[,i-1],padroniza_df(SeriesDadosHist[,((i+1):12)]),padroniza_df(SeriesDadosHist[,(i:12)]))
    else
      Info[[i]] = cbind(SeriesDadosHist_padronizada[,i],SeriesDadosHist_padronizada[,i-1],SeriesDadosHist_padronizada[,12],padroniza_df(SeriesDadosHist[,(i:12)]))
  }
  return(Info)
}

############################

###########Funções que desagregam multiplos anos(Normal e Padronizada)
desag_param_mult<-function(info){
  aux = tail(serie_sintetica,qtd_ano_hist*input_3)

  Desagregado = list()
  if(input_3<=0){
    print("Numero invalido de series a se desagregar")
    break
  }
  for(j in 1:input_3){
    ################MUDAR PARA OS ANOS FINAIS AO INVES DOS ANOS INICIAIS###########
    df_serie_desagregada_param = data.frame(matrix(NA, nrow = qtd_ano_hist, ncol = 12))
    cont = 1
    inicio =((qtd_ano_hist*(j-1))+1)
    fim = qtd_ano_hist*j
    for(i in inicio:fim){
      df_serie_desagregada_param[cont,] = desag_param(info, aux[i,1])
      cont = cont+1    
    }
    Desagregado[[j]] =  df_serie_desagregada_param
  }
  return(Desagregado)
}
desag_param_mult_pad<-function(info){
  aux = tail(serie_sintetica,qtd_ano_hist*input_3)
  aux = padroniza_df(aux)
  Desagregado = list()
  if(input_3<=0){
    print("Numero invalido de series a se desagregar")
    break
  }
  for(j in 1:input_3){
    df_serie_desagregada_param = data.frame(matrix(NA, nrow = qtd_ano_hist, ncol = 12))
    cont = 1
    inicio =((qtd_ano_hist*(j-1))+1)
    fim = qtd_ano_hist*j
    for(i in inicio:fim){
      df_serie_desagregada_param[cont,] = desag_param(info, aux[i,1])
      cont = cont+1    
    }
    Desagregado[[j]] =  df_serie_desagregada_param
  }
  return(Desagregado)
}

#########Função que desegrega
desag_param<-function(info, ano){
  Meses = 0
  Resto = 0
  for(i in 1:11){
    if(i==1){
      ACF_S = acf(info[[i]],lag.max = 1,type = "covariance", plot = FALSE)
      #print(ACF_S)
      ACF_S = ACF_S$acf
      Sxx = ACF_S[1,4,4] # Anual com Anual
      Sxx_ = ACF_S[2,4,4] # Anual com Anual lag1
      Syx = cbind(ACF_S[1,4,1],ACF_S[1,4,3]) # Sazonal com Anual
      Syz = cbind(ACF_S[1,2,1],ACF_S[1,2,3]) # Sazonal com Sazonalidade anterior  ##Alterei aqui
      Sxz = ACF_S[1,2,4] ##### Anual com Sazonalidade anterior //CONFERIR
      Szz = ACF_S[1,2,2] # Sazonalidade anterior com Sazonalidade anterior
      Syy = cbind(rbind(ACF_S[1,1,1],ACF_S[1,3,1]), rbind(ACF_S[1,1,3],ACF_S[1,3,3]))
      Sx_z = ACF_S[2,2,4] ##### Anual com //CONFERIR
      Sxz_cor = Sxx_%*%(solve(Sxx))%*%(Sx_z)
      Syz_cor = t(Syz)+(t(Syx)%*%solve(Sxx)%*%(Sxz_cor-Sxz))
      A = (t(Syx)-Syz_cor%*%solve(Szz)%*%(Sxz_cor))%*%solve(Sxx-Sxz_cor%*%solve(Szz)%*%Sxz_cor)
      C = (Syz_cor-A%*%Sxz_cor)%*%solve(Szz) 
      BBt = (Syy)-(A)%*%(Syx)-C%*%t(Syz_cor)
      B = chol(BBt,pivot = TRUE)
      #print(det(BBt))
      Bt = t(B)
      Y = A%*%ano+Bt%*%rbind(rnorm(1,0,1),rnorm(1,0,1))+C%*%SeriesDadosHist$DEZ[1] #Y = A%*%serie_sint[k]+Bt%*%erro+C%*%Meses
      Meses[i] = Y[1,1]
      Resto[i] = Y[2,1]
     
    }
    else{
      ACF_S = acf(info[[i]],lag.max = 1,type = "covariance", plot = FALSE)
      #print(ACF_S)
      ACF_S = ACF_S$acf
      Sxx = ACF_S[1,4,4] # Anual com Anual
      Sxx_ = ACF_S[2,4,4] # Anual com Anual lag1
      Syx = cbind(ACF_S[1,4,1],ACF_S[1,4,3]) # Sazonal com Anual
      Syz = cbind(ACF_S[1,2,1],ACF_S[1,2,3]) # Sazonal com Sazonalidade anterior  ##Alterei aqui
      Sxz = ACF_S[1,2,4] ##### Anual com Sazonalidade anterior //CONFERIR
      Szz = ACF_S[1,2,2] # Sazonalidade anterior com Sazonalidade anterior
      Syy = cbind(rbind(ACF_S[1,1,1],ACF_S[1,3,1]), rbind(ACF_S[1,1,3],ACF_S[1,3,3]))
      # Sx_z = ACF_S[2,2,4] ##### Anual com //CONFERIR
      #print(Sxx);print(Sxx_);print(Syx);print(Syz);print(Sxz);print(Szz);print(Syy);print(Sx_z);
      A = (t(Syx)-t(Syz)%*%solve(Szz)%*%(Sxz))%*%solve(Sxx-Sxz%*%solve(Szz)%*%Sxz)
      #print("A")
      # print(A)
      C = (t(Syz) - A%*%Sxz)%*%solve(Szz) 
      # print(C)
      BBt = (Syy)-(A)%*%(Syx)-C%*%(Syz)
      #print("BBt")
      #print(BBt)
      #print(det(BBt))
      B = chol(BBt,pivot=TRUE)
      #print("B")
      #print(B)
      Bt = t(B)
      Y = A%*%Resto[i-1]+Bt%*%rbind(rnorm(1,0,1),rnorm(1,0,1))+C%*%Meses[i-1]
      Meses[i] = Y[1,1]
      Resto[i] = Y[2,1]      
    }
    
  }
  Meses[12] = Resto[11]
  #print(Meses)
  #print(sum(Meses))
  return(Meses)
}




desag_param_padronizado<-function(info, ano){
  #Desagregado = list()
  #for(j in 1:length(ano[,1])){
  Meses = 0
  Resto = 0
  for(i in 1:11){
    if(i==1){
      ACF_S = acf(info[[i]],lag.max = 1,type = "covariance", plot = FALSE)
      #print(ACF_S)
      ACF_S = ACF_S$acf
      Sxx = ACF_S[1,4,4] # Anual com Anual
      Sxx_ = ACF_S[2,4,4] # Anual com Anual lag1
      Syx = cbind(ACF_S[1,4,1],ACF_S[1,4,3]) # Sazonal com Anual
      Syz = cbind(ACF_S[1,2,1],ACF_S[1,2,3]) # Sazonal com Sazonalidade anterior  ##Alterei aqui
      Sxz = ACF_S[1,2,4] ##### Anual com Sazonalidade anterior //CONFERIR
      Szz = ACF_S[1,2,2] # Sazonalidade anterior com Sazonalidade anterior
      Syy = cbind(rbind(ACF_S[1,1,1],ACF_S[1,3,1]), rbind(ACF_S[1,1,3],ACF_S[1,3,3]))
      Sx_z = ACF_S[2,2,4] ##### Anual com //CONFERIR
      Sxz_cor = Sxx_%*%(solve(Sxx))%*%(Sx_z)
      Syz_cor = t(Syz)+(t(Syx)%*%solve(Sxx)%*%(Sxz_cor-Sxz))
      A = (t(Syx)-Syz_cor%*%solve(Szz)%*%(Sxz_cor))%*%solve(Sxx-Sxz_cor%*%solve(Szz)%*%Sxz_cor)
      C = (Syz_cor-A%*%Sxz_cor)%*%solve(Szz) 
      BBt = (Syy)-(A)%*%(Syx)-C%*%t(Syz_cor)
      B = chol(BBt,pivot = TRUE)
      #print(det(BBt))
      Bt = t(B)
      Y = A%*%ano+Bt%*%rbind(rnorm(1,0,1),rnorm(1,0,1))+C%*%SeriesDadosHist$DEZ[1]
      Meses[i] = Y[1,1]
      Resto[i] = Y[2,1]
    }
    else{
      ACF_S = acf(info[[i]],lag.max = 1,type = "covariance", plot = FALSE)
      #print(ACF_S)
      ACF_S = ACF_S$acf
      Sxx = ACF_S[1,4,4] # Anual com Anual
      Sxx_ = ACF_S[2,4,4] # Anual com Anual lag1
      Syx = cbind(ACF_S[1,4,1],ACF_S[1,4,3]) # Sazonal com Anual
      Syz = cbind(ACF_S[1,2,1],ACF_S[1,2,3]) # Sazonal com Sazonalidade anterior  ##Alterei aqui
      Sxz = ACF_S[1,2,4] ##### Anual com Sazonalidade anterior //CONFERIR
      Szz = ACF_S[1,2,2] # Sazonalidade anterior com Sazonalidade anterior
      Syy = cbind(rbind(ACF_S[1,1,1],ACF_S[1,3,1]), rbind(ACF_S[1,1,3],ACF_S[1,3,3]))
      # Sx_z = ACF_S[2,2,4] ##### Anual com //CONFERIR
      #print(Sxx);print(Sxx_);print(Syx);print(Syz);print(Sxz);print(Szz);print(Syy);print(Sx_z);
      A = (t(Syx)-t(Syz)%*%solve(Szz)%*%(Sxz))%*%solve(Sxx-Sxz%*%solve(Szz)%*%Sxz)
      #print("A")
      # print(A)
      C = (t(Syz) - A%*%Sxz)%*%solve(Szz) 
      # print(C)
      BBt = (Syy)-(A)%*%(Syx)-C%*%(Syz)
      #print("BBt")
      #print(BBt)
      #print(det(BBt))
      B = chol(BBt,pivot=TRUE)
      #print("B")
      #print(B)
      Bt = t(B)
      Y = A%*%Resto[i-1]+Bt%*%rbind(rnorm(1,0,1),rnorm(1,0,1))+C%*%Meses[i-1]
      Meses[i] = Y[1,1]
      Resto[i] = Y[2,1]
      
    }
    
  }
  print(Meses)
  print(sum(Meses))
  print(Resto)
  #if(j==1)
  #Desagregado =  Meses
  #else
  #Desagregado = rbind(Desagregado,Meses)
  #}
  #return(Desagregado)
}

###DESPADRONIZA (PODE SER UTIL)
despadroniza<-function(desagpp){
  for(i in 1:length(desagpp)){
    desagpp[[i]] = (desagpp[[i]]*SD_serie_sint + M_serie_sint)
    desagpp[[i]] = exp(desagpp[[i]])
  }
  return(desagpp)
}


#########################################


######################################################
######################################################
# valores de A, B e C para os passos de 2 a 11
#A = (t(Syx)-Syz%*%solve(Szz)%*%(Sxz))%*%solve(Sxx-Sxz%*%solve(Szz)%*%Sxz)
#C = (Syz - A%*%Sxz)%*%solve(Szz) 
#BBt = (Syy)-(A)%*%(Syx)-C%*%t(Syz)




