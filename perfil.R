#+ padrao nome arquivo: Avaliacao-20xx-x__perfil--infraestrutura.docx

#' ---
#' title: "Avaliação CINFO -
#' Perfil Alunos e Infraestrutura 2018-2"
#' author: "Comissão de Avaliação CINFO"
#' date: "22/Março/2019"
#' ---
#' * Avaliação Curso Bacharelado em Sistemas de Informação
#' * Semestre 2018-2
#' * Aplicação questionário: Dez/2018

#+ main, echo=FALSE, comment=NA, fig.width=6.3

arquivo1  <<- './dados-avaliacao2018-2/Formulario-Avaliacao-BSI-2018-2__Infra-Perfil-a (respostas).csv'
arquivo2  <<- './avaliacao2018-2/dados-avaliacao2018-2/Formulario-Avaliacao-BSI-2018-2__Infra-Perfil-a (respostas).csv'

escreve <- function(texto){
  cat(texto,sep="\n")
}

#configurações
configura_margem  <- function(){
  
  #par(mai=c(1.02,0.82,0.82,0.42))
}

#configura tipo de execução
#1grafico #2prompt 
configura <- function(exec){
  if (exec == 1) #1grafico #2prompt 
    arquivo_perfil  <<- arquivo1
  else
    arquivo_perfil  <<- arquivo2
  
  enc1= 'LATIN1' #"ISO-8859-1"
  dados_perfil  <<- read.table(arquivo_perfil, sep=';', 
                               header=T, encoding = enc1)
  header  <<-  c("Data e hora", 
                 "Período",
                 "Onde você mora",
                 "Faixa etária",
                 "Como você se considera",
                 "Renda familiar mensal",
                 "Em que tipo de escola cursou o ensino médio",
                 "Possui emprego",
                 "Seu trabalho é na área de informática",
                 "Realizando(realizou) estágio em informática",
                 "Você se sente motivado com o curso",
                 "Condições gerais da infraestrutura física",
                 "Atualização do acervo bibliográfico",
                 "Sequência de disciplinas da grade curricular",
                 "Relação conteúdo(disciplinas)/mercado de trabalho",
                 "Críticas e Sugestões"
  )
  names(dados_perfil) <<- header
  
  #q12-q15
  #levels1 <<- c('Muito Bom', 'Bom','Neutro','Ruim','Muito Ruim')
  #levels1 <<- c('Muito Bom', 'Bom','Regular','Ruim','Péssimo')
  levels1 <<- c('MUITO BOM', 'BOM','NEUTRO','RUIM','MUITO RUIM')
  levels2 <<- 1:8 #q2
  levels3 <<- c('Capital', 'Interior') #q3
  #q4
  levels9 <<- c( 
  'Menos de 20 anos',
  'De 20 a 30 anos',
  'Mais de 30 anos'
  )
  #q5
  levels10 <<- c(
    'Branco (a)',
    'Negro (a)',
    'Pardo (a) / mulato (a)',
    'Amarelo (a) (de origem oriental)',
    'Indígena ou de origem indígena'
  )

  label10 <<- c(
    'Branco',
    'Negro',
    'Pardo/mulato',
    'Amarelo(oriental)',
    'Indígena'
  )
  #q6
  levels11  <<-  c(
    "Até 1,5 salário mínimo (até R$ 1.405)", 
    "Acima de 1,5 até 3 salários mínimos (R$  1.405 a R$ 2.811)",
    "Acima de 3 até 5 salários mínimos (R$ 2.811 a R$ 4.685)",
    "Acima de 5 até 7 salários mínimos (R$ 4.685 a R$ 6.559)",
    "Acima de 7 até 10 salários mínimos (R$ 6.559 a R$ 9.370)",
    "Acima de 10 até 30 salários mínimos  (R$ 9.370 a R$28.110)",
    "Acima de 30 salários mínimos (mais de R$  28.110)"
  )
  
  
  label11  <<-  c('Até 1,5 SM',
                  'Entre 1,5 e 3 SM',
                  'Entre 3 e 5 SM',
                  'Entre 5 e 7 SM',
                  'Entre 7 e 10 SM',
                  'Entre 10 e 30 SM',
                  'Acima de 30 SM'
  )
  levels13 = c('Sim','Não')#q8#q9#q10
  levels14 = c('Sim','Não','Não se aplica')#q1
}

plotHistPercent <- function(x, ...) {
  H <- hist(x, plot = FALSE)
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density), "%", sep="")
  plot(H, freq = FALSE, labels = labs, ylim=c(0, 1.08*max(H$density)),...)
  
}

plot3  <- function(temp_dados, main, ...){
  barplot(temp_dados, main, ...)
}

table2 <- function(dados, levels){
  #return table(factor(dados_perfil[,i],levels=levels1))
}

procentagem <- function(dados){
  round(dados1/sum(dados1)*100);
}

grafico11  <- function(){
  i = 11
  header  <- names(dados_perfil[i])
  dados  <- 0 ;
  escreve(paste('Questão #',i,": ", header))
  
  dados  <- table(factor(dados_perfil[,i],levels=levels11))
  
  bp  <- barplot(dados, main=header, legend.text=label11, names.arg=label11)
  mtext(side = 1, at = bp, line = -2,
        text = paste("",round(dados/sum(dados)*100),"%"))
}

graficos  <- function(){
  cores = c('dodgerblue3','deepskyblue','white','coral1','red','yellow','brown','beige','azure')
  for(i in 2:(length(dados_perfil)-1)){
    header  <- names(dados_perfil[i])
    dados  <- 0 ;
    legenda = ""
    rotulo = ""
    escreve(paste('Questão #',i,": ", header))
    if (i == 12 || i == 13 || i == 14 || i == 15){
      dados  <- table(factor(dados_perfil[,i],levels=levels1))
    }  
    else if (i == 4){
      dados  <- table(factor(dados_perfil[,i],levels=levels9))
    }  
    else if (i == 5){
      legenda = label10
      rotulo  = label10
      dados  <- table(factor(dados_perfil[,i],levels=levels10))
    }  
    else if (i == 6){
      legenda = label11
      rotulo  = label11
      dados   <- table(factor(dados_perfil[,i],levels=levels11))
    }else{
      dados  <- table(factor(dados_perfil[,i]))
    }
    par(mar=c(2.1,2.1,2.1,2.1))
    if (i == 5 || i == 6){
      bp  <- barplot(dados, main=header, 
                     legend.text=rotulo, names.arg=legenda, col=cores)
    }
    else
      bp  <- barplot(dados, main=header, col=cores)
    mtext(side = 1, at = bp, line = -2,
          text = paste("",round(dados/sum(dados)*100),"%"))
  }
}

comentarios_alunos  <-  function(){
  #for(i in 1:length(dados_perfil[[2]]))
  #  print(paste(i,". ",dados_perfil[i,2]))
  dados_perfil[[16]]
}

comentarios_iniciais  <- function(){
  qtdAlunos <<- nrow(dados_perfil)
  txtAlunos  <- paste('Quantidade de alunos participantes da pesquisa:',qtdAlunos)
  escreve(txtAlunos)
}

configura_margem()
configura(2) #1grafico #2prompt 
comentarios_iniciais()
graficos()
#' # Comentários dos alunos
#+ comentarios, echo=FALSE, comment=NA
comentarios_alunos()

#+ padrao nome arquivo: Avaliacao-20xx-x__perfil--infraestrutura.docx