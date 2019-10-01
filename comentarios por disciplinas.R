#' ---
#' title: "Avaliação CINFO - Disciplinas e Professores 2017-2"
#' author: "Comissão de Avaliação CINFO"
#' date: "07/Março/2018"
#' ---
#' * Avaliação Curso Bacharelado em Sistemas de Informação
#' * Semestre 2017-2
#' * Aplicação questionário: Dez/2017
#+ main, echo=FALSE, comment=NA 

#+ main, echo=FALSE, comment=NA 

arquivo1  <<- './dados-avaliacao2018-1/Formulario-Avaliacao-BSI-2018-1__Infra-Perfil-a (respostas).csv'
arquivo2  <<- './avaliacao2018-1/dados-avaliacao2018-1/Formulario-Avaliacao-BSI-2018-1__Infra-Perfil-a (respostas).csv'

configura_header  <- function(){
  header  <<- c(
    "data e hora",
    "Disciplina",
    "Professor",
    "Apresentação e cumprimento do programa",
    "Grau de atualização do conteúdo ministrado",
    "Existência de exercícios ou atividades propostas",
    "Apresentação do material didático e complementar",#"Apresentação do material didático e complementar adequado ao conteúdo da disciplina",
    "Comparece às aulas e cumpre os horários", #"Comparece regularmente às aulas sob e cumpre os horários de início e término das aulas?",
    "Organização e clareza nas explicações", #"Organização das aulas, clareza na apresentação e explicação do conteúdo e na resolução de exercícios (didática)",
    "Demonstra conhecimento, utiliza metodologias diversificadas",
    "Demonstra disponibilidade para o atendimento",
    "Estabelece uma relação harmônica, cordial e educada",
    "Avaliação: informa critérios, aborda os conteúdo e devolve",
    "Participação nas aulas",
    "Interesse pela disciplina",
    "Aquisição de novos conhecimentos",
    "Comentários, críticas e sugestões sobre a disciplina",# (conteúdo e material didático)",
    "Comentários, críticas e sugestões sobre o professor",
    "Motivo desistência disciplina (caso tenha havido)")# da disciplina"  
  perguntas <<- c("Indicação de data e hora",
                  "DISCIPLINA",
                  "PROFESSOR",
                  "[Apresentação e cumprimento do programa]",
                  "[Grau de atualização do conteúdo ministrado]",
                  "[Existência de exercícios ou atividades propostas]",
                  "[Apresentação do material didático e complementar adequado ao conteúdo da disciplina]",
                  "[Comparece regularmente às aulas sob e cumpre os horários de início e término das aulas?]",
                  "[Organização das aulas, clareza na apresentação e explicação do conteúdo e na resolução de exercícios (didática)]",
                  "[Demonstra conhecimento do conteúdo ministrado e utiliza metodologias diversificadas e/ou instigadoras para o processo de ensino-aprendizagem?]",
                  "[Demonstra disponibilidade para o atendimento ao aluno?]",
                  "[Estabelece uma relação harmônica, cordial e educada com os alunos?]",
                  "[Informa antecipadamente e com clareza os critérios de avaliação, aborda os conteúdo selecionadas e devolve regularmente as avaliações da aprendizagem após serem apreciadas e corrigidas?]",
                  "[Participação nas aulas]",
                  "[Interesse pela disciplina]",
                  "[Aquisição de novos conhecimentos]",
                  "Comentários, críticas e sugestões sobre a disciplina (conteúdo e material didático)",
                  "Comentários, críticas e sugestões sobre o professor da disciplina",
                  "Se desistiu da disciplina no decorrer do período letivo, conte-nos o motivo.")
}

#CONFIGURAÇÃO DE EXECUÇÃO
#1grafico (utilizar modo geração pdf/doc) #2prompt (modo para exibição no RStudio)
configura_geral   <- function(exec){

    if (exec == 1) #1grafico #2prompt 
      arquivo_disciplinas  <<- '../dados-avaliacao2015-2/Formulario-Avaliacao-BSI-2015-2__Disciplinas-a (respostas).csv'
    else
      arquivo_disciplinas  <<- './dados-avaliacao2015-2/Formulario-Avaliacao-BSI-2015-2__Disciplinas-a (respostas).csv'
    
  enc1= 'LATIN1' #'UTF-8' 
  dados_disciplinas  <<- read.csv2(arquivo_disciplinas, header=T, encoding = enc1, na.strings=c("", "NA"))
  names(dados_disciplinas) <<- header

  #q4-q16
  levels1 <<- c('MUITO BOM', 'BOM', 'REGULAR','RUIM', 'PÉSSIMO')

  professores  <<- levels(dados_disciplinas$Professor)
  disciplinas  <<- levels(dados_disciplinas$Disciplina)
  
  nprofessores <<- nrow(table(professores))
  ndisciplinas <<- nrow(table(disciplinas))
  
  idDisciplina <<- c(2:3)
  gdisciplina  <<- c(4:7)#E-M 5:13 #
  gprofessor   <<- c(8:13)#
  gautoAvalia  <<- c(14:16)
  gcomentarios <<- c(17:19)
  colunas      <<- c(gdisciplina, gprofessor, gautoAvalia) #1..46
  
}

dadosPorProfessorDisciplina <- function(dados,nomeProfessor,nomeDisciplina){
  f1 <- subset(dados,Professor==nomeProfessor)
  f2 <- subset(f1,Disciplina==nomeDisciplina)
  return(f2)
}

escreve <- function(texto){
  cat(texto,sep="\n")
}
#' 
#' ## Lista de Professores / Disciplinas
#+ info, echo=FALSE, comment=NA 
info  <- function(fonte){
  #escreve("Professor(es) Avaliado(s)")
  escreve(c("Professor: " ,levels(factor(fonte$Professor))))
  #escreve("Disciplina(s) Avaliada(s)")
  escreve(c("Disciplina: ",levels(factor(fonte$Disciplina))))
  escreve(c("No de Respostas: ",nrow(fonte)))
  escreve("")
}

resumoAvaliacoes <-function(fonte, colunas1,legenda){
  cores  = c('dodgerblue3','deepskyblue','gray','coral1','red','yellow','brown','beige','azure')
  par(mar=c(4,2,4,1))
  dSoma <<- 0
  for(i in colunas1){
    dTemp <<- (factor(fonte[,i], levels=levels1))
    dSoma <<- factor(as.integer(dSoma) + as.integer(dTemp))
  }
  #floor or ceiling
  #print(round(as.numeric(dSoma)/length(colunas)))
  dSoma1 <<- factor(floor(as.numeric(dSoma)/length(colunas1)))
  levels(dSoma1) <<- levels1
  dSoma2 <<- table(dSoma1)
  header2  <- legenda
  
  bp  <- barplot(dSoma2, col = cores)#, angle=angulo, density= densi)
  
  title(main = header2, font.main = 2)
  mtext(side = 1, at = bp, line = -2,
          text = paste("",round(dSoma2/sum(dSoma2)*100,2),"%"))
}


## ##Resumo das avaliações 
## RESUMO, echo=FALSE, comment=NA
resumos <- function(fonte){

  escreve("### Média das avaliações da categoria Disciplina")
  resumoAvaliacoes(fonte,gdisciplina,"Disciplina")
  
  escreve("### Média das avaliações da Categoria Professor")
  resumoAvaliacoes(fonte,gprofessor,"Professor")
  
  escreve("### Média das avaliações da Categoria AutoAvaliação do Aluno")
  resumoAvaliacoes(fonte,gautoAvalia,"Auto Avaliação do Aluno")
  
  escreve("")
}
  

#+ graficos, echo=FALSE, comment=NA, fig.width=6, fig.height=5
graficos  <- function(fonte, colunas){
  info(fonte)
  
  cores  = c('dodgerblue3','deepskyblue','gray','coral1','red','yellow','brown','beige','azure')
  #angulo = 10+30*0:9
  #densi  = 10*0:9
  par(mar=c(4,2,4,1))#, fin=c(5.0,3))
  
  for(i in colunas){
    header2  <- names(fonte[i])
    legenda2 = ""
    rotulo2 = ""
    escreve("")
    escreve("")
    if (i %in% gdisciplina)
      escreve('AVALIAÇÃO DA DISCIPLINA')
    if (i %in% gprofessor)
      escreve('AVALIAÇÃO DO PROFESSOR DA DISCIPLINA')
    if (i %in% gautoAvalia)
      escreve('AUTO-AVALIAÇÃO DO ALUNO')
    escreve(paste('Questão #',i,": ", perguntas[i]))
    
    if (i %in% gdisciplina || i %in% gprofessor || i %in% gautoAvalia){
      
      dados2  <- table(factor(fonte[,i], levels=levels1))
      
      bp  <- barplot(dados2, col = cores)#, angle=angulo, density= densi)
      title(main = header2, font.main = 2)
      mtext(side = 1, at = bp, line = -2,
            text = paste("",round(dados2/sum(dados2)*100,1),"%"))
    }
  }
}

comentarios_alunos  <-  function(fonte, colunas){

  escreve('COMENTÁRIOS, CRITICAS E SUGESTÕES')
  
  for(i in colunas){
    header2  <- names(fonte[i])
    escreve(paste('Questão #',i,": ", header2))
    fonte2 <- subset(fonte, !is.na(fonte[i]))
    print(fonte2[i])
#      for(j in nrow(fonte2[i])){
#          print(as.character.factor(fonte2[j,i]))
#      }
  }
}

avaliaGeral  <- function(fonte){
    graficos(fonte, colunas)
    comentarios_alunos(fonte, gcomentarios)
}

avaliaPorPeriodo  <- function(fonte){
  
  #for (i in 1:8){
  for (i in 8:8){
    dadosTemp <- subset(fonte,Período==i)
    graficos(dadosTemp, colunas)
    comentarios_alunos(dadosTemp, gcomentarios)
  }  
  
}
avaliaPorProfessor  <- function(fonte){
  #for (i in 1:nprofessores){
  #c = 1:24
  c =24
  for (i in c){
    dadosTemp <- subset(fonte,Professor==professores[i])
    graficos(dadosTemp, colunas)
    comentarios_alunos(dadosTemp, gcomentarios)
  }  
}

infoProfDisciplinaResposta  <- function(fonte){
  
  for (i in 1:nprofessores){
    prof <- subset(fonte,Professor==professores[i])
    info(prof)
  } 
  
}

avaliaPorDisciplinaGraficosComentarios  <- function(fonte){
  #for (i in 1:24:nprofessores){
  c = 24
  for (i in c){
    prof <- subset(fonte,Professor==professores[i])
    disc <- levels(factor(prof$Disciplina))
    ndisc <- nrow(table(disc))
    #info(prof)
    
    for (j in 1:ndisc){
      dadosTemp = dadosPorProfessorDisciplina(fonte,professores[i], disc[j])
      graficos(dadosTemp, colunas)
      info(dadosTemp)
      comentarios_alunos(dadosTemp, gcomentarios)
    }
  }  
}

avaliaPorDisciplinaSomenteComentarios  <- function(fonte){
  #for (i in 1:24:nprofessores){
  for (i in 1:24){
    prof <- subset(fonte,Professor==professores[i])
    disc <- levels(factor(prof$Disciplina))
    ndisc <- nrow(table(disc))
    #info(prof)
    
    for (j in 1:ndisc){
      dadosTemp = dadosPorProfessorDisciplina(fonte,professores[i], disc[j])
      #graficos(dadosTemp, colunas)
      info(dadosTemp)
      comentarios_alunos(dadosTemp, gcomentarios)
    }
  }  
}

infoProfDisciplinaResposta  <- function(fonte){
  
  for (i in 1:nprofessores){
    prof <- subset(fonte,Professor==professores[i])
    info(prof)
  }  
}

configura_header()
configura_geral(1) #1grafico #2prompt 

###Média das avaliações da categoria Disciplina
#resumos(dados_disciplinas) #NAO?? 

###Avaliação com comentários!
#avaliaGeral(dados_disciplinas)     

#Resumo/lista professores/disc e respostas
#infoProfDisciplinaResposta(dados_disciplinas)

#nao realizado!!!
#avaliaPorPeriodo(dados_disciplinas) 

 #Graficos da avaliação do professor
#avaliaPorProfessor(dados_disciplinas)

#comentários E GRAFICOS professores/disciplinas 
avaliaPorDisciplinaGraficosComentarios(dados_disciplinas) 

#comentários professores/disciplinas 
#avaliaPorDisciplinaSomenteComentarios(dados_disciplinas) 
