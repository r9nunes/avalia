#+ Este script apresenta relatório por professor para progressão.
#+ Configure cabeçalho
#+ Configure dados em #16, 17
#+ Configure qtd professores em #126

#' ---
#' title: "Avaliação CINFO -  Professores - 2018-2"
#' author: "Comissão de Avaliação CINFO"
#' date: "22/Março/2019"
#' ---
#' * Avaliação Curso Bacharelado em Sistemas de Informação\n
#' * Semestre 2018-2\n
#' * Aplicação questionário:  Dez/2018
#+ main, echo=FALSE, comment=NA 

arquivo1  <<- './dados-avaliacao2018-2/Formulario-Avaliacao-BSI-2018-2__Disciplinas-a (respostas).csv'
arquivo2  <<- './avaliacao2018-2/dados-avaliacao2018-2/Formulario-Avaliacao-BSI-2018-2__Disciplinas-a (respostas).csv'

library('plyr')

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
      arquivo_disciplinas  <<- arquivo1
    else
      arquivo_disciplinas  <<- arquivo2
    
  enc1= 'LATIN1' #'UTF-8' 
  dados_disciplinas  <<- read.csv2(arquivo_disciplinas, header=T, encoding = enc1)
  names(dados_disciplinas) <<- header

  #q4-q16
  levels1 <<- c('MUITO BOM', 'BOM', 'REGULAR','RUIM', 'PÉSSIMO')
  levels2 <<- c('Sim', 'Não')
  
  professores  <<- levels(dados_disciplinas$Professor)
  disciplinas  <<- levels(dados_disciplinas$Disciplina)
  
  nprofessores <<- nrow(table(professores))
  ndisciplinas <<- nrow(table(disciplinas))
  
  gdisciplina  <<- c(4:7)#E-M 5:13 #
  gprofessor   <<- c(8:13)#
  gautoAvalia  <<- c(14:16)
  colunas      <<- c(gprofessor) #1..46
  
  dadosProf <<- dados_disciplinas[,c(3,gprofessor)]
  for(i in 2:7){
    dadosProf[,i] <<- revalue(dadosProf[,i],c('MUITO BOM'='Sim', 'BOM'='Sim', 'REGULAR'='Sim','RUIM'='Não', 'PÉSSIMO'='Não'))
  }
  
}

dadosPorProfessorDisciplina <- function(dados,nomeProfessor,nomeDisciplina){
  f1 <- subset(dados,Professor==nomeProfessor)
  f2 <- subset(f1,Disciplina==nomeDisciplina)
  return(f2)
}

escreve <- function(texto){
  cat(texto,sep=" ")
}

#' ## Lista de Professores
#+ info, echo=FALSE, comment=NA 
info  <- function(fonte){
  escreve(c("\nProfessor: " ,levels(factor(fonte$Professor)),'\n'))  
  escreve(c("No de Respostas: ",nrow(fonte),'\n'))
}



configura_header()
configura_geral(1) #1grafico #2prompt 

#'  \pagebreak
#+ loopx, comment=NA, echo=FALSE
resumo <- function(dt){
  print(summary(dadosTemp[,2:7]))
}

for(i in c(1:25)){
  dadosTemp <- subset(dadosProf,Professor==professores[i])
  info(dadosTemp)
  resumo(dadosTemp)  
  
}  

#Arquivo "Avalicacao-professor-para-progressao__20xx-x.docx" apresenta relatório 
#para ser utilizado na avaliação de progressão dos professores. 