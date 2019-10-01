# Roteiro - VERSAO 201801a

## Instação
* instalar R e RStudio. Instalar os pacotes necessário ao rodar o script

# Dados
* Baixar dados do google!drive (formato excel)
* Abrir no excel e remover linhas e colunas em branco, bem como verificar se há dados a serem corrigidos. Substituir ";" por outro caracter.
* exportar para CSV (SALVAR COMO 'CSV' - separados por virgulas)

# Configurar ambiente
* abrir rstudio
* abrir projeto em (D:\dados\desenvolvimento\avaliacao-cinfo__R\ifal.rproject)
* Limpar Workspace (caso necessário)

# Perfil do aluno
* abrir arquivo perfil, atualizar "renda familiar", caso necessário
* atualizar fonte de dados no arquivo, bem como demais campos necessários.
* rodar script perfil
* renomear arquivo. padrão: Avaliacao-20xx-x__perfil--infraestrutura.docx

# Relatório progressão
* abrir arquivo professores__avalicacao-progressao.R
* atualizar fonte de dados no arquivo, bem como demais campos necessários.
* rodar relatorio de professores para progressão.
* renomear arquivo. padrão: Avalicacao-professor-para-progressao__20xx-x.docx
* gerar PDF

# Relatório Professores/Disciplinas
* abrir arquivo professores__avaliacao-cinfo.R
* atualizar fonte de dados no arquivo, bem como demais campos necessários.
* rodar script. ver opções no final do script!
* renomear arquivos.
* Padroes:
	* avaliacao-geral-20xx-x---com-comentarios
	* avaliacao-geral-20xx-x---sem-comentarios
	* Avaliacao-20xx-x__RESUMO_RESPOSTAS.docx
	* Avaliacao-20xx-x__RESUMO_AVALIACOES.docx
	* pasta professores-disciplina-20xx-x

# Outros
* abrir arquivo disciplinas, "renda familiar", caso necessário, e executar

# To do
* melhorar COMENTARIOS POR DISCIPLINA (não escrever texto vazio!!!)
