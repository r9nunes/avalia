# Roteiro - VERSAO 201801a

## Insta��o
* instalar R e RStudio. Instalar os pacotes necess�rio ao rodar o script

# Dados
* Baixar dados do google!drive (formato excel)
* Abrir no excel e remover linhas e colunas em branco, bem como verificar se h� dados a serem corrigidos. Substituir ";" por outro caracter.
* exportar para CSV (SALVAR COMO 'CSV' - separados por virgulas)

# Configurar ambiente
* abrir rstudio
* abrir projeto em (D:\dados\desenvolvimento\avaliacao-cinfo__R\ifal.rproject)
* Limpar Workspace (caso necess�rio)

# Perfil do aluno
* abrir arquivo perfil, atualizar "renda familiar", caso necess�rio
* atualizar fonte de dados no arquivo, bem como demais campos necess�rios.
* rodar script perfil
* renomear arquivo. padr�o: Avaliacao-20xx-x__perfil--infraestrutura.docx

# Relat�rio progress�o
* abrir arquivo professores__avalicacao-progressao.R
* atualizar fonte de dados no arquivo, bem como demais campos necess�rios.
* rodar relatorio de professores para progress�o.
* renomear arquivo. padr�o: Avalicacao-professor-para-progressao__20xx-x.docx
* gerar PDF

# Relat�rio Professores/Disciplinas
* abrir arquivo professores__avaliacao-cinfo.R
* atualizar fonte de dados no arquivo, bem como demais campos necess�rios.
* rodar script. ver op��es no final do script!
* renomear arquivos.
* Padroes:
	* avaliacao-geral-20xx-x---com-comentarios
	* avaliacao-geral-20xx-x---sem-comentarios
	* Avaliacao-20xx-x__RESUMO_RESPOSTAS.docx
	* Avaliacao-20xx-x__RESUMO_AVALIACOES.docx
	* pasta professores-disciplina-20xx-x

# Outros
* abrir arquivo disciplinas, "renda familiar", caso necess�rio, e executar

# To do
* melhorar COMENTARIOS POR DISCIPLINA (n�o escrever texto vazio!!!)
