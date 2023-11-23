# TUR5019.1NRS

# Atvidade 1

# Banco de dados em análise: https://doi.org/10.5281/zenodo.4734098

# Após a instalação do git e do git hub, foram realizados os seguintes passos:

# Instação do "rmarckdown"
install.packages("rmarkdown")

# Ativação do pacote rmarkdown
library("rmarkdown")

# Passo a passo - TUR5019

# Ativação do pacote para ler excel

library(readxl)

# No GitHub online, fiz o upload do banco de dados (bd.xlsx) que será analisado e o carrguei como objeto no R

bd <- read_excel("bd.xlsx")

# Verificando a importação

glimpse(bd)

# Seleção das variáveis
# Categóricas:local (GEO) e se possuem documentos estratégicos de turismo cultural a nível local (1=sim; 0=não) (Cult_strat_doc)
# Numéricas: qtd sítio de patrimônio mundial UNESCO (World_heri_sites) e qtd de patrmônio imaterial na lista da UNESCO (Intangi_UNESCO_heri)

table(bd$GEO)
table(bd$Cult_strat_doc)
table(bd$World_heri_sites)
table(bd$Intangi_UNESCO_heri)

# Usando o data.table para eliminar as variáveis não selecionadas do banco de dados

bd <- bd[bd$Year %in% c(2017, 2018, 2019), c("GEO", 'Year', "World_heri_sites", "Intangi_UNESCO_heri", "Cult_strat_doc")]
view(bd)

# Analisando dados faltantes (df)

df <- colSums(is.na(bd))

# Excluindo dados faltantes
bd_sem_faltantes <- bd[complete.cases(bd), ]


# Frequência absoluta de GEO (fageo)
fageo <- table(bd$GEO)
print(fageo)
# OBS:o resultado mostra 35 localidades, cada qual aparecendo três vezes, pois se referem aos anos de 2017, 2018 e 2019.

# Frequência absoluta doc estratégico (facsd)
facsd <- table(bd$Cult_strat_doc)
print(facsd)
# OBS: o resultado mostra 59 respostas "sim" sobre o local ter documentos estratégicos de turismo cultural, isso ao londo dos três anos, ou seja, a resposta pode ter variado na mesma localidade ao longo dos anos.  

# Criando tabela de frequência relativa do GEO
prop.table(table(bd$GEO))

# Criando tabela de frequência relativa de Cult_strat_doc
prop.table(table(bd$Cult_strat_doc))

# Criando tabela cruzada com as variáveis categóricas
trc <- table(bd$GEO, bd$Cult_strat_doc)

# Gráfico de barras para a variável categórica GEO
barplot(prop.table(table(bd$GEO)), main = "Gráfico de Barras para frequência relativa GEO", xlab = "GEO", ylab = "Frequência")
# OBS: todas as localidades foram analisadas em três anos, por isso o gráfco aparece com todas as barras do memso tamanho.

# Gráfico de barras para a variável categórica Cult_strat_doc
barplot(prop.table(table(bd$Cult_strat_doc)), main = "Gráfico de Barras para frequência relativa docs", xlab = "0 = não 1 = sim", ylab = "Frequência")
# OBS: a maior parte das localidades possuem documentos estratégicos para cultura.

# Calculando as principais medidas de resumo para as variáveis numéricas
# Sítio de patrimônio mundial UNESCO

summary(bd$World_heri_sites)

# qtd de patrmônio imaterial na lista da UNESCO
summary(bd$Intangi_UNESCO_heri)

# Histograma para Sítio de patrimônio mundial UNESCO
hist(bd$World_heri_sites)

# Histograma para patrmônio imaterial na lista da UNESCO
hist(bd$Intangi_UNESCO_heri)

# Gráfico de dispersão para patrmônio imaterial na lista da UNESCO
plot(bd$Intangi_UNESCO_heri, bd$Year)

# Gráfico de dispersão para Sítio de patrimônio mundial UNESCO
plot(bd$World_heri_sites,bd$Year)

# Correlação
cor(bd$World_heri_sites,bd$World_heri_sites)

