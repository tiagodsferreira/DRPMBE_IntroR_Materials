# AGENDA |  Medidas de associação entre variáveis e GLM  
# 
# 1. Associação entre fatores    
# 2. Associação entre variáveis contínuas 
# 3. Comparações de médias 
# 4. O "General Linear Model" (GLM)
# 5. Análise de regressão
# 6. Análise de variância  

##########################################################
##########################################################
##########################################################
# 1. Associação entre fatores

## Tabelas de contingência
# As tabelas de contingências permitem sumariar frequências relativas entre categorias de duas ou mais variáveis categoriais (ou fatores), possibilitando uma análise da associação entre essas variáveis. No R, as tabelas de contingências são simples extensões das tabelas de frequências, sendo criadas pelo mesmo grupo de funções (i.e., **table** e **xtabs**).  

### A função **table()** para tabelas de contingência
# two way table using table()
mytable1 <- with(coffee, table(coffee_rec, tea_rec))
mytable1  # frequencies
prop.table(mytable1) # proportions
prop.table(mytable1)*100 # percentages

### A função **xtabs()** para tabelas de contigências   
# Para criar uma tabela de contingência usando a função **xtabs** a, as variáveis a serem "cruzadas" devem surgir do lado direito da fórmula (**~**).

#1. Creates table
mytable2 <- xtabs(num_reviews ~ coffee_rec + tea_rec, data = coffee)
#2. Presents table
mytable2 # frequencies
#3. Displays row sums
margin.table(mytable2, 1)
#4. Displays column sums
margin.table(mytable2, 2)
#5 Adds row and column sums to the table
addmargins(mytable2) 
#6 cell proportions
prop.table(mytable2)
#7 row proportions
prop.table(mytable2, 1) 
#8 column proportions
prop.table(mytable2, 2) 
#9 cell and column proportion with row and column margins
addmargins(prop.table(mytable2)) 


# Se na formula de xtabs() uma outra variável é incluída no lado esquerdo da formula (~), essa variável é tratada como um vetor com frequências a incluir na tabela de contingências.  
paste("The total number of reviews is", sum(coffee$num_reviews), sep=": ")
mytable3 <- xtabs(num_reviews ~ coffee_rec + tea_rec, data = coffee)
mytable3
sum(mytable3)


### Tabelas de contigências com 3 ou mais variáveis

#1. Creates a three-way table
mytable4 <- xtabs(~ coffee_rec + internet + cityarea, data=coffee)
#2. Presents table
mytable4
#3. Prints table in compact format
ftable(mytable4) 
#4 Table percentage (2 decimals) of coffee categories for internet and city area with marginal sums 
round(ftable(addmargins(prop.table(mytable4)))*100, 2)

## Visualização de contingências
### Gráficos de barras
mytable_5 <- table(coffee$convenience_rec, coffee$seating)
mytable_5

# Stacked barplot
library(RColorBrewer)

par(mar=c(3,2,3,8))
barplot(mytable_5, 
        main="Bar Plot for coffee convenience stacked by available seating",
        xlab="Seating", ylab="Frequency",
        xlim=c(0, ncol(mytable_5)+1),
        col= brewer.pal(nrow(mytable_5),  "BrBG"),            
        legend.text=rownames(mytable_5),
        args.legend=list(x=ncol(mytable_5) + 1.3, y=max(colSums(mytable_5)), bty = "n"))
dev.off()

# grouped barplot                       
barplot(mytable_5, 
        main="Bar Plot for coffee convenience grouped by available seating",
        xlab="Seating", ylab="Frequency",
        xlim=c(0, nrow(mytable_5)*2 + 4),
        col= brewer.pal(nrow(mytable_5),  "BrBG"),            
        legend.text=rownames(mytable_5), beside=TRUE)

### Spinograms
if (!require("vcd")) install.packages("vcd")
library(vcd)

mytable_6 <- table(coffee$convenience_rec, coffee$cityarea)
spine(mytable_6, main="Spinogram crossing coffee convenience and city area")
#?spine

### Ainda mais gráficos...
library("gplots")
# 1. convert the data as a table
mytable_7 <- xtabs(~ popular + internet, data=coffee)

(DF_table7 <- as.table(as.matrix(mytable_7)))
# 2. Graph
?balloonplot
balloonplot(t(DF_table7), main ="Internet by popular", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

mytable_7
barplot(mytable_7, beside = TRUE)
mosaic(mytable_7) # Plots (extended) mosaic displays.

# O pacote vcd apresenta algumas funções úteis para a vizualização de associação entre variáveis categoriais.

library("vcd")
cotabplot(mytable_7) # cotabplot is a generic function for creating trellis-like coplots (conditional plots) for contingency tables.

doubledecker(mytable_7, data = coffee) # This function creates a doubledecker plot visualizing a classification rule

assoc(mytable_7)  # Produce an association plot indicating deviations from a specified independence model in a possibly high-dimensional contingency table.

cd_plot(coffee$cityarea~coffee$rating) # Computes and plots conditional densities describing how the distribution of a categorical variable y changes over a numerical variable x.


## Testes à independência entre fatores  
# O R disponibiliza vários testes para avaliar a independência de variáveis categoriais.

### Chi-square test
# O Chi-square test permite verificar a independência entre as observações das colunas e linhas de uma tabela de contingências. 

browseURL("G:\\My Drive\\FPCEUP\\R trainning\\GitRepo\\DRPMBE_R-Intro\\Tutorials\\Figures\\chi_square.JPG")
browseURL("G:\\My Drive\\FPCEUP\\R trainning\\GitRepo\\DRPMBE_R-Intro\\Tutorials\\Figures\\chi_square_MODEL.JPG")

# step by step
mytable_8 <- xtabs(~ popular + internet, data=coffee)
mat <- matrix(as.numeric(mytable_8), ncol=2)
mytable_8_marg <- addmargins(mytable_8) 
mat_marg <- matrix(as.numeric(mytable_8_marg), ncol=3)

# cálculo das frequências esperadas
Eij <- c((mat_marg[1,3]*mat_marg[3,1])/mat_marg[3,3],
         (mat_marg[2,3]*mat_marg[3,1])/mat_marg[3,3],
         (mat_marg[1,3]*mat_marg[3,2])/mat_marg[3,3],
         (mat_marg[2,3]*mat_marg[3,2])/mat_marg[3,3])

Eij <- matrix(Eij, nrow=2)
Eij

# Cálculo chi-quadrado
(chi <- sum(((mat - Eij )^2)/Eij))

# DF
(df <- (nrow(mat)-1)*(ncol(mat)-1))

# p-value
pchisq(chi, df=df, lower.tail=FALSE)

chisq.test(mytable_8, correct=FALSE)
chisq.test(mytable_8, correct=TRUE) # Yates’s correction applies to 2 × 2 contingency table to avoid Type I error - subtracts .5 to the differences between observed and estimated model

mytable_9 <- xtabs(~ tea_rec + cityarea, data=coffee)
mytable_9
chisq.test(mytable_9)

mytable_10 <- xtabs(~ coffee_rec + cityarea, data=coffee)
mytable_10
chisq.test(mytable_10)

#?chisq.test


#### Chi-square test: Pressupostos  
# Independencia das observações - cada sujeito apenas pode contribuir para uma observação (não utilizar com medidas repetidas); 
# As frequências esperadas para cada célula deverão ser maiores que 5, sob pena de se perder poder estatístico para a deteção de efeitos. No caso deste pressuposto ser violado, deve ser utilizado o Fisher's Exact Test

### Fisher's Exact Test
# O teste de chi-square apresenta uma limitação importante: a sua distribuição amostral é apenas aproximada a uma distribuição de chi-square. Esta aproximação é bastante exata quando lidamos com amostras de dimensão significativa, mas pode não ser suficiente quando lidamos com amostras de menores dimensões. Nestes casos a distribuição da amostra pode ser demasiado distinta da distribuição teórica chi-square. Assim, geralmente considera-se que quando a frequência esperada para cada célula é reduzida (menor que 5) o teste apresenta uma probabilidade significativa de erro do tipo II, o que significa que podemos não rejeitar a H0 (não existem diferenças entre frequências esperadas e observadas) quando na verdade o devemos fazer.
 
# O Fisher’s exact test pode ser utilizado nestas circunstâncias (com amostras de tamanho mais reduzido) para colmatar as limitação do chi-square test. No entanto, este teste apresenta problemas de eficiência com amostras de grandes dimensões, sendo especialmente utilizado em tabelas 2 x 2.

mytable_11 <- xtabs(~ popular + internet, data=coffee)
mytable_11
chisq.test(mytable_11)
fisher.test(mytable_11)

mytable_12 <- xtabs(~ tea_rec + cityarea, data=coffee)
mytable_12
chisq.test(mytable_12)
fisher.test(mytable_12)

mytable_13 <- xtabs(~ coffee_rec + cityarea, data=coffee)
mytable_13
chisq.test(mytable_13)
fisher.test(mytable_13)
# ?fisher.test


### A função **CrossTable**
# A função gmodels::CrossTable permite testar a independência entre variáveis categoriais de uma forma simplificada.

if (!require("gmodels")) install.packages("gmodels")
library(gmodels)
CrossTable(coffee$coffee_rec, coffee$cityarea)

CrossTable(coffee$coffee_rec, coffee$cityarea, fisher = TRUE, chisq = TRUE, expected = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, sresid = TRUE, format = "SPSS")
# OR
CrossTable(mytable_12, fisher = TRUE, chisq = TRUE, expected = TRUE, prop.c = FALSE,
           prop.t = FALSE, prop.chisq = FALSE, sresid = TRUE, format = "SPSS")


#### Argumentos de **CrossTable**

FUN_arg <- c("digits", "mcnemar", "prop.c", "prop.t",  "prop.chisq", "resid", "sresid", "asresid", "Format")

Arg_desc <- c("number of digits after the decimal point", 
              "TRUE: produce the results of McNemar’s test for testing differences between two related groups when nominal data have been collected. This test can be applied to compare two related dichotomous variables", 
              "FALSE: column proportions will not be displayed", 
              "FALSE: total proportions will not be displayed", 
              "FALSE: Chi-square proportions will not be displayed", 
              "TRUE: Produces Pearson residuals",
              "TRUE: Produces standardized residuals", 
              "TRUE: Produces adjusted standardized residuals", 
              "SAS/SPSS: Mimic SAS (default) or SPSS output. To see residuals you need to set the format to SPSS")

tables <- data.frame(FUN_arg, Arg_desc)
View(tables)


### Tamanho do efeito da associação entre variáveis categoriais
# Anteriormente, abordamos testes como o chi-square que permitem avaliar a independência de variáveis categoriais. Estes testes fornecem evidência para rejeitar ou não a h0 da independência das variáveis. Após verificação da existência de diferenças, torna-se importante verificar a magnitude dessas diferenças. Esta magnitude pode ser analisada com recurso a distintos indicadores, nomeadamente o phi coefficient (para tabelas 2x2), contingency coefficient o o Cramer’s V. Em geral, valores mais elevados nestes coeficientes são indicativos de associações mais fortes entre as variáveis. Valores próximos de 0 indicam baixa magnitude de associação.

# Measures of association for a two-way table
if (!require("vcd")) install.packages("vcd")
library(vcd)
mytable_13 <- xtabs(~coffee_rec+cityarea, data=coffee)
mytable_13
assocstats(mytable_13)
# ?assocstats


##########################################################
##########################################################
##########################################################
# 2. Associação entre variáveis contínuas  

# Existem dois tipos gerais de correlações:   
# > Correlações bivariadas;   
# > Correlações parciais.  

# Dentro da categoria dos coeficientes de correlação bivariada existem testes paramétricos (Pearson) e não paramétricos (Spearman e Kendal)

## Breve nota sobre pressupostos de teste paramétricos 
# Neste ponto iremos abordar alguns dos pressupostos básicos subjacentes a vasto conjunto de testes, habitualmente denominados de testes paramétricos. No entanto, é importante salientar que certos testes tem subjacentes pressupostos adicionais que é importante considerar. A maioria do procedimentos estatísticos que serão apresentados neste curso são testes paramétricos baseados na distribuição normal.

browseURL("G:\\My Drive\\FPCEUP\\R trainning\\GitRepo\\DRPMBE_R-Intro\\Tutorials\\Figures\\normar distribution.JPG")


# Um teste paramétrico tem subjacente 4 pressupostos básicos:   
# > Os dados apresentam uma distribuição normal
# > As variâncias devem ser homogéneas quando existem multiplos grupos
# > Os dados devem ser intervalares ou contínuos
# > As observações devem ser independentes, isto é os dados dos diferentes participantes deve ser independente.

# A não observação destes pressupostos poderá inviabilizar a correta aplicação de um teste paramétrico. Os pressupostos da continuidade dos dados e independência das observações são avaliados facilmente com base no senso comum. Assim, de seguida, focam-se com mais detalhe alguns procedimentos que permitem avaliar em que medida os pressupostos da normalidade e da homogeneidade das variâncias estão cumpridos.

hist(rnorm(1000, mean = 0, sd = 1), main="Histogram for a normal distribution")
hist(rpois(1000, lambda = 4))


### Normalidade  
# A normalidade de uma distribuição pode ser avaliada de 3 formas distintas e complementares:   
# > através de representações gráficas  
# > através de estatística descritiva  
# > através de estatística inferencial  

#### Histogramas e Q-Q plots
#1. Criar um histograma para a variável rating
hist(coffee$rating,  
     col="darkgreen",
     breaks=10, 
     xlab="Coffee ratings", 
     main="Coffee global ratings in Austin")

#2. Criar um gráfico de densidade para a variável rating
d <- density(coffee$rating) # returns the density data  
plot(d,  main="Coffee global ratings in Austin", col="darkblue", lwd=4) 
polygon(d, col="lightblue", border="darkblue") 
rug(coffee$rating, col="darkorange") 

#3. Criar um histograma para a variável rating adicionando a curva de densidade a esse histograma;
hist(coffee$rating, 
     freq=FALSE, 
     breaks=10, 
     col="darkorange", 
     xlab="Coffee global ratings", 
     main="Coffee global ratings in Austin") 
lines(density(coffee$rating), col="darkblue", lwd=4)

#4. Criar um histograma para a variável rating adicionando a curva normal a esse histograma
x <- coffee$rating
h <- hist(x, 
          breaks=10, 
          col="darkorange", 
          xlab="Coffee global ratings", 
          main="Coffee global ratings in Austin") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="darkblue", lwd=4)
box()



# O Q-Q plot (Quantile-Quantile plot) é um outro gráfico útil na avaliação da normalidade dos dados.  
# NOTA: Um quantil é a proporção de casos que podem ser encontrados abaixo de um determinado valor.
# O Q-Q plot permite comparar os quartis da distribuição observada com os quartis de uma distribuição normal. Se os dados observados seguirem uma observação normal, os seus valores irão ser semelhantes aos valores de uma distribuição normal, situando-se sobre a linha diagonal do gráfico.  

# Normal Q-Q Plot
qqnorm(coffee$rating, main = "Normal Q-Q Plot for coffee ratings",
       col="darkorange", pch=19)
qqline(coffee$rating, col = "steelblue", lwd = 2)
?points
?qqnorm

qqnorm(scale(coffee$rating), main = "Normal Q-Q Plot for coffee ratings",
       col="darkorange", pch=19)
qqline(scale(coffee$rating), col = "steelblue", lwd = 2)


# Normal Q-Q Plot using car::qqPlot
if (!require("car")) install.packages("car")
library(car)

qqPlot(scale(coffee$rating), 
       main = "Normal Q-Q Plot for coffee ratings", col="darkgrey", pch=19, col.lines="darkred")
# ?qqPlot


#### Assimetria e curtose
skew <- function(x){
  sum((x-mean(x))^3/sd(x)^3)/length(x)
}
skew(coffee$rating)

kurt <- function(x){
  sum((x-mean(x))^4/sd(x)^4)/length(x) - 3
}
kurt(coffee$rating)

library(psych)
psych::describe(coffee$rating)


#### Shapiro–Wilk test
shapiro.test(coffee$rating) 

# O teste Shapiro-Wilk compara os valores de uma distribuição observada com os valores de uma observação normal com a mesma média e desvio padrão. O h0 do teste Shapiro-Wilk é de que distribuição da amostra é proveniente de uma distribuição normal.
# Assim, se resultado do teste é não significativo (p > .05) indica que a distribuição não é significativamente diferente de uma distribuição normal.

# NOTA: Uma das limitações do teste Shapiro-Wilk é a forte propensão para erro do tipo I em amostras de maiores dimensões. Isto é, há uma grande probabilidade de rejeitar a h0 em amostras grandes, levando a assumir que a mostra tem uma distribuição significativamente diferente da distribuição normal, quando não verdade o desvio face à normalidade poderá ser reduzido.
  
library(pastecs)
stat.desc(coffee$rating, basic=FALSE, desc=TRUE, norm=TRUE)
?stat.desc

### Homegeneidade das variâncias
plot(coffee$rating ~ coffee$cityarea)
points(coffee$rating ~ coffee$cityarea)
  
# O pressuposto da homogeneidade das variâncias significa que a variância de uma determinada variável não deve ser diferente em distintos níveis de uma outra variável, mantendo-se estável nos diferentes níveis. 

# O Levene’s test avalia este pressuposto, testando a h0 de que as variâncias nos diferentes grupos são iguais. Assim perante valores de  p ≤ .05 indicam que a h0 pode ser rejeitada, concluindo-se que as variâncias são significativamente diferentes entre os diferentes grupos. Resultados não significativos no Levene’s test indicam que as variâncias não são significativamente diferentes, podendo-se assumir a homogeneidade das variáveis.  

library(car)
# leveneTest(outcome variable, group, center = median/mean)
leveneTest(coffee$rating, coffee$cityarea)

## Correlações Pearson, Spearman e Kendall  
### As funções **cor()** e **cor.test()**    

Argument <- c("x", "y", "use", "method")
Description <- c("numeric variable or dataframe", 
                 "numeric variable (NULL if x is a matrix or dataframe)",
                 "string: all.obs (the presence of missing observations will produce an error NA); everything (NA whenever a observations is NA); complete.obs (listwise delection); pairwise.complete.obs (pairwise delection)", 
                 "string: pearson (default); spearman; kendall")

cor.table <- data.frame(Argument, Description)
View(cor.table)



names(coffee)
coffee_num <- coffee[c(3,5,10,12,14,16,18,20,22)]
sapply(coffee_num, class)

cov(coffee_num) # variances and covariance
cor(coffee_num) # Person correlation
cor(coffee_num, method="spearman") # Spearman correlation
cor(coffee_num, method="kendall") # Kendall correlation

names(coffee_num)
coffee_num1 <- coffee_num [1:2]
cor(coffee_num, coffee_num1)

cor.test(coffee$rating, coffee$num_reviews) # correlation with significance test
cor.test(coffee$rating, coffee$num_reviews, method="kendall")
# ?cor
# ?cor.test


### As funções psych::corr.test e Hmisc:rcorr
library(psych)
corr.test(coffee_num)
# ?corr.test


library(Hmisc)
rcorr(as.matrix(coffee_num))
# ?rcorr


browseURL("http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software")

cor_mat <- rcorr(as.matrix(coffee_num))
class(cor_mat)
str(cor_mat)
names(cor_mat)

#defining flattenCorrMatrix function
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor = (cormat)[ut],
    p = (pmat)[ut]
  )
}

# Extract the correlation coefficients
cor_mat$r
# Extract p-values
cor_mat$P

cor_mat2 <- flattenCorrMatrix(cor_mat$r, cor_mat$P)
cor_mat2[,c(-1,-2)] <- round(cor_mat2[,c(-1,-2)],3) #to round only the numeric variables
cor_mat2



browseURL("http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package")

cor_mat <-round(cor(coffee_num),2)

# Lower and upper triangular part of a correlation matrix
lower.tri(cor_mat, diag = FALSE)
upper.tri(cor_mat, diag = FALSE)

upper.tri(cor_mat)

# Hide upper triangle
upper <- cor_mat
upper
upper[upper.tri(cor_mat)] <- ""
upper
upper <- as.data.frame(upper)
upper

#Hide lower triangle
lower <- cor_mat
lower
lower[lower.tri(cor_mat, diag=TRUE)] <- ""
lower
lower <- as.data.frame(lower)
lower




# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 



if (!require("xtable")) install.packages("xtable")
library(xtable)
# detach("package:ggm", unload=TRUE)

print(xtable(upper), type="html")
corstars(coffee_num, result="html")

tab <- corstars(coffee_num, result="html")
write(tab, file="tab.html")


## Visualização da associação entre variáveis contínuas
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)
library(Hmisc)

corrplot(cor(coffee_num), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

cor_mat <- Hmisc::rcorr(as.matrix(coffee_num))
# Insignificant correlation are crossed
corrplot(cor_mat$r, type="upper", order="hclust", 
         p.mat = cor_mat$P, sig.level = 0.01, insig = "label_sig")


# Insignificant correlations are leaved blank
corrplot(cor_mat$r, type="upper", order="hclust", 
         p.mat = cor_mat$P, sig.level = 0.01, insig = "blank")
#?corrplot


plot(coffee_num)
if (!require("PerformanceAnalytics")) install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(coffee_num, histogram=TRUE, pch=19)
#?chart.Correlation


## Correlação parcial
# A correlação parcial consiste numa correlação entre duas variáveis quantitativas, controlando um ou mais variáveis quantitativas. No R a correlação parcial pode ser calculada através da função ggm:pcor. Esta função incluí os seguintes argumentos:

# pcor(u, # vector of numbers: the first two numbers are the indices of the correlated variables; the remaining values are the indices of conditioning variables)
#      S) # covariance matrix among the variables.


if (!require("ggm")) install.packages("ggm")
library(ggm)
names(coffee_num)
cor(coffee_num$rating, coffee_num$coffee)
pcor(c(2, 3, 4:9), cov(coffee_num))
#?pcor


# A significância da correlação parcial pode ser avaliada através da função psych::pcor.test. 
pcor.test(r, # result of pcor()
          q, # number of variables being controlled
          n) # sample size

library(psych)
pcor.test(pcor(c(2, 3, 4:9), cov(coffee_num)), q= 6, n = nrow(coffee_num))
nrow(coffee_num)



##########################################################
##########################################################
##########################################################
# 3. Comparações de médias   

## t-test  
# O t-test permite comparar dois grupos quando a variável de interesse (outcome) é contínua e com uma distribuição normal.   

### t-test para amostras independentes
# O t-test para amostras independentes é utilizado para avaliar a hipótese de que as médias de duas populações são iguais (h0), assumindo que os dois grupos são independentes e que os dados são retirados de uma população normal.
# A função genérica no R para o t test é:  
  
# t.test(y ~ x, data) 
# y é uma variável numérica e x é uma variável dicotómica; data corresponde a uma matriz ou DF.


# O t-test para amostras independentes é utilizado para avaliar a hipótese de que as médias de duas populações são iguais (h0), assumindo que os dois grupos são independentes e que os dados são retirados de uma população normal.
# A função genérica no R para o t test é:  
 
#   Por defeito, e ao contrário da maioria das pacotes estatísticos, esta função assume a heterogeneidade das variâncias, aplicando welsh’s t-test. Este teste é uma adaptação do Student’s t-test, sendo robusto a amostras com diferentes variâncias.  
# O argumento var.equal=TRUE pode ser acrescentado à função para aplicar o Student’s t-test, assumindo-se a homogeneidade das variâncias.
  
# Por defeito a hipótese alternativa é two tailed. Este comportamento também pode ser alterado com o argumento alternative=”less” ou alternative=”greater”

# A função anterior pode ser utilizada como vetores numéricos para os outcomes medidos nos dois grupos:  

t.test(y1, y2 , data) 
# Neste caso y1 e y2 são o outcome medidos nos dois grupos

names(coffee)
str(coffee)

library(car)
leveneTest(coffee$rating, coffee$popular)
leveneTest(coffee$rating, as.factor(coffee$popular)) # equal variances can be assumed

t.test(rating ~ popular, data=coffee, var.equal=TRUE) # h0 (no differences) is retained


leveneTest(coffee$rating, as.factor(coffee$internet)) # equal variances can be assumed
t.test(rating ~ internet, data=coffee, var.equal=TRUE) # h0 (no differences) is rejected


### t-test para amostras dependentes  
# O t-test para amostras dependentes aplica-se quando as observações nos dois grupos estão relacionados. Este teste deve ser aplicado quando as medidas são repetidas (e.g., estudos longitudinais com dois momentos; duas condições experimentais com os mesmos sujeitos no grupo de controlo e experimental). No exemplo seguinte, verificamos diferenças na média dos ratings dos cafés no período da manha (grupo 1) e no período da tarde (grupo 2) - assume-se que as mesma pessoas atribuem os ratings em diferentes momentos do dia. Verifica-se assim a não independências das observações, uma vez que as mesmas pessoas avaliam o mesmo café em momentos diferentes.

names(coffee)
cor.test(coffee$ratingMorning, coffee$ratingAfternoon)

library(reshape2)
coffee_rec <- melt(coffee[c(6,8)])
names(coffee_rec) <- c("Morn_After", "rating")
str(coffee_rec)

t.test(coffee_rec$rating ~ coffee_rec$Morn_After, paired = TRUE)

# OR
t.test(coffee$ratingMorning, coffee$ratingAfternoon, paired = TRUE)

# No exemplo anterior a diferença de médias não é suficiente para a rejeição da h0, de que as médias entre os grupos são iguais.


## Testes não paramétricos
### Wilcoxon rank sum test (ou Mann-Whitney U test) para amostras independentes   
# No R, a função genérica do Wilcoxon test é muito semelhante à função para o t-test:
wilcox.test(y ~ x, data) # y é uma variável numérica e x é uma variável dicotómica; data corresponde a uma matriz ou DF.
 
# Ou, em alternativa:  
wilcox.test(y1, y2 , data) # y1 e y2 são o outcome medidos nos dois grupos

names(coffee)
hist(coffee$num_reviews)

wilcox.test(coffee$num_reviews ~ coffee$internet)

hist(jitter(coffee$num_review, factor = .000001))
cor(coffee$num_reviews, jitter(coffee$num_review, factor = .000001))
wilcox.test(jitter(coffee$num_review, factor = .000001) ~ coffee$internet)


library(psych)
psych::describe(coffee$num_reviews) # wilcox test may incorrectly reject the null hypothesis (Type I error) when the sampled is excessively skewed

hist(coffee$num_reviews)

psych::describe(log(coffee$num_reviews[is.finite(log(coffee$num_reviews))==TRUE]), omit=TRUE) # log tranformation may be usefull

hist(log(coffee$num_reviews))
log(coffee$num_reviews)
cor(coffee$num_reviews[-67], log(coffee$num_reviews)[-67])

wilcox.test(jitter(log(coffee$num_reviews), factor = .000001) ~ coffee$internet)


### Wilcoxon rank sum test (ou Mann-Whitney U test) para amostras dependentes   
# O Wilcoxon-Test para amostras dependentes aplica-se quando os grupos não são independentes e os pressupostos de normalidade estão violados. Para aplicar este teste apenas é necessário acrescentar o argumento **paired=TRUE** à função **wilcox.test()**. 

t.test(coffee$ratingMorning, coffee$ratingAfternoon, paired = TRUE)
wilcox.test(coffee$ratingMorning, coffee$ratingAfternoon, paired = TRUE)

# No exemplo anterior, os dois testes, paramétrico e não paramétrico, obtêm resultados semelhantes. Quando os pressuposto paramétricos são cumpridos é recomendada a utilização do t-test, uma vez que este teste é menos propenso a erro do tipo II (não rejeitando a h0 quando o deveria fazer – AKA falso negativo). Os testes não paramétricos devem estar reservados para situações em que os pressupostos de normalidade são manifestamente violados.

##########################################################
##########################################################
##########################################################
# 4. O "General Linear Model" (GLM)
# Regressão e análise de variância e covariância são tradicionalmente apresentados como procedimentos estatísticos distintos. Esta distinção é meramente artificial, uma vez que todos este procedimentos derivam de um modelo teórico mais amplo denominado de General Linear Model (GLM). A grande maioria dos pacotes estatísticos operacionaliza os procedimentos de regressões e análises de variância de forma distinta para conveniência do utilizador e não porque exista uma diferença matemática entre eles.
# O GLM permite predizer ou estimar uma variável (variável dependente ou resposta) através de uma ou mais variáveis (variáveis independentes, preditoras ou explicativas). O GLM assume a seguinte fórmula geral:
  
# Y = *(model)* + ε  

# Yi = β0 + β1X1i + β2X2i +...βkXki + ε; i = 1...n

# A variável dependende (Y) é então predita por um conjunto de coeficientes (β1, β2… βk) que representam o peso de uma determinada variável independente (X1, X2...Xk) sobre a variável dependente. O GLM inclui ainda duas componentes adicionais, o intercept (representado por  β0) e o erro (ou resíduo) (ε). O intercept da equação é uma constante matemática dependendo da escala das variáveis no modelo, enquanto que o erro (ou resíduo) consiste na diferença entre o valor observado na variável dependente para uma determinada observação e o valor da variável dependente estimado pelo modelo para essa mesma observação.

# Neste contexto matemático é possível incluir no GLM variáveis independentes (VI) contínuas, fatores, bem como transformações (se não lineares) de VI. Habitualmente, quando as variáveis do GLM são contínuas falamos de regressões. Quando são fatores, falamos de ANOVA ou ANCOVA, caso também sejam incluídas covariáveis (variáveis contínuas) no modelo. 

## Em síntese, os três parâmetros fundamentais do GLM são:  
# O intercept (β0) - que pode ser interpretado como o valor predito da VI quando todas as VD’s assumem o valor de 0  
# O(s) coeficiente(s) de regressão (β1, β2… βk) -  Por exemplo β1 X1 pode ser interpretado como o valor de incremento na (VD) por cada unidade de incremento de X1, controlando estatisticamente todas as restantes variáveis no modelo (i.e., fixando os seus valores)  
# O erro (ε) ou variância residual do modelo   

# No caso específico da ANOVA e ANCOVA os parâmetros preconizados no GLM são incluidos através de um procedimento de recodificação dos níveis dos fatores, habitualmente denominado de codificação dummy. 

#  Assim, para um **fator com 3 níveis**, o GLM adquire a seguinte formula:  
  
# Y = α + β1X1+ β2X2 + ε   

# > X1 consiste no código dummy para o primeiro nível do fator. A observação neste nível recebe o valor de 1 para X1 ou de 0 quando assim não é;  
# > X2 consiste no código dummy para o primeiro nível do fator. A observação neste nível recebe o valor de 1 para X2 ou de 0 quando assim não é;  
# > O terceiro nível do fator é apenas predito pelo parameter α, que em termos práticos representa a sua média.  

# *Assim o valor predito para as diferentes níveis do fator são preditos da seguinte forma:*   
  
# > Y3 = α    
# > Y2 = α + β2   
# > Y1 = α + β1  

# O teste de significância para a ANOVA avalia simultaneamente em que medida β1 = 0 e β2 = 0.  
# O método mais frequente de estimação dos parâmetros do GLM é através do critério dos quadrados mínimos – ordinary least squares (OLS) criterion. Os detalhes matemáticos para aplicação deste método estão para além do âmbito deste curso. Importa no entanto referir que a utilização deste critério permite estimar o GLM considerando a diferença mínima possível entre os valores observados e os valores preditos pelo modelo (i.e. o erro do modelo).

##########################################################
##########################################################
##########################################################
# 5. Análise de regressão  
# Em termos gerais, a análise de regressão é utilizada para avaliar a relação entre uma ou mais VI’s contínuas e uma variável dependente. A criação de um modelo de regressão é frequentemente um processo complexo e interativo que se desenvolve em vários passos.  
# Existem vários tipos específicos de regressão de tal forma que por vezes pode ser difícil perceber a que tipo de análise o termo “regressão” se refere. 


Tipo <- c("Simple linear", 
          "Polynomial",
          "Multiple linear",
          "Multilevel",
          "Multivariate",
          "Logistic",
          "Poisson",
          "Cox proportional hazards",
          "Time-series",
          "Nonlinear",
          "Nonparametric",
          "Robust")
Uso <- c("Predição de uma VD quantitativa por uma VI quantitiva", 
         "Predição de uma VD quantitativa por uma VI quantitiva, quando a relação é polinomial",
         "Predição de uma VD quantitativa por duas ou mais VI's",
         "Predição de uma VD em dados hierárquicos",
         "Predição de mais do que uma VD por uma ou mais VI's", 
         "Predição de uma VD categorrial por uma ou mais VI's",
         "Predição de uma VD de contagens por uma ou mais VI's",
         "Predição do tempo para um evento por uma ou mais VI's",
         "Modelação de time-series de dados com erros correlacionados",
         "Predição de mais do que uma VD por uma ou mais VI's, quando a relação não é linear", 
         "Predição de mais do que uma VD por uma ou mais VI's, quando a formas de dados não especificadas previamente",
         "Predição de mais do que uma VD por uma ou mais VI's, utilizado uma abordagem robusta a observações influentes") 

Regression <- data.frame(Tipo, Uso)
View(Regression)


# A grande quantidade de procedimentos de regressão e opções de análise disponíveis no R para estimação de modelos de regressão pode também suscitar alguma confusão. [Neste link poderá encontrar uma lista exaustiva de funções do R que permitem estimar modelos de regressão]

browseURL("https://cran.r-project.org/doc/contrib/Ricci-refcard-regression.pdf")

# No presente workshop a nossa incursão no mundo das regressão limita-se a apresentar e discutir a estimação do modelo de regressão linear simples. Este modelo permite avaliar a relação entre duas variáveis quantitativas. A correta estimação de modelos de regressão através do método OLS tem por base os seguintes pressupostos.
# 
# > **Normalidade** - A VD apresenta uma distribuição normal para valores fixos das VIs;  
# > **Independência** - Os valores observados da VD são independentes;  
# > **Linearidade** - A VD é uma função linear das VIs;  
# > **Homocedasticidade** - A variância das VD não varia nos diferentes níveis das VIs (i.e., existe uma variância constante para os diferentes valores das VI).

# A estimação de um modelo de regressão no R pode ser feita através da função **lm()**. 

# model <-  lm(formula, data) # The formula argument defines the model to be fitted; the argument data specifies the data used to fit the model

# A definição da fórmula do modelo segue a seguinte estrutura genérica:  
#  y ~ x1 + x2 + x3 + ... + xk # ~ separates the DV (y) from the IV (x1, x2, x3, ... xk); IV's are separated by +

# A fórmula do modelo permite utilizar um conjunto de outros símbolos com significado específico.  

símbolo <- c("~", 
             "+",
             ":",
             "*",
             "^",
             ".",
             "-",
             "-1",
             "I()",
             "function")
significado <- c("Separa VI's de VD's", 
                 "Separa VD's",
                 "Define interação entre VD's",
                 "Atalho para definir interações entre todas as VD's do modelo (y ~ x * z * w <==> y ~ x + z + w + x:z + x:w + z:w + x:z:w)",
                 "Define interação a um nível específico ((y ~ x * z * w)^2 <==> y ~ x + z + w + x:z + x:w + z:w)", 
                 "Define todas as variáveis da DF como VD's",
                 "Remove uma variável da equação ((y ~ x * z * w)^2 - x:w <==> y ~ x + z + w + x:z + z:w)",
                 "Retira o intercept da equação, forçando a linha de regressão a cruzar origem em x=0",
                 "Elementos dentro dos parentesis são interpretrados de forma aritmética (y ~ x + I(x^2) tem como VD's x e o quadrado de x",
                 "Funções matemáticas podem ser incorporadas no modelo (ex.: log()") 
regsimbol <- data.frame(símbolo, significado)
View(regsimbol)


# Após criação do modelo pela função lm(), existe um conjunto de funções disponibilizadas pelo R que podemos utilizar para clarificar diferentes aspetos dos modelo.

# A denominação de regressão linear simples é aplicada quando o modelo criado incluí uma VI e uma VD.

data(women)
str(women)
plot(weight ~ height, data = women)
abline(lm(weight ~ height, data = women))


# Para além da função **lm()** o R disponibiliza um conjunto de funções úteis quando produzimos estimamos modelos de regressão. Essas funções são aplicadas aos objetos produzidos pela função **lm()** permitindo retirar informação importante do modelo estimado


fun <- c("summary()", 
         "coefficients()",
         "confint()",
         "fitted()",
         "residuals()",
         "anova()",
         "vcov()",
         "AIC()",
         "plot()",
         "predict()")
operation <- c("apresenta resultados do modelo estimado", 
               "apresenta lista de coeficientes estimados",
               "apresenta intervalos de confiança para os parâmetros estimados",
               "apresenta lista com valores estimados do modelo",
               "apresenta lista com valores dos resíduos", 
               "apresenta tabela anova para modelo e permite compara dois ou mais modelos ajustados",
               "apresenta matriz de covariâncias para parâmetros do modelo",
               "Apresenta valor de AIC para modelo",
               "apresenta gráficos para diagnóstico e avaliação do modelo",
               "apresenta estimativas do modelo") 
lm.fun <- data.frame(fun, operation)
View(lm.fun)


## Modelos de regressão linear simples (preditor único)
names(coffee)
par(mfrow=c(2,2))

plot(rating ~ coffee, data = coffee)
abline(lm(rating ~ coffee, data = coffee))

plot(rating ~ food, data = coffee)
abline(lm(rating ~ food, data = coffee))

plot(rating ~ price, data = coffee)
abline(lm(rating ~ price, data = coffee))

plot(rating ~ service, data = coffee)
abline(lm(rating ~ service, data = coffee))

dev.off()



model1 <-  lm(rating ~ coffee, data = coffee)
summary(model1)
confint(model1)

plot(rating ~ coffee, data = coffee)
abline(lm(rating ~ coffee, data = coffee))

# O sumário do output da função lm() permite obter os coeficientes de regressão. O valor do intercept (3.9265) corresponde ao valor estimado do rating (VD), quando a pontuação atribuída pelos clientes ao café (VI) é 0. Já o valor do coeficiente para coffee significa que por cada unidade na pontuação do café (VD) é estimado que a pontuação geral do café (VI) suba cerca de meio ponto (.5518).

# Numa regressão (ou ANOVA) o modelo é estimado com base nos dados observados, permitindo predizer os resultados na população. Para estimar o modelo de regressão simples são calculados 3 indicadores essenciais: 
  
# > Total sums of squares (SStotal) (~variância)  
# > Residual sums of squares (SSresidual) (variância não explicada pelo model)  
# > Model sums of squares (SSmodel) (SStotal - SSresidual)  

# Estes indicadores servem de base ao cálculo de indicadores adicionais que nos ajudam a perceber a qualidade do nosso modelo, nomeadamente, a média dos quadrados, RSE, F-value e R2. Em termos gerais, SStotal,  a SSresidual e a Ssmodel são calculados com base nos quadrados dos resíduoes avaliam o quão bem o modelo descreve os nossos dados. 

### SStotal (SSt)
# Este indicador avalia em que medida a média (enquanto modelo mais simples que pode ser estimado) estima bem os dados observados de Y.
# > a fórmula do SSt é igual ao numerador da fórmula usada para calcular a variância.
# Este modelo mais simples é usado para comparar com o modelo produzido no procedimentos de OLS e verificar a sua pertinência na explicação de Y.


plot(rating ~ coffee, data = coffee)
abline(h=mean(coffee$rating))


### SSresidual (SSr) 
# Este indicador avalia em que medida o modelo de regressão estimado com os nossos preditores representa bem os dados observados. É calculada pela soma dos quadrados das diferenças entre os valores observados de y e os valores estimados de Y pelo modelo)

plot(rating ~ coffee, data = coffee)
abline(lm(rating ~ coffee, data = coffee))


### SSmodel (SSm)
# Este indicador compara em que medida o modelo de regressão estimado representa melhor dados observados relativamente ao modelo mais simples (a média). Na prática este indicador é a diferença entre SSt e SSr

plot(rating ~ coffee, data = coffee)
abline(h=mean(coffee$rating))
abline(lm(rating ~ coffee, data = coffee))


### Calculando SStotal, a SStotal e SSmodel passo a passo

# A função anova() permite obter os SSresidual e SSmodel automaticamente no R.

anova(model1) # To get the sums of squares and mean squares

# De seguida explicamos mais detalhadamente os procedimentos envolvidos no cálculo destes indicadores.

# SStotal
coffee$rating
mean(coffee$rating)
SSt <- sum((coffee$rating - mean(coffee$rating))^2) # numerador da fórmula da variância
SSt


# SSresidual
sum(residuals(model1)) # it is guaranteed by the least squares fitting procedure that the sum of the residuals is zero. 
sum(residuals(model1)^2) # mean squared error (MSE) is the sum of the square of the residuals

coffee$rating
fitted(model1)
SSr <- sum((coffee$rating - fitted(model1))^2) # Sum of the squared residuals
SSr

# OR
residuals(model1)
SSr <- sum(residuals(model1)^2)  
SSr

# SSmodel
SSm <- SSt - SSr
SSm


# A média destes indicadores é obtida com base nos graus de liberdade respetivos. 
# Calculate degrees of freedom (total, residual and model)
n <- length(coffee$coffee)
model1$coef
k <- length(model1$coef) # k = model parameter: b0, b1
(df.total <- n-1)
(df.residual <- n-k)
(df.model <- k-1)


# Calculate mean squares - these values are just variances
(ms.residual <- SSr/df.residual)
(ms.model<- SSm/df.model)

### Avaliação da qualidade do modelo  
# Como dito anteriormente, a estimação por OLS garante que a média dos resíduos é zero. Por isso, foi necessário anteriormente fazer a soma dos **quadrados** desses mesmos resíduos (SSr). Por conseguinte, para ter uma valor interpretável, faz mais sentido calcular a raiz quadrada da média dos quadrados dos resíduos. O R denomina este indicador de residual standard error (RSE). Assim, o valor do RSE pode ser calculado pela raiz quadrada da média dos quadrados dos resíduos (SSr). O RSE pode ser visto como o desvio padrão dos resíduos. Consiste numa estimativa do desvio ou erro médio presente na predição feita pelo modelo. Mais concretamente, o RSE é uma estimativa do desvio padrão do parâmetro ε do modelo. O RSE é vista como uma medida da qualidade do modelo, uma vez que avalia a distância média entre os valores preditos e os valores observados da VD.

# Calculate residual standard error
(RSE <- sqrt(ms.residual))

# No exemplo anterior, o valor observado do rating geral do café desvia-se em média cerca de .305 do valor estimado pelo nosso modelo. 

mean(coffee$rating)
RSE
RSE/mean(coffee$rating)

# Por outras palavras, considerando que a média dos ratings para todos os cafés é de cerca 4.23 e que o RSE é de .305, podemos dizer que a percentagem de erros da nossa estimativa é de cerca de 7%. Ainda no casos do nosso modelo, os graus de liberdade para cálculo do RSE são iguais ao n da nossa amostra (67) menos os parametros estimados pelo modelo (2), o intercept e a slope para a VD.

# O R quadrado também é um indicador do bom ajustamento do modelo. Ele expressa a quantidade de variância dos dados que é explicada pelo modelo. Neste contexto, a variância total é variação dos dados relativamente à média, o modelo mais simples que pode ser estimado.

# Multiple R-squared
summary(model1)$r.squared
# OR
SSm/SSt # the ratio of the SSmodel and the SStotal
# OR
cor(coffee$rating, fitted(model1))^2 # R2  is the squared correlation of the OLS prediction Y^ and the DV Y
# OR
var(fitted(model1))/var(coffee$rating) #R2  is also equal to the variance of Y^ divided by the variance of Y - variance accounted for by the predictors.

# O valor de F é obtido pelo rácio entre média dos quadrados do modelo e média dos quadrados dos resíduos. Este valor pode ser traduzido num teste de significância ao modelo, avaliando em que medida as VI’s do modelo no seu conjunto, predizem a VD mais do que o esperado pelo acaso. 

# Calculate F-value
(f_stats <- ms.model/ms.residual)

# Calculate P-value
(p_value <- 1 - pf(f_stats, df.model, df.residual))


### Diagnóstico da regressão
# A função plot() pode ser aplicada ao resultado da função lm(). 

par(mfrow=c(2,2))
plot(model1)
dev.off()

# O resultado é um conjunto de 4 gráficos que permitem avaliar a qualidade do modelo estimado, tendo por base os pressupostos da estimação por OLS:  
  
# > Normalidade: Pode ser avaliada através do Q-Q plot. O Q-Q plot permite comparar os quartis da distribuição observada com os quartis de uma distribuição normal. Se os dados observados seguirem uma observação normal, os seus valores irão ser semelhantes aos valores de uma distribuição normal, situando-se sobre a linha diagonal do gráfico.    
 
# > Independência das observação: Nenhum dos gráficos permite avaliar este pressuposto, devendo ser inferido pela forma como os dados foram recolhidos e pela sua natureza  
 
# > Linearidade: Pode ser avaliada pelo gráfico "Residuals vs Fitted". Se a VD apresenta uma relação linear com a VI não deverá existir uma relação sistemática entre os valores residuais do modelo e os valores preditos por esse modelo, uma vez que toda a variância sistemática dos dados deve ser capturada pelo modelo, sobrando apenas erro aleatório. A disposição aparentemente aleatória dos valores dos resíduos para os valores estimados é indicativa de que este pressuposto está cumprido  

# > Homocedasticidade: Pode ser avaliada pelo gráfico "Scale-Location". Se o pressuposto da homegeneidade das variâncias está cumprido, os pontos no gráfico Scale-location devem estar dispostos de forma aleatória ao longo do eixo horizontal.
 
# O gráfico Residuals vs Leverage acrescenta informação relativamente à existência de observações que podem estar a afetar a qualidade do modelo. Este gráfico permite identificar três tipos de valores:  
# * Outliers: valores que não são preditos corretamente pelo modelo e que, por isso apresentam valores muito elevados de erro (neste caso podem ser positivos ou negativos uma vez que são valores estandardizados);
# * Observações com elevado “leverage” são observações outliers ao nível da VI, isto é valores da VI que são inesperados;
# * Observações influentes, isto é observações que apresentam um impacto desproporcional na determinação dos parâmetros do modelo. Neste gráfico estas observações são aquelas que estão fora da distância de Cook.  


## Modelos de regressão polinomial  
# O gráfico de dispersão seguinte sugere que a predição o nosso modelo pode ser melhorada se incluirmos um termo quadrático na equação:

df_lm <- coffee[, c("coffee", "rating") ]
df_lm <- df_lm[order(df_lm$coffee),]
df_lm$coffee


plot(df_lm$rating ~ df_lm$coffee, 
     xlab="Coffee ratings",
     ylab="Coffee shop general score")
abline(lm(rating ~ coffee, data = coffee))


# A equação de um modelo de regressão com um termos quadrático é a seguinte:
  
# rating ~ β0 + β1*coffee + β2*$coffee^2$ + ε 

# No R, este modelo é estimado usando os seguinte código:  
  
  
model1 <- lm(rating ~ coffee, data=df_lm)
model2 <- lm(rating ~ coffee + I(coffee^2), data=df_lm)


# O novo termo do modelo **I(coffee^2)** define o termo quadrático na equação. A função **I()** trata o seu conteúdo como um expressão aritmética regular do R e não como conteúdo normal de uma objeto do tipo fórmula. Esta função é necessária uma vez que o símbolo **^** tem um significado específico quando lida numa contexto de fórmula R.
summary(model2)

# Pelo sumário é possível definir o seguinte modelo de predição:

# rating = 4.39 - 1.50 * coffee + 1.93 * $coffee^2$ + ε   

# Ambos os coeficientes de regressão são significativos e a variância explica é de cerca de 23.6%. O termo quadrático significativo (t = 2.972, p = .004) sugere que a sua inclusão melhora o ajustamento do modelo aos dados. O gráfico seguinte aponta no mesmo sentido

plot(df_lm$coffee, df_lm$rating,
     xlab="Coffee ratings",
     ylab="Coffee shop general score", 
     main="Modelo de regressão polinomial")
lines(df_lm$coffee, fitted(model1))
lines(df_lm$coffee, fitted(model2))

# A função **anova()** permite testar formalmente se existem ou não diferenças entre o ajustamento dos dois modelos, com e sem termos quadrático.
anova(model1, model2)

# O R disponibiliza vários pacotes que permite apresentar e comparar resultados. O pacote **jtools** é um exemplo desse tipo de pacotes.
library(jtools)
library(huxtable)

summ(model1)
summ(model2)

effect_plot(model1, pred = coffee, interval = TRUE, plot.points = TRUE)
effect_plot(model2, pred = coffee, interval = TRUE, plot.points = TRUE)

export_summs(model1, model2)

plot_summs(model1, model2, scale = TRUE)
plot_summs(model1, model2, scale = TRUE, plot.distributions = TRUE)

## Modelos de regressão multipla (mais do que um preditor)

# Quando existe a necessidade incluir mais do que uma variável preditora para explicar uma determinada variável dependente falamos em modelos de regressão múltipla. Imagine-se que pretendemos explicar o score global de um determinado estabelecimento não apenas pela satisfação dos clientes face ao café mas também função de aspetos como a qualidade da comida servida, preço e simpatia do serviço. Tal como os modelos de regressão simples, os modelos de regressão múltipla podem ser testados no R através da função **lm()**.  

# Um primeiro passo recomendado quando testamos este tipo de modelos é avaliar a associação entre as variáveis que fazem parte do modelo.

vars.model <- c("rating", "coffee", "food", "price", "service")
cor(coffee[, vars.model])

library(car)
scatterplotMatrix(coffee[, vars.model], spread=FALSE, smoother.args=list(lty=2),
                  main="Matriz de diagramas de dispersão")


# Vamos agora criar um modelo de regressão incluindo os ratings do café, comida, preço e serviço como preditores da satisfação geral dos clientes com o café.
model3 <- lm(rating ~ coffee + food, data = coffee)
summary(model3)

model4 <- lm(rating ~ coffee + food + price, data = coffee)
summary(model4)

model5 <- lm(rating ~ coffee + food + price + service, data = coffee)
summary(model5)


# Quando existe mais do que um preditor no modelo, o coeficiente de regressão para um determinado preditor indica a medida em que a VD aumenta ou diminui por unidade de mudança desse preditor, mantendo constantes os valores dos outros preditores do modelo.
 
# Por exemplo, do modelo 5, com os 4 preditores, o coeficiente de regressão para a qualidade de serviço é de .64, o que significa que ao aumento de 1 ponto na satisfação com o serviço está associada o aumento de .64 pontos na satisfação global com o estabelecimento, controlando os valores de satisfação com o café, comida e preço. Este coeficiente é significativamente diferente de zero (*p* = .023).

# É possível comparar os modelos 1, 3, 4 e 5 com a função **anova()**. Este procedimento é habitualmente designado por regressão linear hierárquica e permite determinar e permite, pela comparação sucessiva de modelos regressão, verificar a mais valia que cada modelo tem, em termos de variância explicada da VD, face aos outros envolvidos na comparação.
export_summs(model1, model3, model4, model5)
anova(model1, model3, model4, model5)

# Pode ser também interessante comparar a magnitude dos efeitos dos diferentes preditores numa mesma escala. Para o efeito é necessário calcular os coeficiente de regressão estandardizados, usando, por exemplo, a função **lm.beta()**. A função **export_summs()** com o argumento **scale=TRUE** permite também ter acesso aos coeficientes estandardizados.
library(lm.beta)
lm.beta(model2)

export_summs(model1, model3, model4, model5, scale=TRUE)

## Modelos de regressão multipla com fatores de interação
# Uma questão que frequentemente emerge no processo de estimação de modelos de regressão tem a ver com a interacção entre variáveis independentes.
# Vamos assumir que estamos interessados em clarificar os efeitos da satisfação com o café e qualidade do serviço sobre o score geral de satisfação atribuido a cada um dos estabelecimentos. Podemos ajustar um modelo de regressão incluindo estes dois preditores e um terceiro preditor que capta a sua interacção. No R o termo de interacção é criado com o operador **:** tal como demonstrado no código seguinte.
model6 <- lm(rating ~ coffee + service, data = coffee)
summary(model6)

model7 <- lm(rating ~ coffee + service + coffee:service, data = coffee)
summary(model7)

anova(model6, model7)


# No sumário do modelo 7 verificamos que o coeficiente de regressão que representa a interação entre satisfação com o café e com serviço é significativo. Isto significa que o efeito de um dos preditores (eg. "coffee") sobre a VD dependen do nível do outro preditor (eg.: "service"). Assim, a relação entre satisfação com café e satisfação geral varia de acordo com a satisfação do serviço.
 
# Assim, o modelo estimado é o seguinte:  
# rating = 3.18 - (1.55 * coffee) + (2.39 * service) - (3.18 * coffee * service)  + ε   

# Para interpretar a interacção pode ser útil substituir uma das variáveis por alguns valores de referência. Podemos escolher 3 valores para substituir da variável **service**, a média e um desvio padrão acima e abaixo desse valor. Assim, podemos verificar os coeficientes de **coffee** nestes 3 diferentes níveis de **service**

mean(coffee$service)-sd(coffee$service)
# rating = 3.18 + (2.39 * 0.19) - (1.55 * coffee) - (3.18 * 0.19 * coffee)
# rating = 3.18 - (1.55 * coffee) - (.60 * coffee)
# rating = 3.63 - (.95 * coffee)

mean(coffee$service)
# rating = 3.18 + (2.39 * 0.32) - (1.55 * coffee) - (3.18 * 0.32 * coffee)
# rating = 3.94 - (1.55 * coffee) - (1.02 * coffee)
# rating = 3.63 - (.53 * coffee)

mean(coffee$service)+sd(coffee$service)
# rating = 3.18 + (2.39 * .45) - (1.55 * coffee) - (3.18 * .45 * coffee)
# rating = 4.26 - (1.55 * coffee) - (1.43 * coffee)
# rating = 3.63 - (0.12 * coffee)


# Assim com o aumento da satisfação com o serviço (0.19, 0.32 e 0.45) a mudança esperada no rating geral por unidade de satisfação com o café parece diminuir (.95, .53 e .12). Por outras palavras o efeito da qualidade do café na satisfação geral dos clientes parece atenuar-se quando os níveis de satisfação com o serviço aumentam. Os gráficos seguintes permitem visualizar esta interação.
library(effects)
plot(effect(term="coffee:service", mod=model7, xlevels=list(service=c(0.19, 0.32, 0.45))), multiline=TRUE)

library(sjPlot)
library(sjmisc)
library(ggplot2)
plot_model(model7, type = "pred", terms = c("coffee", "service"))


##########################################################
##########################################################
##########################################################
  
# 6. Análise de variância
# Nos modelos de regressão anteriores utilizamos VIs contínuas para predizer uma VD também contínua. É possível expandir o este modelo para incluir fatores ordinais como VI e mesmo combinar variáveis contínuas e fatores como VI’s no mesmo modelo. Por norma, quando são incluídos fatores no modelo o foco deixa de ser a predição da VD, para passar a ser também a análise de diferenças entre grupos.  

# O desenvolvimento da análise de variância está muito associada a desenhos de investigação experimental. Existe toda uma nomenclatura própria associada a este tipo de análise, que pode por vezes ser confusa. Para clarificar alguma da terminologia associada à análise de variância recorremos ao exemplo clássico de um estudo experimental com grupo de controlo (GC) e grupo experimental (GE), em que se pretende avaliar o impacto de uma intervenção sobre o níveis de ansiedade dos participantes (Ans.).  

# Neste exemplo, o fator "between-groups" é o grupo a que participante é alocado, GC ou GE (dois níveis do fator). Cada participante apenas pode pertencer a um grupo. Assim, neste modelo a VD são os resultados de ansiedade dos participantes e a VI é o fator de dois níveis correspondente ao grupo a que o sujeito foi alocado (GC ou GE). Falamos em “balanced designs” quando existe o mesmo número de observações nos diferentes níveis do fator e em “unbalanced designs” quando esta condição não se verifica.  

# Porque neste modelo apenas estamos interessados no efeito de um fator sobre a VD, o design é denominado de “one-way between groups ANOVA”. Os efeitos neste contexto de modelação são avaliados com recurso ao F-test, já apresentado anteriormente.  

# Se o interesse for perceber o efeito da intervenção sobre a ansiedade ao longo do tempo, a denominação mais comum é “one-way within groups ANOVA”. Neste caso, o tempo é um fator “within-group”, uma vez que cada sujeito é avaliado mais do que uma vez. Este design também se pode chamar de “repeated measures ANOVA”. Neste contexto, é fácil ampliar o modelo para perceber em que medida existe uma interação entre os dois fatores, grupo e tempo, afeta a VD . Para o efeito, simplesmente acrescentamos aos dois efeitos principais (do grupo e do tempo) o termo de interação. Este design é habitualmente denominado de “factorial ANOVA”. A utilização de 2 fatores no modelo produz uma “two-way ANOVA”, enquanto que a utilização de 3 fatores dá origem a um modelo designado por “three-way ANOVA”. No caso do design incluir simultaneamente fatores “between groups” e “within-groups”, como é o caso dos fatores grupo (GC vs GE) e tempo, o design pode ser também chamado de “mixed-model ANOVA”. Assim, o termo “two-way mixed-model factorial ANOVA” representará um modelo que inclui como preditores dois fatores, um “between groups” e outro “within-groups”, bem como a sua interação.

# Este modelo de ANOVA pode ser também expandido para incluir variáveis contínuas como preditores. No contexto da investigação experimental as variáveis contínuas são essencialmente vistas como variáveis ruído e que importa controlar no modelo para que o efeito dos fatores seja estimado de forma mais realista. Na análise de variância este tipo de variáveis são denominadas de covariáveis. Um modelo de análise de variância que, para além dos fatores, inclui uma ou mais covariaveis é apelidado de análise de convariância ou ANCOVA. Finalmente, podemos ainda ampliar o modelo para incluir mais do que uma variável dependente. Quando este é o caso, falamos de MANOVA. 

# Apesar da flexibilidade dos modelos de análise de variância, no contexto deste curso iremos restringir-nos à apresentação e discussão da one-way ANOVA.  


## Estimação de modelos de análise de variância no R  
# Apesar dos modelos de regressão já apresentados e os modelos de análise de variância surgirem tradicionalmente em campos de investigação diferentes (estudos correlacionais ou estudos experimentais), ambos são casos especiais da aplicação do GLM (“General Linear Model”). Por conseguinte, no R é possível analisar uma ANOVA (ou outro modelo de análise de variância) através da função lm(), usada anteriormente para estimar modelos de regressão. Isto será demonstrado mais à frente neste curso. Apesar disso, é mais comum utilizar a função aov() quando pretendemos estimar modelos de análise de variância no R. Apesar das funções lm() e aov() serem equivalentes, a função aov() apresenta os resultados num formato mais habitual e familiar à investigação de cariz experimental. 

# A função aov() apresenta o seguinte formato genérico:

# model <-  lm(formula, data)
# The formula argument defines the model to be fitted; the argument data specifies the data used to fit the model
  

# A forma de especificar a fórmula do modelo é semelhante à forma de especificar a fórmula a utilizar na função lm():  

# y ~ A + B + C  
# ~ separates the DV (y) from the factors (x1, x2, x3, ... xk); 
# factors are separated by +


# Os seguintes símbolos podem ser utilizados na especificação da fórmula do modelo:  
  
símbolo <- c(":",
             "*",
             "^",
             ".")
significado <- c("Define interação entre fatores",
                 "Atalho para definir interações entre todos os fatores do modelo (y ~ A * B * C <==> y ~ A + B + C + A:B + A:C + B:C + A:B:C)",
                 "Define interação a um nível específico ((y ~ A * B * C)^2 <==> y ~ A + B + C + A:B + A:C + B:C)", 
                 "Define todas as variáveis da DF como fatores no modelo ( y ~ . <==> y ~ A + B + C)") 
anovasimbol <- data.frame(símbolo, significado)
View(anovasimbol)


# Na tabela seguinte apresentam-se as fórmulas para alguns dos designs de análise de variância mais comuns.

Design <- c("One-way ANOVA",
            "One-way ANCOVA com 1 covariável (x) ",
            "Two-way factorial ANOVA",
            "Two-way factorial ANOVA com 2 covariáveis (x1 e x2)", 
            "One-way within-groups ANOVA", 
            "Repeate measures ANOVA com 1 between-groups factor (B) e 1 fator within-groups (w)")
Fórmula <- c("y ~ A",
             "y ~ x + A",
             "y ~ A * B", 
             "y ~ x1 + x2 + A * B", 
             "y ~ A + Error(Subject/A)", 
             "y ~ B * W + Error(Subject/W)")
anovasimbol <- data.frame(Design, Fórmula)
View(anovasimbol)


# A ordem pela qual os efeitos são especificados na fórmula não é irrelevante quando (a) existe mais do que um fator e o design é “unbalanced” ou (b) são incluídas covariáveis. Nestas circunstâncias as VI’s do modelo estão correlacionadas, não existindo por isso uma forma não ambígua de perceber o contributo independente de cada uma dessas VI’s sobre a VD. A seguinte fórmula define um modelo de ANOVA fatorial com y a ser estimado pelos fatores A e B, e sua interação, A:B:  
  
# > y ~ A + B + A:B  
# Assumindo que o modelo anterior vai ser aplicado sobre um “unbalanced design”, existem 3 abordagem gerais para perceber a quantidade de variância de y que é explicada pelos diferentes efeitos preditores do modelo (i.e., dois efeitos principais (A e B) e um efeito de interação (A:B)):   
  
# > Abordagem sequencial (tipo I) – Os efeitos são ajustados aos efeitos que surgem primeiro na fórmula. No exemplo anterior:  
# >> (1) é calculado efeito de A;  
# >> (2) o efeito de B é ajustado ao efeito de A;  
# >> (3) o efeito de A:B é ajustado aos efeitos de A e B.      

# > Abordagem hierárquica (tipo II) – Cada efeito é ajustado a todos os efeitos do mesmo nível ou de nível inferior. No exemplo anterior:   
# >> (1) o efeito de A é ajustado ao efeito de B;  
# >> (2) o efeito de B é ajustado ao efeito de A;  
# >> (3) o efeito de A:B é ajustado aos efeitos de A e B.     

# > Abordagem marginal (tipo III) – Cada efeito é ajustado a todos os outros efeitos estimados no modelo. No exemplo anterior:  
# >> (1) o efeito de A é ajustado ao efeito de B e A:B;  
# >> (2) o efeito de B é ajustado ao efeito de A e A:B;  
# >> (3) o efeito de A:B é ajustado aos efeitos de A e B.    

# Por defeito, o R aplica a abordagem sequencial (tipo I). Outros pacotes estatísticos como o SAS ou o SPSS aplicam por defeito a abordagem marginal (tipo III). Por conseguinte, ao utilizar a função oav() com modelo ortogonais (modelos em que as VD estão correlacionas), faz sentido especificar primeiro na fórmula do modelo os efeitos mais fundamentais, por exemplo as covariáveis. Se for importante aplicar uma abordagem diferente da abordagem sequencial (tipo I), o utilizador poderá recorrer à função Anova() do pacote car. Esta função permite aplicar as abordagens hierárquica (tipo II) e marginal (tipo III) na estimação do modelo de variância.

## One-way ANOVA  
# Como já foi dito, aplica-se a one-way ANOVA quando estamos interessados em comparar a média de uma variável dependente em dois ou grupos definidos por um fator. O exemplo seguinte estima este modelo utilizando a função aov().

names(coffee)

replications(rating~cityarea*convenience_rec, data=coffee)
# replications(rating~cityarea*convenience_rec, data=coffee) # check if the design is balance or unbalanced
table(coffee$cityarea)  

aggregate(coffee$rating, by=list(coffee$cityarea), FUN=mean) 
aggregate(coffee$rating, by=list(coffee$cityarea), FUN=sd) 

# ?aov
model1 <- aov(rating ~ cityarea, data = coffee)
summary(model1)

if (!require("gplots")) install.packages("gplots")
library(gplots)
plotmeans(coffee$rating ~ coffee$cityarea, xlab="coffee location in city area", ylab="rating", 
          main="Mean Plot\nwith 95% CI")

### Múltiplas comparações
# O resultado do F-test (p < .05) evidencia que o rating médio dos cafés varia em função da sua localização. Importa agora perceber que cafés diferem entre sim. Esta questão pode ser respondida através de comparações múltiplas entre cafés.

# Tukey HSD pairwise group comparisons
TukeyHSD(model1)
# ?TukeyHSD

# ploting the confidence intervals for the multiple comparisons
par(las=2) # rotates axis labels
par(mar=c(5,8,4,2)) # increases the left margin are
plot(TukeyHSD(model1)) 
dev.off()


# Multiple comparisons using the multcomp package
if (!require("multcomp")) install.packages("multcomp")
library(multcomp)
par(mar=c(5,4,6,2)) # increases top margin
# ?glht
tuk <- glht(model1, linfct=mcp(cityarea = "Tukey")) # reproduces Tukey HSD test
tuk
plot(cld(tuk, level=.05),col="lightgrey") 
plot(cld(tuk, level=.001),col="lightgrey") 

# Other multiple comparisons tests
if (!require("DescTools")) install.packages("DescTools")
library(DescTools)

# ?PostHocTest 
PostHocTest(model1, method = "lsd") # options for method are: "hsd" (Tukey), "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"
PostHocTest(model1, method = "hsd") 
PostHocTest(model1, method = "scheffe")

# compare p-values:
round(cbind(
  lsd = PostHocTest(model1, method="lsd")$cityarea[,"pval"], 
  hsd = PostHocTest(model1, method="hsd")$cityarea[,"pval"],
  scheffe = PostHocTest(model1, method="scheffe")$cityarea[,"pval"]), 4)

### Diagnóstico da modelo
# A análise dos pressupostos subjacentes à estimação do modelo pode ser feita da mesma forma que análise dos pressupostos do modelo de regressão.  

plot(model1)

# Tal como foi discutido anteriormente para os modelo de regressão, também os modelos de análise de variância pressupõem a normalidade da VI e a homogeneidade da variância para os diferentes níveis da VD. Em alternativa à função plot() a normalidade da VD pode ser avaliada através da função qqPlot() do pacote car.  

library(car)
qqPlot(aov(rating ~ cityarea, data=coffee), 
       simulate=TRUE, main="Q-Q Plot", labels=FALSE)

# A homogeneidade das variâncias pode ser testada por exemplo através do Bartlett’s test.

bartlett.test(rating ~ cityarea, data=coffee)

# A homogeneidade das variâncias pode ser testada por exemplo através do Bartlett’s test.
# Ou, em alternativa os teste de Fligner-Killeen, através da função fligner.test() , de Brown-Forsythe, através da função hov().

fligner.test(rating ~ cityarea, data=coffee)
if (!require("HH")) install.packages("HH")
library(HH)
hov(rating ~ cityarea, data=coffee)

# Finalmente, a presença de outliers pode ser detetada pela função outlierTest() do pacote car.

library(car)
outlierTest(model1) # no indication of outliers (NA occurs when p>1)
#?outlierTest

## ANOVA e regressão
# Como referido anteriormente a análise de variância e a regressão são derivações do GLM. Por conseguinte, é possível estimar modelos de variância, como o modelo one-way ANOVA antes apresentado, utilizando a função lm(). 
# Primeiro, estimamos o modelo utilizando a função aov().

levels(coffee$cityarea)
modelfit_oav <- aov(rating ~ cityarea, data = coffee)
summary(modelfit_oav)

# De seguida, o mesmo modelo é estimado com recurso à função lm(). 

modelfit_lm <- lm(rating ~ cityarea, data = coffee)
summary(modelfit_lm)

# Para perceber o resultado da aplicação da função lm() ao modelo anterior, é útil perceber como internamente o R lida com variáveis categoriais no processo de estimação de modelos de regressão. Uma vez que os GLM apenas acomodam preditores numérico, quando a função **lm()** “encontra” um fator na sua fórmula, substitui esse fator por um conjunto de variáveis numéricas que representam os contrastes entre os diferentes níveis do fator. 
# Assim, se o fator tem k níveis, o número de variáveis de contraste geradas será k-1. O R disponibiliza 5 métodos diferentes para criar estas variáveis contraste.


# contr.helmert(c("L1", "L2", "L3", "L4", "L5")) # "contrast the second level with the first, the third with the average of the first two, and so on"

# contr.poly(c("L1", "L2", "L3", "L4", "L5")) # constrast used for trend analysis, using linear, quadratic, cubic... for each constrast variable

# contr.sum(c("L1", "L2", "L3", "L4", "L5")) # contrasts are constrained to sum to zero

# contr.treatment(c("L1", "L2", "L3", "L4", "L5")) #  contrasts each level with the baseline level (specified by base) - dummy coding

# contr.SAS(c("L1", "L2", "L3", "L4", "L5")) # Similar to contr.treatment, but sets the base level to be the last level of the factor

# Por defeito o R emprega o “treatment contrast” em fatores não ordenados e “orthogonal polynomials” em fatores ordenados. O modelo anterior emprega as seguintes variáveis contraste.

coffee$cityarea
levels(coffee$cityarea)
contrasts(coffee$cityarea) # same as contr.treatment()

# Voltando à estimação da ANOVA através da função **lm()**, podemos verificar que a variável **cityareaeastcity** no sumário do modelo não é mais do que a variável contraste eastcity vs. downtown, enquanto que a variável **cityareawestcity** é a variável contraste westcity vs. eastcity e downtown. Pelos valores das probabilidades podemos verificar que westcity e eastcity são significativamente diferentes de downtown em termos dos seus ratings. O F-test combina os efeitos destas variáveis contraste salientando que rating médio dos cafés varia em função da sua localização. A função **anova()** pode ser aplicada ao resultado da função lm(), gerando um sumário igual ao sumário da função aov().

modelfit <- lm(rating ~ cityarea, data = coffee)
summary(modelfit)

anova(modelfit)

# A função lm() permite alterar o tipo de contraste através do argumento "contrast".

?lm
modelfit <- lm(rating ~ cityarea, data = coffee, contrast="contr.SAS")
summary(modelfit)
anova(modelfit)

