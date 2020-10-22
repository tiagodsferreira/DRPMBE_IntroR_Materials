###########################
# EXERCÍCIO 4: IRIS I
##########################

# 1. Importar para workspace a base de dados iris;    
data(iris)

# 2. Aceder a informação disponível acerca para descrever a base de dados;  
?iris

# 3. Verificar a estrutura e dimensões da base de dados;  
str(iris)
dim(iris)

# 4. Verificar os nomes dadas variáveis de iris:  
names(iris)

# 5. Verificar os 4 primeiros e últimos casos de iris. 
head(iris, 4)
tail(iris, 4)


###########################
# EXERCÍCIO 5: IRIS II
##########################
# 1. Importar para workspace a base de dados iris;  
data(iris)

# 2. Verificar a estrutura e dimensões da base de dados;  
str(iris)

# 3. Verificar os nomes das variáveis de iris;  
names(iris)

# 4. Visualizar os 10 primeiros elementos da DF;  
head(iris, 10)

# 5. Visualizar 15 elementos aleatórios da DF;  
iris[sample(c(nrow(iris)), 15),]

# 6. Aceder à segunda coluna de iris, usando o operador $;  
iris$Sepal.Width

# 7. Aceder ao elemento da décima linha com a terceira coluna, usando indexação de matrizes [];    
iris[10,3]

# 8. Sumariar as variáveis da DF;  
summary(iris)

# 9. Anexar iris e contar o número de especies usando a função table();  
attach(iris)
table(Species)

# 10. Fazer um histograma da variável Species e interpretar o resultado;    
hist(Species)

# 11. Desanexar a DF.  
detach(iris)



###########################
# EXERCÍCIO 6: Titanic 
##########################

# Criar a base de dados titanic_trans correndo a sintaxe seguinte.

data(Titanic) # "This data set provides information on the fate of passengers on the fatal maiden voyage of the ocean liner ‘Titanic’, summarized according to economic status (class), sex, age and survival."
Titanic
# changing dataset for the exercise
titanic_trans <- as.data.frame(Titanic)
titanic_trans <- titanic_trans[rep(1:nrow(titanic_trans), titanic_trans[["Freq"]]), ]
# titanic_trans[["Freq"]]
# rep(1:nrow(titanic_trans), titanic_trans[["Freq"]])
row.names(titanic_trans) <- 1:nrow(titanic_trans)
titanic_trans[["Freq"]] <- NULL
titanic_trans <-  as.data.frame(sapply(titanic_trans, as.character), stringsAsFactors = FALSE )
titanic_trans <- as.data.frame(titanic_trans, stringsAsFactors = FALSE)

# 1. A base de dados titanic_trans já está criada a partir da base de dados datasets::Titanic. Verificar a estrutura e primeiras linhas de titanic_trans;  
str(titanic_trans) # 1
head(titanic_trans)

# 2. Transformar as variáveis string de titanic_trans nos fatores mais apropriados: categorial nominal ou categorial ordinal;  
titanic_trans[["Class"]] <- factor(titanic_trans[["Class"]], # 2
                                   levels = c("1st", "2nd", "3rd", "Crew")) 
titanic_trans$Sex <-  factor(titanic_trans$Sex,
                             levels = c("Male", "Female"))
titanic_trans[, 3] <-  factor(titanic_trans[, 3], order = TRUE,
                              levels = c("Child", "Adult"))
titanic_trans[, 4] <-  factor(titanic_trans[, 4], 
                              levels = c("Yes", "No"))

# 3. Contar o número de sobreviventes;  
table(titanic_trans[, 4]) # 3

# 4. Contar o número de sobreviventes do sexo masculino e feminino;  
table(titanic_trans[, 4], titanic_trans[, 2]) # 4

# 5. Contar o número de sobreviventes por sexo e faixa etária;  
table(titanic_trans[, 4], titanic_trans[, 2], titanic_trans[, 3]) # 5

#Extra - this code will produce a Mosaic Plot
data <- table(titanic_trans[, 4], titanic_trans[, 2], titanic_trans[, 3])
plot(data, 
     main ="Titanic surving by age and sex",
     color=TRUE,
     margin=TRUE)

# 6. Verificar a correlação entre sobrevivência e classe de viagem. Interprete o output. Em alternativa poderá utilizar a função chisq.test() para perceber se as duas variáveis são (H0) ou não são (H1) independentes.  
cor(titanic_trans[, 1],titanic_trans[, 4]) # 6
chisq.test(titanic_trans[, 1],titanic_trans[, 4])

?chisq.test


###########################
# EXERCÍCIO 7: "Valar Morghulis"
##########################
# Importar a base de dados GOT.

GOT <- read.csv("G:\\My Drive\\FPCEUP\\R trainning\\GitRepo\\DRPMBE_R-Intro\\Tutorials\\Data\\GoT.csv", sep=",", dec=".")
# Retrived from: https://data.world/data-society/game-of-thrones

# 1. Verificar estrutura, nomes das variáveis, primeiras 10 linhas, últimas 10 linhas;   
str(GOT)
head(GOT)
tail(GOT)
names(GOT)

# 2. Eliminar as variáveis "plod", "dateOfBirth", "DateoFdeath", "mother", "father", "heir", "house", "spouse", "book1", "book2", "book3", "book4", "book5", "boolDeadRelations" e "isPopular";  
GOT[c("plod", "dateOfBirth", "DateoFdeath", "mother", "father", "heir", "house", "spouse", "book1", "book2", "book3", "book4", "book5", "boolDeadRelations", "isPopular")] <-  NULL 
names(GOT)

# 3. Mudar o nome da variável "S.No" para "id", "male" para "sex", "isAliveMother" para "condition_mother", "isAliveFather" para "condition_father","isAliveHeir" para "condition_heir" e "isAliveSpouse" para "condition_spouse";  
names(GOT)[c(1, 4, 6:9)] <- c("id", "sex", "condition_mother", "condition_father", "condition_heir", "condition_spouse")
names(GOT)

# 4. Transformar o fator "name" numa variável string;   
GOT$name <-  as.character(GOT$name)
str(GOT)

# 5. Transformar a variável "sex" num fator com 2 níveis (0 = Female; 1 = Male);   
GOT$sex <- factor(GOT$sex, 
                  levels = c(0,1),
                  labels = c("Female", "Male"))
str(GOT)

# 6. Transformar as variáveis começadas por "condition..." em fatores com 2 níveis (0 = dead; 1 = alive);  
names(GOT[, c(6:9)])
GOT$condition_mother <- factor(GOT$condition_mother, 
                               levels = c(0,1),
                               labels = c("dead", "alive"))
table(GOT$condition_mother)
GOT$condition_father <- factor(GOT$condition_father, 
                               levels = c(0,1),
                               labels = c("dead", "alive"))
table(GOT$condition_father)
GOT$condition_heir <- factor(GOT$condition_heir, 
                             levels = c(0,1),
                             labels = c("dead", "alive"))
table(GOT$condition_heir)
GOT$condition_spouse <- factor(GOT$condition_spouse, 
                               levels = c(0,1),
                               labels = c("dead", "alive"))
table(GOT$condition_spouse)
# OR 
# GOT[, c(6:9)] <- lapply(GOT[, c(6:9)], factor, 
#                       levels=c(0,1), 
#                       labels = c("dead", "alive"))
str(GOT)

# 7. Transformar as variáveis começadas por "isMarried", "isNoble" e "isAlive" em fatores lógicos (0 = FALSE; 1 = TRUE);  
GOT$isMarried <- factor(GOT$isMarried, 
                        levels = c(0,1),
                        labels = c(FALSE, TRUE)) # 7
table(GOT$isMarried)
GOT$isNoble <- factor(GOT$isNoble, 
                      levels = c(0,1),
                      labels = c(FALSE, TRUE))
table(GOT$isNoble)
GOT$isAlive <- factor(GOT$isAlive, 
                      levels = c(0,1),
                      labels = c(FALSE, TRUE))
table(GOT$isAlive)
# OR
# GOT[, c(10,11,15)] <- lapply(GOT[, c(10,11,15)], factor, 
#                         levels = c(0,1), 
#                         labels = c(FALSE, TRUE))
str(GOT)

# 8. Verificar os dados atribuídos a "Jon Snow". Tendo em conta os desenvolvimentos da sétima temporada televisiva de GoT, alterar para esta personagem os valores das variáveis "condition_mother" (Lyanna) e "condition_father" (Rhaegar) para "dead";
GOT[GOT$name=="Jon Snow",]
GOT$condition_mother[GOT$name=="Jon Snow"] <- "dead"
# OR
GOT$condition_mother[1750] <- "dead"
GOT$condition_father[GOT$name=="Jon Snow"] <- "dead"
# OR
GOT$condition_father[1750] <- "dead"

# 9. Sumariar todas as variáveis da base de dados excetuando a variável "id";  
summary(GOT[,-1])
str(GOT)

# 10. Fazer um boxplot da variável idade;  
boxplot(GOT$age)

# 11. Confirmar quais os nomes das personagens com valores de idade "impossíveis" e substituir pelos valores corretos (pista: Doreah tem 25 anos e o Rhaego tem 0). Após a correção, visualizar o histograma com a distribuição da variável "age";  
which(GOT$age<0)
GOT$name[c(1685, 1869)]
# OR
GOT$name[which(GOT$age<0)]
# OR
GOT[which(GOT[12]<0),2]
# OR
GOT[which(GOT$age<0),"name"]

GOT$age[which(GOT$age<0)] <- c(25,0)
hist(GOT$age)

# 12. Criar uma nova variável na base de dados, “deadrelative”, consistindo num fator lógico em que TRUE significa que pelo menos um familiar (mãe, pai herdeiro, ou conjugue) foi morto e FALSE significa que nenhum desses familiares foi morto. Contar as frequências utilizando a função table();  
GOT$condition_mother == "dead"
GOT$condition_father == "dead"

GOT$deadrelative[GOT$condition_mother == "dead" |
                   GOT$condition_father == "dead" |
                   GOT$condition_heir == "dead" |
                   GOT$condition_spouse == "dead"] <- "TRUE"
GOT$deadrelative[GOT$condition_mother == "alive" &
                   GOT$condition_father == "alive" &
                   GOT$condition_heir == "alive" &
                   GOT$condition_spouse == "alive"] <- "FALSE"                             
table(GOT$deadrelative)


# OR - with subset() 
# relative_dead <- subset(GOT, GOT$condition_mother == "dead" |
#                           GOT$condition_father == "dead" |
#                           GOT$condition_heir == "dead" |
#                           GOT$condition_spouse == "dead")
# rownames(relative_dead)
# 
# GOT[rownames(relative_dead), "deadrelative2"] <- TRUE
# 
# relative_alive <- subset(GOT, GOT$condition_mother == "alive" &
#                            GOT$condition_father == "alive" &
#                            GOT$condition_heir == "alive" &
#                            GOT$condition_spouse == "alive")
# rownames(relative_alive) # logo não é preciso fazer mais nada para esta condição
# 
# table(GOT$deadrelative2)
# 
# 
# OR (The winner!!!!)
# 
# GOT[GOT$condition_mother %in% "dead" |
#   GOT$condition_father %in% "dead" |
#   GOT$condition_heir %in% "dead" |
#   GOT$condition_spouse %in% "dead", "deadrelative3"] <- TRUE
# 
# table(GOT$deadrelative3)
# class(GOT$deadrelative3)
# 
# # o truque para evitar o NA é usar %in% em vez de ==

# 13. Criar uma crosstab (freq1) com a frequência de casos com e sem famíliares mortos por título. Transformar a tabela de frequências numa tabela de proporções (porp1);  
freq1 <- table(GOT$deadrelative, GOT$title)
freq1
prop1 <- a/sum(a)
prop1
# OR
prop1 <- prop.table(freq1)
prop1

# 14. Criar uma crosstab (freq2) combinando as frequências dos fatores "sex" e "isAlive". Fazer o sumário deste objeto (freq) e verificar os resultados obtidos. Se necessário, criar uma tabela de proporções para clarificar resultados;  
freq2 <- table(GOT$sex, GOT$isAlive) # 14
freq2
summary(freq2)
prop.table(freq2, 1)

# 15. Usando a função plot fazer um gráfico para vizualizar a relação entre idade e popularidade e um gráfico para visualizar a relação entre sexo e popularidade. Utilize a função cor() para verificar o valor da associação entre os mesmos dois pares de variáveis. NOTA:  incluir o argumento use="pairwise.complete.obs" para que a função cor() seja capaz de lidar com missing values. Interpretar os resultados e tentar forçar a correlação entre sexo e popularidade.  
plot(GOT$age, GOT$popularity) # 15
plot(GOT$sex, GOT$popularity)
cor(GOT$age, GOT$popularity, use="pairwise.complete.obs")
# cor(GOT$sex, GOT$popularity, use="pairwise.complete.obs")
chisq.test(GOT$sex, GOT$popularity)

cor(as.numeric(GOT$sex), GOT$popularity, use="pairwise.complete.obs")