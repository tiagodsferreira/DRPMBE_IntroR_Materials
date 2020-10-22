###########################
# EXERCÍCIO 3: CIDADES II
##########################
# Criar os vetores "city" (nomes das cidades), "population" (número de habitantes), "areakm2" (área) e buying_pw (rendimento per capita)usando a seguinte sintaxe.  
# city <- c("Odivelas", "Almada", "Amadora", "Matosinhos", "Braga", "Loures", "Cascais", "Porto", "Vila Nova de Gaia", "Sintra", "Lisboa")
# population <- c(144549, 174030, 175136, 175478, 181494, 205094, 206479, 237591, 302295, 377835, 547733)
# areakm2 <- c(26.5, 70.2, 23.8, 62.4, 183.4, 167.2, 97.4, 41.4, 168.5, 319.2, 100.0)
# buying_pw <- c(91.94, 109.80, 105.74, 124.35, 104.17, 102.38, 132.01, 161.65, 99.13, 101.25, 216.88)

# 1. Criar a matriz data_city (11x2) combinando os vetores population e areakm2;   
data_city <- cbind(population, areakm2)

# 2. Atribuir nomes às linhas da matriz utilizado o vetor city;  
row.names(data_city) <- city

# 3. Visualizar e confirmar a classe e dimensões do objeto data_city;  
data_city 
class(data_city)
dim(data_city)

# 4. Aceder à segunda coluna de data_city;  
data_city[,2]

# 5. Usar a função round() para arredondar a coluna areakm2 para 0 casas decimais;  
data_city[,2] <- round(data_city[ , 2], 0)

# 6. Criar a matriz area com os dados da segunda coluna de data_city;  
area <- data_city[ , 2, drop=FALSE]
area

# 7. Aceder aos dados das cidades da zona metropolitana do Porto;  
data_city[c("Matosinhos", "Porto", "Braga"), ]

# 8. Criar a matrix zmporto com os dados das cidades da zona metropolitana do Porto;  
zmporto <- data_city[c("Matosinhos", "Porto", "Vila Nova de Gaia"), ]
zmporto

# 9. Calcular a média da população nas cidades da zona metropolitana do Porto;
mean(zmporto)

# 10. Acrescentar à matriz o vetor poder de compra já disponível no workspace ("buying_pw");  
data_city
data_city <- cbind(data_city, buying_pw)
data_city

# 11. Criar o vetor district, contendo o distrito para as diferentes cidades da matriz;   
data_city # 11
district <- c("Lisboa", "Setubal", "Lisboa", "Porto", "Braga", "Lisboa", "Lisboa", "Porto", "Porto", "Lisboa", "Lisboa")

# 12. Criar a matriz data_city_alt acrescentando o vetor district à matriz data_city;  
data_city_alt <- cbind (data_city, district)

# 13. Verificar a estrutura e o tipo de dados da matriz data_city_alt. Explicar o resultado obtido;  
str(data_city_alt)
storage.mode(data_city_alt)

# 14. Eliminar a matriz data_city_alt do seu workspace;  
rm(data_city_alt)

# 15. Utilizando a matriz data_city, calcular um indicador de densidade populacional (denspop), que consistirá no rácio população/areakm2;
denspop <- data_city[,1]/data_city[,2] 

# 16. Adicionar o vetor denspop à matriz city_data e visualize o resultado;
data_city <- cbind(data_city, denspop)

# 17. Fazer um sumário das variáveis na matriz data_city;
summary(data_city)
 
# 18. Criar um histograma para cada uma das variáveis na matriz data_city;  
hist(data_city[,"population"])
hist(data_city[,"areakm2"])
hist(data_city[,"denspop"])
hist(data_city[,"buying_pw"])

# 19. Calcular a correlação entre poder de compra e densidade populacional e visualizar graficamente;   
cor(data_city[, "buying_pw"], data_city[, "denspop"])
plot(data_city[, "buying_pw"], data_city[, "denspop"])

# 20. Limpar o workspace e consola. 
rm(list = ls())
# shortcut Ctrl + l 
