###########################
# EXERCÍCIO 2: CIDADES I
##########################
# 1. Com base na tabela anterior, criar dois vetores:  
#   * "city_name", nome das 11 cidades portuguesas mais populosas;   
# * "population", número de pessoas residentes em cada uma destas cidades.   

city_name <- c("Odivelas", "Almada", "Amadora", "Matosinhos", "Braga", "Loures", "Cascais", "Porto", "Vila Nova de Gaia", "Sintra", "Lisboa")
city_name

population <- c(144549, 174030, 175136, 175478, 181494, 205094, 206479, 237591, 302295, 377835, 547733)
population

# 2. Utilizar "city_name" para definir o nome dos elementos do vetor "population";  
names(population) <- city_name
population

# 3. Selecionar as cidades que têm menos de 200000 residentes;   
population[population<200000]

# 4. Criar o vetor lógico "large_city" para assinalar (TRUE) as cidades portuguesas com mais de 300000 residentes;  
large_city <- population > 300000
large_city

# 5. Inspecionar a classe do objeto "population", bem como o seu cumprimento;  
class(population)
length(population)

# 6. Sabendo que a população residente em Portugal é de 10300300 pessoar, criar o vetor "porp_pop", para representar a proporção da população portuguesa que vive em cada uma das cidades;   
# 7. Arredondar o vetor "porp_pop" a 2 casas decimal;  
porp_pop <- round(population / 10300300, 2)
porp_pop 

# 8. Criar o vetor "perc_pop" para representar a percentagem da população portuguesa a residir em cada uma das cidades portuguesas;  
perc_pop <-  porp_pop * 100
perc_pop

# 9. Qual o número total de pessoas a residir nas 11 maiores cidades portuguesas?   
sum(population)

# 10. Calcular o número médio de habitantes nestas 11 cidades.   
sum(population)/length(population)

