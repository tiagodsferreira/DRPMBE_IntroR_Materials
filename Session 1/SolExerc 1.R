###########################
# EXERCÍCIO 1: Lotaria  
##########################

# 1. Iniciar uma nova sessão de R, verificando a versão de R e as informações da sessão iniciada. Verifique também quais os pacotes que tem carregados;   
getRversion() # or "version" command
sessionInfo()
search()

# 2. Verifique o working directory da sua sessão e tente alterar para o seu desktop;  
getwd()
setwd("C:/Users/tiago/Desktop")

# 3. Crie um novo objeto R, "meusnumeros", contento cinco números inteiros, não repetidos, selecionados por si de entre o conjunto de números inteiros de 1 a 50;  
meusnumeros <- c(7, 4, 22, 14, 24)

# 4. Crie um novo objeto R, "minhasestrelas", contento dois números inteiros, não repetidos, selecionados por si de entre o conjunto de números inteiros de 1 a 12;  
minhasestrelas <- c(5, 7)

# 5. Crie um novo objeto R, chavenumeros, que combina cinco números inteiros não repetidos selecionados aleatoreamente de entre o conjunto de números inteiros de 1 a 50. Para tal, utilize o seguinte argumento: sample(1:50, 5, replace=FALSE);  
chavenumeros <- sample(1:50, 5, replace=FALSE)

# 6. O que é que a função "sample()" está a fazer? Peça ajuda ao R para compreender os argumentos desta função;  
?sample

# 7. Utilizando novamente a função sample(), crie o objeto "chaveestrelas", que combina 2 números inteiros não repetidos selecionados aleatoriamente de entre o conjunto de números inteiros de 1 a 12;  
chaveestrelas <- sample(1:12, 2, replace=FALSE)

# 8. Visualize na consola os quatro objetos criados, "meusnumeros", "minhasestrelas", "chavenumeros" e "chaveestrelas";  
meusnumeros
minhasestrelas
chavenumeros
chaveestrelas

# 9. Verificar se os seus números e estrelas foram os sorteados pelo R. Para tal utilize o operador lógico %in% que permite verificar que elementos são comuns a dois objetos (eg., objeto1 %in% objeto2). Poderá usar o código **?match** para aceder a ajuda relacionada com **%in%**.  
meusnumeros %in% chavenumeros
minhasestrelas %in% chaveestrelas 

# 10. Verifique que objetos estão no seu worspace;   
ls()

# 11. Elimine os objetos "meusnumeros" e "minhasestrelas" do workspace;  
rm(meusnumeros, minhasestrelas)

# 12. Apague todos os objetos do seu workspace e limpe a console. 
rm(list = ls())
# shortcut Ctrl + l

