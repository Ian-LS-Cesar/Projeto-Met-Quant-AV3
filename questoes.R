# Trabalho Métodos Quantitativos

# Colegas de equipe
# Nome: Ian Lucas 

# Matrícula: 2310294  

# Nome: Pedro Guilherme

# Matrícula: 2223975

#Questão 1

q1_x <- 0:50


q1_simples <- dbinom(q1_x, 50,  0.3)
q1_acumulada <- pbinom(q1_x, 50, 0.3)

plot(q1_x, q1_simples, type = "h")
plot(q1_x,q1_acumulada, type = "h")

#Questão 2

q2_x <- 0:100

q2_fx1 <- dpois(q2_x, 25)
plot(q2_x, fx1, type = 'h')

q2_fx2 <- ppois(q2_x, 40)

plot(q2_x, q2_fx2, type = 'h')

#Questão 3 

peso <- 0:1000
x = rnorm(1000,65,5)
y = hist(rnorm(1000,65,5), freq = FALSE)
curve(dnorm(x, mean = 65, sd = 5), 
      col = "darkblue", lwd = 2, add = TRUE)

#Questão 4
#Item A

#Modelo Binomial
q4_n <- 50  #n = 50
q4_p <- 0.15 #p = 0.15

#Item B

##p(x=0) = 0.000295764663712699

q4_b <- dbinom(0, q4_n, q4_p)
q4_b

#Item C

##p(x < 5) = 0.112105208054978

q4_c <- pbinom(4, q4_n, q4_p)
q4_c

#Item D

##p(x >= 10) = 0.208906330332109

q4_d <- 1 - pbinom(9, q4_n, q4_p)
q4_d

#Item E

# p(x <= 30) = 0.999999999999955

q4_e <-  pbinom(30, q4_n, q4_p)
q4_e

#Item F

##p(3 < x < 12) = 0.891141352975622

q4_f <- pbinom(11, q4_n, q4_p) - pbinom(3, q4_n, q4_p)
q4_f

#Item G

##p(13 <= x <= 27) = 0.0300605282007782

q4_g <-  pbinom(27, q4_n, q4_p) - pbinom(12, q4_n, q4_p)
q4_g

#Questão 5

q5_n <- 1000 # n = 1000
q5_p <- 0.25 # p = 0.25

#Item A

##p(x = 0) = 0.778800783071405
q5_a <- dpois(0, q5_p)


#Item B

##p(x <= 3) = 0.999866630349486
q5_b <- ppois(3, q5_p)

#Item C

##p(x >= 5) = 1 - p(x <= 4) = 6.61171056104415e-06
q5_c <- 1 - ppois(4, q5_p)

#Item D

#n = 500
#p = 0.5


q5_d <- pbinom(3, 10, dpois(0, 0.5))

#questão 6

#a)distribuição binomial
n <- 15 
p <- 0.2

#b)
media <- n*p
media
variancia <- n*p*(1-p)
variancia
#c)
p_qst_c <- 1 - pbinom(3, n, p)
p_qst_c
#d)
p_qst_d <- 1 - pbinom(2, n, (1-p))
#e)
p_qst_e <- pbinom(9, 20, p) - pbinom(2, 20, p)
p_qst_e

#Questão 7

#μ = 2090
#σ = 150
media_7 <- 2090
desvio_padrao_7 <- 150

#Item A

##p(x >= 1900) = 0.897362748167864

q7_a <- 1 - pnorm(1900, media_7, desvio_padrao_7)

#Item B

## p(1800 <= x <= 1900) = 0.0760396778111262
q7_b <- pnorm(1900, media_7, desvio_padrao_7) - pnorm(1800, media_7, desvio_padrao_7)

#Item C

q7_c <- qnorm(0.025, media_7, desvio_padrao_7)
q7_c
#Item D

# p(x <= 1) = 7.37624119494705e-05
q7_d <- pbinom(1, 4, 1 - pnorm(1800, media_7, desvio_padrao_7))

#Item E

## p(x = 2) = 0.344071080519169
q7_e <- dbinom(2, 5, pnorm(2060, media_7, desvio_padrao_7))

#questão 8
#a)
p_qst8_a <- pnorm(990, 1000, 10)
p_qst8_a

#b)
p_qst8_b <- pnorm((1000+(2*10)),1000,10) - pnorm((1000-(2*10)),1000,10)
p_qst8_b

#c)
prob_maior_1002 <- 1 - pnorm(1002, mean = 1000, sd = 10)
n <- 10
prob_no_max_4 <- pbinom(4, n, prob_maior_1002)
prob_no_max_4
