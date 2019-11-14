
getwd()

#pacotes exigidos para o exercicio#

library(ggplot2)
library(dplyr)
library(statsr)
library(BAS)
library(grid)
library(gridExtra)


#setar seed para replicar#

set.seed(3514)


#baixar os dados do exemplo#

load(url('https://stat.duke.edu/~mc301/data/movies.Rdata'))


names(movies)


#criar novas variaveis#

movies <- movies %>%
          mutate(feature_film = as.factor(ifelse(title_type == 'Feature Film', 'yes','no'))) %>%
          mutate(drama = as.factor(ifelse(genre == 'Drama', 'yes', 'no'))) %>%
          mutate(mpaa_rating_R = as.factor(ifelse(mpaa_rating == 'R', 'yes', 'no'))) %>%
          mutate(oscar_season = as.factor(ifelse(thtr_rel_month %in% c(10:12), 'yes', 'no'))) %>%
          mutate(summer_season = as.factor(ifelse(thtr_rel_month %in% c(5:8), 'yes', 'no')))


#remover os NA#

movies <- filter(movies, !is.na(runtime))


#graficos sobre os filmes#


p1 <- ggplot(data = movies,
             aes(x = genre)) +
      geom_bar(fill = "blue") +
      xlab('Genre') +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       vjust = 0))


p2 <- ggplot(data = movies,
             aes(x = title_type)) + 
      geom_bar(fill = 'blue') +
      xlab("Movie Type") +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1))

p3 <- ggplot(data = movies,
             aes(x = mpaa_rating)) +
      geom_bar(fill = 'blue') +
      xlab("MPAA Rating") +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       vjust = 0))

p4 <- ggplot(data=movies, aes(x=runtime)) + 
      geom_histogram(binwidth=10, fill="blue") +
      xlab("Runtime")

p5 <- ggplot(data=movies, aes(x=thtr_rel_month)) + 
      geom_histogram(binwidth=1, fill="blue") +
      scale_x_continuous(breaks=c(1:12)) +
      scale_y_continuous(limits=c(0, 100)) +
      geom_vline(xintercept=c(5, 8, 10, 12), colour='green', linetype='longdash') +
      geom_text(label='Summer', x=6.5, y=95, hjust='center', size=3) +
      geom_text(label='Oscar', x=11, y=95, hjust='center', size=3) +
      geom_text(label='Season', x=6.5, y=89, hjust='center', size=3) +
      geom_text(label='Season', x=11, y=89, hjust='center', size=3) +
      xlab("Theater Release Month")


p6 <- ggplot(data=movies, aes(x=audience_score)) + 
      geom_histogram(binwidth=5, fill="blue") +
      geom_vline(xintercept=mean(movies$audience_score), colour='green', linetype='longdash') +
      geom_text(label='Mean', x=55, y=60, hjust='center', size=3) +
      xlab("Audience Score")

p7 <- ggplot(data=movies, aes(x=imdb_rating)) + 
      geom_histogram(binwidth=1, fill="blue") +
      geom_vline(xintercept=mean(movies$imdb_rating), colour='green', linetype='longdash') +
      geom_text(label='Mean', x=6, y=225, hjust='center', size=3) +
      xlab("IMDb Rating")

p8 <- ggplot(data=movies, aes(x=critics_score)) + 
      geom_histogram(binwidth=5, fill="blue") +
      geom_vline(xintercept=mean(movies$critics_score), colour='green', linetype='longdash') +
      geom_text(label='Mean', x=51, y=50, hjust='center', size=3) +
      xlab("Critics Score")

##diminuir o dataframe usando apenas as variaveis indicadas para a predicao##


movies <- movies %>% 
          select(runtime, thtr_rel_year, imdb_rating, imdb_num_votes,
                 critics_score, audience_score, best_pic_nom, best_pic_win,
                 best_actor_win, best_actress_win, best_dir_win, top200_box,
                 feature_film, drama, mpaa_rating_R, oscar_season, summer_season)


##Modelo Bayes##

###Usando a simulacao de Markov Chain Monte Carlo (MCMC) ###
###A FDP usada foi da Zellner-Siow Cauchy###

basLM1 <- bas.lm(audience_score ~ ., data=movies, method='MCMC',
                 prior='ZS-null', modelprior=uniform())




#graficos do residuos e probabilidade#
#os graficos representam:
#i)residuos e fitted
#ii)probabilidade dos modelos
#iii)inclusao de prob


par(mfrow=c(2,2))
plot(basLM1, which=c(1, 2), ask=FALSE)
plot(basLM1, which=4, ask=FALSE, cex.lab=0.5)


#predicao#

BMA_basLM1 = predict(basLM1, estimator="BMA", se.fit=TRUE)
BMA_confint_fit = confint(BMA_basLM1, parm="mean")
BMA_confint_pred = confint(BMA_basLM1, parm="pred")

head(cbind(BMA_confint_fit, BMA_confint_pred), 10)


#diagnose da predicao#

diagnostics(basLM1, type="model",  pch=16)



#usando o filme DeadPool como exemplo para a predicao
#criamos um data frame que indica os parametros do filme
#quer dizer que criaremos vetores identicos ao Deadpool
#o mesmo tempo de duracao(runtime), o numero de votos do imdb (imdb_num_votes)

dfDeadpool <- data.frame(runtime=108,
                         thtr_rel_year=2016,
                         imdb_rating=8.1,
                         imdb_num_votes=500049,
                         critics_score=84,
                         audience_score=0,
                         best_pic_nom=factor("no", levels=c("no", "yes")),
                         best_pic_win=factor("no", levels=c("no", "yes")),
                         best_actor_win=factor("no", levels=c("no", "yes")),
                         best_actress_win=factor("no", levels=c("no", "yes")),
                         best_dir_win=factor("no", levels=c("no", "yes")),
                         top200_box=factor("no", levels=c("no", "yes")),
                         feature_film=factor("yes", levels=c("no", "yes")),
                         drama=factor("no", levels=c("no", "yes")),
                         mpaa_rating_R=factor("yes", levels=c("no", "yes")),
                         oscar_season=factor("no", levels=c("no", "yes")),
                         summer_season=factor("no", levels=c("no", "yes")))


#predicao da nota da audiencia no filme DeadPool#
#note-se que o audience_score no dfDeadpool foi 0

BMA_basLM1_DP <- predict(basLM1, newdata=dfDeadpool, estimator="BMA", se.fit=TRUE)

#calcular margem de erro

BMA_basLM1_predME <- qt(0.95, df=BMA_basLM1_DP$se.bma.pred[1]) *
  mean(BMA_basLM1_DP$se.bma.pred)


#mostrar a predicao#

df <- data.frame(t="Deadpool",
                 p=sprintf("%2.1f", BMA_basLM1_DP$Ybma),
                 i=sprintf("%2.1f - %2.1f", BMA_basLM1_DP$Ybma - BMA_basLM1_predME,
                           BMA_basLM1_DP$Ybma + BMA_basLM1_predME),
                 r=84)


colnames(df) <- c("Movie Title", "Predicted Rating", "95% Prediction Interval", 
                  "Actual Rating")

df
