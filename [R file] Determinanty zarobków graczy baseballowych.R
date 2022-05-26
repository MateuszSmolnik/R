library(Lahman)
library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)

Salaries$salary <- Salaries$salary/1000000
my_salaries <- filter(Salaries, yearID %in% 2006:2016)
salaries2016NL <- filter(Salaries, yearID == 2016, lgID == "NL")
salaries2016AL <- filter(Salaries, yearID == 2016, lgID == "AL")
salariesNL <- filter(my_salaries, lgID == "NL")
salariesAL <- filter(my_salaries, lgID == "AL")

#Statystki opisowe
summary(my_salaries$salary)
describeBy(my_salaries$salary,group=my_salaries$yearID, mat=T, digits = 2)

#Zarobki w poszczególnych klubach z podzialem na lige
ggplot(salaries2016AL, aes(x=factor(teamID), y=salary, color=factor(teamID))) + 
  geom_boxplot() + ggtitle("Wykresy pudelkowe zarobkow w druzynach z ligi Amerykańskiej w roku 2016") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(salaries2016NL, aes(x=factor(teamID), y=salary, color=factor(teamID))) + geom_boxplot() +
  ggtitle("Wykresy pudelkowe zarobkow w druzynach z ligi Narodowej w roku 2016") +
  theme(plot.title = element_text(hjust = 0.5))

#zarobki w zaleznosci od ligi
ggplot(my_salaries, aes(x=factor(yearID), y=salary, color=factor(lgID))) + geom_boxplot() +
  ggtitle("Wykresy pudelkowe zarobkow baseballistów w latach 2006-2016") +
  theme(plot.title = element_text(hjust = 0.5))

#Test t-studenta
t.test(salariesAL$salary, salariesNL$salary, alternative="greater", mu=0,
       var.equal=T)
t.test(salaries2016AL$salary, salaries2016NL$salary, alternative="greater", mu=0,
       var.equal=T)


#obrobka danych
mydata <- right_join(People,Appearances) %>% left_join(Batting) %>% arrange(playerID, yearID) 
mydata$debut <- as.Date(mydata$debut,format = "%Y")
mydata$debut <- as.integer(substring(mydata$debut,1,4))

mydata <- mutate(mydata, age = yearID - birthYear,
                 experience = yearID - debut,
                 prev_year_games = ifelse(playerID==lag(playerID),lag(G_all),0), 
                 prev_3years_games = ifelse(playerID==lag(playerID),lag(G_all),0) + 
                   ifelse(playerID==lag(playerID,k = 2),lag(G_all, k=2),0)+
                   ifelse(playerID==lag(playerID,k = 3),lag(G_all, k=3),0),
                 prev_year_hitting= ifelse(playerID==lag(playerID),lag(H),0),
                 prev_3years_hitting=ifelse(playerID==lag(playerID),lag(H),0) + 
                   ifelse(playerID==lag(playerID,k = 2),lag(H, k=2),0)+
                   ifelse(playerID==lag(playerID,k = 3),lag(H, k=3),0),
                 prev_year_homeruns= ifelse(playerID==lag(playerID),lag(HR),0),
                 prev_3years_homeruns=ifelse(playerID==lag(playerID),lag(HR),0) + 
                   ifelse(playerID==lag(playerID,k = 2),lag(HR, k=2),0)+
                   ifelse(playerID==lag(playerID,k = 3),lag(HR, k=3),0))
mydata <- inner_join(mydata,Salaries)
mydata <- filter(mydata, yearID %in% 2006:2016)

####WYKRESY####
#wynagrodzenie w zaleznosci od doswiadczenia
ggplot(mydata, aes(x=experience, y=salary, col=age)) + geom_jitter(aes(alpha=0.7))+
  ggtitle("Zależność wynagrodzeń od doświadczenia(lata od debiutu) i wieku") +
  theme(plot.title = element_text(hjust = 0.5))



#wynagrodzenia w zaleznosci od liczby rozegranych meczy w poprzednim roku
select(mydata, playerID, yearID, G_all, prev_year_games)
ggplot(mydata, aes(x=prev_year_games, y=salary)) + geom_jitter(aes(alpha=0.8)) +
  ggtitle("Zależność wynagrodzeń od liczby meczy rozegranych w poprzednim roku") +
  theme(plot.title = element_text(hjust = 0.5))

#wynagrodzenia w zaleznosci od liczby rozegranych meczy w poprzednich 3 latach
ggplot(mydata, aes(x=prev_3years_games, y=salary)) + geom_jitter(aes(alpha=0.8)) +
  ggtitle("Zależność wynagrodzeń od liczby meczy rozegranych w poprzednich 3 latach") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(mydata, aes(x=prev_3years_games, y=salary)) + geom_jitter(aes(alpha=0.8)) +
  ggtitle("Zależność wynagrodzeń od liczby meczy rozegranych w poprzednich 3 latach") +
  theme(plot.title = element_text(hjust = 0.5))


#wynagrodzenia w zaleznosci od liczby rozegranych meczy w poprzednich 3 latach + linia zaleznosci
ggplot(mydata, aes(x=prev_3years_games, y=salary, )) + geom_point(aes(alpha=0.8)) + 
  geom_smooth(se = F) +
  ggtitle("Zależność wynagrodzeń od liczby meczy rozegranych w poprzednich 3 latach") +
  theme(plot.title = element_text(hjust = 0.5))



#Wynagrodzenie od uderzen w poprzednim roku
ggplot(mydata, aes(x=prev_year_hitting, y=salary)) + geom_point(aes(alpha=0.8))+
  ggtitle("Zależność wynagrodzeń od liczby uderzeń w poprzednim roku") +
  theme(plot.title = element_text(hjust = 0.5))

#Wynagrodzenie od uderzen w poprzednich 3 latach
ggplot(mydata, aes(x=prev_3years_hitting, y=salary)) + geom_jitter(aes(alpha=0.8)) +
  ggtitle("Zależność wynagrodzeń od liczby uderzeń w poprzednich 3 latach") +
  theme(plot.title = element_text(hjust = 0.5))

#Wynagrodzenie od uderzen w poprzednich 3 latach + linia zaleznosci
ggplot(mydata, aes(x=prev_3years_hitting, y=salary, col =)) + geom_jitter(aes(alpha=0.5)) + 
  geom_smooth(method = "lm", se = F) +
  ggtitle("Zależność wynagrodzeń od liczby uderzeń w poprzednich 3 latach") +
  theme(plot.title = element_text(hjust = 0.5)) 



#Wynagrodzenie od uderzen, po ktorych zostaly zdobyte wszystkie cztery bazy 
#(w poprzednim roku)
ggplot(mydata, aes(x=prev_year_homeruns, y=salary)) + geom_jitter(na.rm = T,aes(alpha=1))+
  ggtitle("Zależność wynagrodzeń od liczby uderzeń w poprzednim roku") +
  theme(plot.title = element_text(hjust = 0.5))

#Wynagrodzenie od uderzen, po ktorych zostaly zdobyte wszystkie cztery bazy 
#(w poprzednich 3 latach)
ggplot(mydata, aes(x=prev_3years_homeruns, y=salary)) + geom_jitter(aes(alpha=0.8)) +
  ggtitle("Zależność wynagrodzeń od liczby uderzeń, po których zostaly zdobyte wszystkie bazy w (poprzednich 3 latach)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits=c(0,150)) +
  scale_y_continuous(limits=c(0,25))
