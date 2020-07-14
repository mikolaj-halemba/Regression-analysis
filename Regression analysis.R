# Badam zale¿noœæ zu¿ycia paliwa w milach przejechanych na jednym galonie 
# (zmienna mpg) od wielkoœci ich mocy (zmienna horsepower).
# Dane 15 losowo wybranych samochodach znajduj¹ siê w ramce danych auta2.

auta2 <- data.frame(mpg=c(43.1,20.3,17,21.6,16.2,31.5,31.9,
                          25.4,27.2,37.3,41.5,34.3,44.3,43.4,36.4),
                    horsepower=c(48,103,125,115,133,71,71,77,71,69,
                                 76,78,48,48,67))

##	Wyznaczam liniowy model regresji opisuj¹cy badan¹ zale¿noœæ.

y <-auta2$mpg # Zuzycie paliwa
x <-auta2$horsepower  # Moc auta

#wyznaczenie prostej regresji
plot(x,y,xlab="Moc auta",ylab = "Zuzycie paliwa")

# Szukamy prostej regresji y = a + b*x
model <- lm(y~x)
summary(model)

#y = 57.63524 + -0.32761*x
abline(model,col=4)
text(100,30,"y = 57.63524 + -0.32761*x",col=4)

##Weryfikuje poprawnoœæ modelu

# 1 Czy istnieje zaleznosci miedzy Y a X ?
# H0: b=0 (nie)
# H1: b!=0 (tak)
# x p-value = 2.78e-06 < 0.05 czyli przyjmujemy H1 czyli jest zaleznosc miedzy Y a X

# 2 Czy wyraz wolny (stala A ) jets istotna w modelu 
# H0: a=0 (nie)
# H1: a!=0 (tak)
# (Intercept) p-value = 4.60e-10 < 0.05 przyjmujemy H1 czyli wyraz wolny jest istotny

# 3  Wspoczynnik  determinacji R^2 = 0.8255 czyli  model bardzo dobrze dopasowany do danych bo blisko 1

# 4 analiza reszt (normalnosci)
segments(x,y,x,model$fitted.values) # Y policzone z modelu
model$residuals # reszty = Y-model2$fitted.values 

# H0 : reszty maja rozklad normalny
# H1 : nie maja

shapiro.test(model$residuals)
#p-value = 0.1241 > 0.05 czyli reszty maja rozklad normalny

##Oszacowuje punktowo i przedzia³owo (poziom ufnoœci 95%) zu¿ycie paliwa auta o mocy 80KM.

# prognoza punktowa
57.63524 + -0.32761*80
# wynik to 31.42644
points(80,31.42644,pch=20) # Zaznaczenie punktu na wykresie


predict(model,list(x=80),interval = 'c',level = 0.95)
#(29.07035 33.78299)prognoza przedzialowa dla sredniego Y:95%CI


#Ramka danych obrazy zawiera ceny obrazów (w 100tys. $) dla trzech miejsc aukcji i trzech autorów.
#Przeprowadzam analize czy miejsce aukcji ma wp³yw na cenê obrazu,czy cena obrazu zale¿y od miejsca aukcji,
#czy wystêpuj¹ tu istotne interakcje miêdzy miejscem aukcji a autorem.

obrazy <- data.frame(cena=c(6, 7, 7, 8, 6, 5, 6, 6, 7, 5, 7, 8, 6, 7, 6,
                            8, 6, 5, 6, 7, 7, 8, 7, 6, 6, 6, 7, 7, 6, 5,
                            7, 5, 7, 6, 7, 7, 6, 7, 7, 8, 8, 9, 7, 8, 9),
                     aukcja=gl(3,15,45,labels = c('Londyn','Nowy Jork','Tokio')),
                     autor=gl(3,5,45,labels = c('Picasso','Chagall','Salvador Dali')))

#Tworzymy zmienna pomocnicza do sprawdzenia zalozenia testu F 
proba <- obrazy$aukcja:obrazy$autor


model <-aov (obrazy$cena~obrazy$aukcja*obrazy$autor)
summary(model)

# H0AB: brak interakcji
# H1AB: wystepuje interakcja 
#obrazy$aukcja:obrazy$autor p-value = 0.0225 > 0.01 nie wystepuje interkacja 

# H0A: czynnik A nie wplywa na X
# H1A: czynnik A wplywa na X
#obrazy$aukcja p-value = 0.0389  > 0.01 obrazy$aukcja nie wplywa X

# H0B: czynnik B nie wplywa na X
# H1B: czynnik B wplywa na X
#obrazy$autor p-value = 0.1669  > 0.01 obrazy$autor nie wplywa na X


#Wybrano losowo grupê mê¿czyzn i zapytano ich o œredni¹ liczbê litrów pepsi spo¿ywan¹ w ci¹gu miesi¹ca

x <- c(37,42,25,14,48,78,18,34,20,57) # Wiek
y <- c(3,2,4,5,1,0.3,8,2.5,7,0.5) # Litry

par(mfrow=c(2,2))

#a)	Wyznaczam mo¿liwie najlepszy nieliniowy model regresji opisuj¹cy zale¿noœæ spo¿ywanej liczby pepsi w zale¿noœci od wieku.
model2<-lm(log(y)~x) # Najlepsze
model3<-lm(y~log(x))
model4<-lm(log(y)~log(x))

plot(x,log(y));abline(model2)
plot(log(x),y);abline(model3)
plot(log(x),log(y));abline(model4)

#Analizuje poprawnoœæ modelu

#R^2 dla tych modeli:
summary(model2)$r.squared # = 0.9377014 Najlepsza
summary(model3)$r.squared # = 0.8009534
summary(model4)$r.squared # = 0.8639345 
# i wybieramy ktory ma najwieksze R^2
# nasz model najlepszy to model 2


model2<-lm(log(y)~x)
summary(model2)

# 1 Czy istnieje zale¿nosc miedzy Y a X ?
# H0: b=0 (nie)
# H1: b!=0 (tak)
# x p-value = 4.23e-06 < 0.05 czyli przyjmujemy H1 czyli jest zaleznosc miedzy Y a X

# 2 Czy wyraz wolny (stala A ) jest istotna w modelu 
# H0: a=0 (nie)
# H1: a!=0 (tak)
# (Intercept) p-value = 8.12e-07 < 0.05 czyli przyjmujemy H1 czyli wyraz wolny jest istotny

# summary(model2)$r.squared # = 0.9377014 bliskie 1 czyli model jest dobrze dopasowany 


shapiro.test(model2$residuals)
#p-value = 0.1194 > 0.05 czyli reszty maja rozklad normalny

#Oszacowuje punktowo i przedzia³owo ile litrów pepsi miesiêcznie spo¿ywa 20-latek.Przyjmujr poziom ufnoœci 0.95.

exp(predict(model2,list(x=20),interval = 'c',level = 0.95))
#(1.42348 2.001245)prognoza przedzialowa dla sredniego Y:95%CI z log 
#(4.151544 7.398258)prognoza przedzialowa dla sredniego Y:95%CI bez log

par(mfrow=c(1,1))
plot(x,log(y))
abline(model2,col=2)
text(40,0,"Y =  2.786537-0.053709*X",col=2)

