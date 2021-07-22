# PROJEKT R

suppressWarnings(suppressMessages(require("library_name")))
options(warn=-1)
# Aby uruchomic program w trybie wsadowym nalezy znajdujac sie w folderze, 
# w ktorym jest skrypt, wpisac w terminalu WINDOWS:
# C:\"Program Files"\R\R-4.0.4\bin\Rscript.exe Projekt_R.R http://www.cs.put.poznan.pl/kgutowska/RPiS/dane/przykladoweDane-Projekt.csv
# Uruchamianie odbywa sie przez Rscript.exe a nie przez R.exe!

# Potrzebne biblioteki
install.packages("Hmisc", repos = "https://cloud.r-project.org/")
library(Hmisc)
install.packages("dplyr", repos = "https://cloud.r-project.org/")
library(dplyr)
install.packages("ggpubr", repos = "https://cloud.r-project.org/")
library(ggpubr)
install.packages("dunn.test", repos = "https://cloud.r-project.org/")
library(dunn.test)
install.packages("FSA", repos = "https://cloud.r-project.org/")
library(FSA)
install.packages("car", repos = "https://cloud.r-project.org/")
library(car)




# Tryb wsadowy -> Uruchomienie skryptu z conajmniej 1 argumentem.

args = commandArgs(trailingOnly = TRUE)
if(length(args) == 0)
  stop("Nalezy podac conajmniej 1 argument wejsciowy")

DANE <- read.csv2(file = args[1], sep = ";")


#testowo bez trybu wsadowego
DANE <- read.csv2("http://www.cs.put.poznan.pl/kgutowska/RPiS/dane/przykladoweDane-Projekt.csv", sep = ";")

print("NASZ PLIK:")
print(DANE)



print("WYSZUKIWANIE ILE MAMY GRUP BADANYCH I ILE PACJENTOW W KAZDEJ W GRUP:")

grupy <- unique(DANE$grupa)   # wektor grupa z nazwami grup badanych
print(paste("W tym pliku mamy", length(grupy), "grupy."))
index <- 1
ile_badanych <- c()
for(i in 1:length(grupy)){            # funkcja, ktora wylicza ilosc badanych w tych grupach
  ile_badanych[index] <- length(which(DANE$grupa == grupy[i]))
  index = index + 1
}
print("Ilosc badanych pacjentow w grupach:")
print(ile_badanych)
cat("\n")



print("PRZYGOTOWANIE DANYCH WEJSCIOWYCH -> ZASTEPOWANIE BRAKOW")
# HGB 13 wiersz œrednia - 12,41 mediana - 12,404
# MON 5 wiersz mediana - 0,76 srednia - 0,85
# HGB wiersz 68 mediana - 11,43 srednia - 11,26
#i <- 1
poczatek <- 1
koniec <- ile_badanych[1]
for(i in 1:length(grupy)){
  for(j in 1:length(DANE)){
    if(is.numeric(DANE[,j])){
      if(length(which(is.na(DANE[poczatek:koniec,j]))) != 0){
        braki <- which(is.na(DANE[1:nrow(DANE),j]))
        DANE[poczatek:koniec,j] <- impute(DANE[poczatek:koniec,j], mean)
        print(paste("Znaleziono braki w grupie:", grupy[i], "w kolumnie", names(DANE[j])))
        print("Indeksy z brakami: ")
        print(braki)
        print(paste("Braki w przedziale indeksow", poczatek, "-", koniec, "zastapiono srednia z grupy", grupy[i]))
      }
    }else{
      mediana <- median(DANE[poczatek:koniec,j])
      print(paste("Mediana:", mediana, "- dla grupy", grupy[i], "- dla parametru", names(DANE[j])))
    }
  }
  poczatek <- koniec + 1
  koniec <- koniec + ile_badanych[i+1] 
}
print("NASZE DANE BEZ BRAKOW")
print(DANE)

cat("\n\n")

print("PRZYGOTOWANIE DANYCH WEJSCIOWYCH -> WARTOSCI ODSTAJACE")

poczatek <- 1
koniec <- ile_badanych[1]
a <- 1
grupa_wykres <- c()
parametr_wykres <- c()
min_odstajace <- c()
max_Odstajace <- c()
for(i in 1:length(grupy)){
  for(j in 1:length(DANE)){
    Odstajace <- c()
    if(is.numeric(DANE[,j])){
      min <- mean(DANE[poczatek:koniec,j]) - sd(DANE[poczatek:koniec,j])
      max <- mean(DANE[poczatek:koniec,j]) + sd(DANE[poczatek:koniec,j])
      m <- 1
      for(k in poczatek:koniec){
        if((DANE[k,j] > max) || (DANE[k,j] < min)){
          Odstajace[m] = DANE[k,j]
          m <- m+1
        }
      }
      Odstajace <- sort(Odstajace)
      grupa_wykres[a] <- grupy[i]
      parametr_wykres[a] <- names(DANE[j])
      min_odstajace[a] <- Odstajace[1]
      max_Odstajace[a] <- Odstajace[length(Odstajace)]
      a = a+1
    }
  }
  poczatek <- koniec +1
  koniec <- koniec + ile_badanych[i+1]
}
Tabela_odstajacych <- data.frame(grupa = grupa_wykres, parametr = parametr_wykres, Min_odstajaca = min_odstajace, Max_odstajaca = max_Odstajace)
print("Wartosci odstajace:")
print(Tabela_odstajacych)

cat("\n\n")

print("STATYSTYKA OPISOWA")

# Statystyka opisowa 
for(i in 1:length(DANE)){
  if(names(DANE[i]) != "grupa"){
    if(is.numeric(DANE[,i])){
      print(paste("CHARAKTER ZMIENNEJ", names(DANE[i]), " -> MIERZALNY"))
      podsumowanie <- group_by(DANE, grupa) %>%
        summarise(
          Ilosc_badanych = n(),
          Srednia = format(round(mean(DANE[,i]), 2), nsmall = 2),
          Mediana = format(round(median(DANE[,i]), 2), nsmall = 2),
          Rozstep_miedzykwartalowy = format(round(IQR(DANE[,i]), 2), nsmall = 2),
          Wariancja = format(round(var(DANE[,i]), 2), nsmall = 2),
          Odchylenie_standardowe = format(round(sd(DANE[,i]), 2), nsmall = 2)
        )
      print(paste("Podsumowanie statystyki opisowej parametru", names(DANE[i]), "dla poszczegolnych grup"))
      print(podsumowanie)
    }else{
      print(paste("CHARAKTER ZMIENNEJ", names(DANE[i]), " -> NIEMIERZALNY"))
    }
  }
}


# funkcje:

rozklad_normalny <- function(numer, dane, grupy){   # test normalnosci rozkladu
  p.val <- c()
  for(i in 1:length(grupy)){
    grupa <- with(dane, dane[grupa == grupy[i],])
    result <- shapiro.test(grupa[,numer])
    p.val <- append(p.val, result[["p.value"]], after = length(p.val))
  } 
  return(p.val)
}

graficzny_rozklad_normalny <- function(numer, dane){  # graficzna ocena zgodnosci z rozkladem normalnym
  ggdensity(dane, x = names(dane[numer]),
            title = "Graficzna ocena zgodnosci z rokladem normalnym",
            color = "grupa",
            fill = "grupa",
            pallete = c("blue", "red", "green"),
            ylab = "Gestosc",
            xlab = names(DANE[numer])
  ) + facet_wrap(~ grupa, scales = "free")
}

jednorodnosc_wariancji <- function(numer, dane){
  p.value <- leveneTest(dane[,numer] ~ grupa, dane)$"Pr(>F)"[1]
  return(p.value)
}


Test_Kruskala <- function(numer){
  p.value <- kruskal.test(DANE[,numer] ~ grupa, DANE)$p.value
  print(kruskal.test(DANE[,numer] ~ grupa, DANE))
  return(p.value)
}

Test_Dunna <- function(numer){
  Tab_Dunna <- dunnTest(DANE[,numer], DANE$grupa)$res
  print(Tab_Dunna)
  for(i in 1:length(Tab_Dunna)){
    if(names(Tab_Dunna[i]) != "Comparison"){
      for(j in 1:nrow(Tab_Dunna[i])){
        if(Tab_Dunna[j,i] < 0.05){
          print(paste("Roznica miedzy", Tab_Dunna[j,1], "w parametrze '", names(Tab_Dunna[i]), "' jest istotna statycznie"))
        }
      }
    }
  }
}

Test_Anova <- function(numer){
  p.value <- summary(aov(DANE[,numer] ~ grupa, DANE))[[1]][["Pr(>F)"]][[1]]
  summary(aov(DANE[,numer] ~ grupa, DANE))
  return(p.value)
}

Test_Tukey <- function(numer){
  TukeyHSD(aov(DANE[,numer] ~ grupa, DANE))
  Tab_Tukey <- as.data.frame(TukeyHSD(aov(DANE[,numer] ~ grupa, DANE))$grupa)
  print(Tab_Tukey)
  for(i in 1:length(Tab_Tukey)){
    for(j in 1:nrow(Tab_Tukey)){
      if(Tab_Tukey[j,i] < 0.05){
        print(paste("Sa istotne roznice statyczne w parametrze", names(Tab_Tukey[i]), "w wierszu", j))
      }
    }
  }
}

Test_Chisq <- function(numer){
  p.value <- chisq.test(DANE$grupa, DANE[,numer])$p.value
  print(chisq.test(DANE$grupa, DANE[,numer]))
  return(p.value)
}

cat("\n\n")
print("POROWNANIE WIECEJ NIZ 2 GRUP NIEZALEZNYCH")

numery_parametryczne_wiele <- c()
a <- 1
numery_nieparametryczne_wiele <- c()
b <- 1

for(i in 1:length(DANE)){
  if(names(DANE[i]) != "grupa"){
    if(is.numeric(DANE[,i])){
      print("_______________________________________________________________")
      print(paste("DLA PARAMETRU", names(DANE[i])))
      print(graficzny_rozklad_normalny(i, DANE))
      print("Wartosci rozkladu normalnego") 
      print(rozklad_normalny(i, DANE, grupy))
      if(sum(rozklad_normalny(i, DANE, grupy) > 0.05) == length(grupy) && length(grupy) > 2){
        print(paste("Grupy sa zgodne z rozkladem normalnym"))
        if(jednorodnosc_wariancji(i, DANE) > 0.05){
          print("Wystepuje jednorodnosc wariancji - TEST ANOVA")
          numery_parametryczne_wiele[a] <- i
          a = a+1
          print(Test_Anova(i))
          if(Test_Anova(i) < 0.05){
            cat("\n")
            print("Test ANOVA wykryl, ze sa roznice miedzy tymi grupami - TEST TUKEY")
            print("Test TUKEY:")
            Test_Tukey(i)
          }else{
            cat("\n")
            print("Test Anova wykryl, ze nie ma roznic miedzy tymi grupami")
          }
        }else{
          print("Nie wystepuje jednorodnosc wariancji - TEST KRUSKALA")
          numery_nieparametryczne_wiele[b] <- i
          b = b+1
          print(Test_Kruskala(i))
          if(Test_Kruskala(i) < 0.05){
            print("Test KRUSKALA wykryl, ze sa roznice miedzy tymi grupami - TEST DUNNA")
            print("Test DUNNA:")
            Test_Dunna(i)
          }else{
            print("Test KRUSKALA wykryl, ze nie ma roznic miedzy tymi grupami")
          }
        }
      }else{
        print(paste("Grupy nie sa zgodne z rozkladem normalnym - TEST KRUSKALA"))
        print("Test Kruskala:")
        numery_nieparametryczne_wiele[b] <- i
        b = b+1
        print(Test_Kruskala(i))
        if(Test_Kruskala(i) < 0.05){
          cat("\n")
          print("Test KRUSKALA wykryl, ze sa roznice miedzy  grupami - TEST DUNNA")
          print("Test DUNNA:")
          Test_Dunna(i)
        }else{
          cat("\n")
          print("Test KRUSKALA wykryl, ze nie ma roznic miedzy grupami")
        }
      }
    }else{
      print(paste("Parametr", names(DANE),  "jest nominalny - TEST CHISQ"))
      print(Test_Chisq(i))
      if(Test_Chisq(i) < 0.05){
        print("Test Chisq wykryl, ze sa znaczace roznice miedzy grupami")
      }else{
        print("Test Chisq wykryl, ze nie ma znaczacych roznic miedzy grupami")
      }
    }
  }
}
cat("\n\n")

# Funkcje:

Test_Wilcoxona <- function(numer){
  p.value <- wilcox.test(Dwie_grupy[,numer] ~ grupa, Dwie_grupy)$p.value
  print(wilcox.test(Dwie_grupy[,numer] ~ grupa, Dwie_grupy))
  return(p.value)
}

Test_Studenta <- function(numer){
  p.value <- t.test(Dwie_grupy[,numer] ~ grupa, Dwie_grupy, var.equal = TRUE)$p.value
  print(t.test(Dwie_grupy[,numer] ~ grupa, Dwie_grupy, var.equal = TRUE))
  return(p.value)
}

Test_Welcha <- function(numer){
  p.value <- t.test(Dwie_grupy[,numer] ~ grupa, Dwie_grupy, var.equal = FALSE)$p.value
  print(t.test(Dwie_grupy[,numer] ~ grupa, Dwie_grupy, var.equal = FALSE))
  return(p.value)
}


print("POROWNANIE DOKLADNIE 2 GRUP NIEZALEZNYCH")

print(paste("Do dyspozycji jest", length(grupy), "grup."))
print("sposrod podanych grup, wybierz numery dwoch grup, ktore chcesz porownac:")
for(i in 1:length(grupy)){
  print(paste(i, ":", grupy[i]))
}
grupa1 <-2
grupa2 <-3
con <- if (interactive()) stdin() else file('stdin')
message('Wybierz grupe pierwsza:')
grupa1 <- scan(file=con, sep=',', nlines=1, quiet=TRUE)

con <- if (interactive()) stdin() else file('stdin')
message('Wybierz grupe druga: ')
grupa2 <- scan(file=con, sep=',', nlines=1, quiet=TRUE)

print(paste("Wybrane grupy:", grupy[grupa1], "i", grupy[grupa2]))

DANE_podzielone <- split(DANE, DANE$grupa)
Dwie_grupy <- full_join(DANE_podzielone[[grupy[grupa1]]], DANE_podzielone[[grupy[grupa2]]])
grupy2 <- unique(Dwie_grupy$grupa)


numery_parametryczne_2 <- c()
a <- 1
numery_nieparametryczne_2 <- c()
b <- 1

for(i in 1:length(Dwie_grupy)){
  if(is.numeric(Dwie_grupy[,i])){
    print("____________________________________________________________________")
    print(paste("DLA PARAMETRU", names(Dwie_grupy[i])))
    print(graficzny_rozklad_normalny(i, Dwie_grupy))
    print("Wartosci rozkladu normalnego") 
    print(rozklad_normalny(i, Dwie_grupy, grupy2))
    if(sum(rozklad_normalny(i, Dwie_grupy, grupy2) > 0.05) == 2){
      print("Grupy sa zgodne z rozkladem normalnym")
      if(jednorodnosc_wariancji(i, Dwie_grupy) > 0.05){
        print("Wystepuje jednorodnosc wariancji")
        numery_parametryczne_2[a] <- i
        a = a+1
        print("Test T.STUDENTA")
        print(Test_Studenta(i))
        if(Test_Studenta(i) < 0.05){
          print("Test T-STUDENTA wykryl, ze sa znaczace roznice miedzy grupami")
        }else{
          print("Test T-STUDENTA wykryl, ze nie ma znaczacych roznic miedzy grupami")
        }
      }else{
        print("Nie wystepuje jednorodnosc wariancji")
        numery_nieparametryczne_2[b] <- i
        b = b+1
        print("Test WELCHA")
        print(Test_Welcha(i))
       if(Test_Welcha(i) < 0.05){
         print("Test WELCHA wykryl, ze sa znaczace roznice miedzy grupami")
       }else{
         print("Test WELCHA wykryl, ze nie ma znaczacych roznic miedzy grupami")
       }
      }
    }else{
      print("Grupy nie sa zgodne z rozkladem normalnym")
      numery_nieparametryczne_2[b] <- i
      b = b+1
      print("Test WILCOXONA")
      print(Test_Wilcoxona(i))
      if(Test_Wilcoxona(i) < 0.05){
        print("Test WILCOXONA wykryl, ze sa znaczace roznice miedzy grupami")
      }else{
        print("Test WILCOXONA wykryl, ze nie ma znaczych roznic miedzy grupami")
      }
    }
  }
}


# TESTY KORELACJI

# funkcje:

Korelacja_Spearmana <- function(dane, grupy, ktora_grupa, parametr1, parametr2){
  grupa <- dane %>% filter(grupa == grupy[ktora_grupa])
  print(paste("Grupa -", grupy[ktora_grupa], "| Parametry:", names(grupa[parametr1]), "-", names(grupa[parametr2])))
  wynik_p.value <- cor.test(grupa[[parametr1]], grupa[[parametr2]], method = "spearman")$p.value
  wynik_cor <- cor.test(grupa[[parametr1]], grupa[[parametr2]], method = "spearman")$estimate
  return(Wspolczynnik_korelacji(wynik_p.value, wynik_cor))
}

Korelacja_Pearsona <- function(dane, grupy, ktora_grupa, parametr1, parametr2){
  grupa <- dane %>% filter(grupa == grupy[ktora_grupa])
  print(paste("Grupa -", grupy[ktora_grupa], "| Parametry:", names(grupa[parametr1]), "-", names(grupa[parametr2])))
  wynik_p.value <- cor.test(grupa[[parametr1]], grupa[[parametr2]], method = "pearson")$p.value
  wynik_cor <- cor.test(grupa[[parametr1]], grupa[[parametr2]], method = "pearson")$estimate
  return(Wspolczynnik_korelacji(wynik_p.value, wynik_cor))
}

Wspolczynnik_korelacji <- function(p, r){
  print(paste("Wyniki:"))
  if(p < 0.05){
    print(paste("P.value =", p, "< 0.05 - Korelacja miedzy zmiennymi"))
    print(paste("Rodzaj korelacji:"))
    if(r > 0){
      print(paste("r =", r, "> 0 - Korelacja Dodatnia"))
    }else if(r == 0){
      print(paste("r =", r, "= 0 - Brak korelacji"))
    }else{
      print(paste("r =", r, "< 0 - Korelacja Ujemna"))
    }
    
    print(paste("Sila korelacji:"))
    if(-1 < r && r < -0.7){
      print(paste(r, "- Bardzo silna korelacja ujemna"))
    }else if(-0.7 < r && r < -0.5){
      print(paste(r, "- Silna korelacja ujemna"))
    }else if(-0.5 < r && r < -0.3){
      print(paste(r, "- Korelacja ujemna o srednim stezeniu"))
    }else if(-0.3 < r && r < -0.2){
      print(paste(r, "- Slaba korelacja ujemna"))
    }else if(-0.2 < r && r < 0.2){
      print(paste(r, "- Brak korelacji"))
    }else if(0.2 < r && r < 0.3){
      print(paste(r, "- Slaba korelacja dodatnia"))
    }else if(0.3 < r && r < 0.5){
      print(paste(r, "- Korelacja dodatnia o srednim stezeniu"))
    }else if(0.5 < r && r < 0.7){
      print(paste(r, "- Silna korelacja dodatnia"))
    }else if(0.7 < r && r < 1){
      print(paste(r, "- Bardzo silna korelacja dodatnia"))
    }
  }else{
    print(paste("P.value =", p, ">= 0.05 - Brak korelacji miedzy zmiennymi"))
  }
}

Graficznie_Korelacja <- function(dane, grupy, ktora_grupa, parametr1, parametr2, metoda){
  grupa <- dane %>% filter(grupa == grupy[ktora_grupa])
  #jpeg(paste("Wykres_", grupy[ktora_grupa], "_parametry_", names(grupa[parametr1]), "-", names(grupa[parametr2]), ".jpg"), width = 600, height = 400)
    ggscatter(grupa, x = names(grupa[parametr1]), y = names(grupa[parametr2]),
            title = paste("Wykres dla metody", metoda, "dla parametrow", names(grupa[parametr1]), "-", names(grupa[parametr2])),
            add = "reg.line", conf.int = TRUE,
            cor.coef = TRUE, cor.method = metoda,
            color = "grupa", fill = "grupa",
            ylab = names(grupa[parametr1]),
            xlab = names(grupa[parametr2])
  )
  #dev.off()
}
parametr1 <- 5
parametr2 <- 7
ktora_grupa <- 2
gol <- "TEST"
numer <- 2
jpeg(paste("wykres",grupy[2], "gestosci.jpg"), width = 600, height = 400)
#jpeg(paste("Wykres_", grupy[ktora_grupa], "_parametry_", names(grupa[parametr1]), "-", names(grupa[parametr2]), ".jpg"), width = 600, height = 400)
  Graficznie_Korelacja(DANE, grupy, 2, 5,7, "spearman")
dev.off()


# TESTY KORELACJI DLA >2 GRUP
cat("\n\n")

# TEST PEARSONA
print("TESTY PARAMETRYCZNE KORELACJI DLA WIECEJ NIZ 2 GRUP")
print("TEST PEARSONA")
start <- 2
for(i in 1:length(DANE)-1){
  for(j in start:length(DANE)){
    if(i != j && i-1 != j && is.numeric(DANE[,i]) && is.numeric(DANE[,j])){
      if( i %in% numery_parametryczne_wiele && j %in% numery_parametryczne_wiele){
        for(grupa in 1:length(grupy)){
          Korelacja_Pearsona(DANE, grupy, grupa, i, j)
          jpeg(paste("Wykres_", grupy[grupa], "_parametry_", names(DANE[i]), "-", names(DANE[j]), "gestosc.jpg"), width = 600, height = 400)
            Graficznie_Korelacja(DANE, grupy, grupa, i, j, "pearson")
          dev.off()
          cat("\n")
        }
      }
    }
  }
}

#TEST SPEARMANA
print("TESTY NIEPARAMETRYCZNE KORELACJI DLA WIECEJ NIZ 2 GRUP")
print("TEST SPEARMANA")
start <- 2
for(i in 1:length(DANE)-1){
  for(j in start:length(DANE)){
    if(i != j && i-1 != j && is.numeric(DANE[,i]) && is.numeric(DANE[,j])){
      if( i %in% numery_parametryczne_wiele && j %in% numery_parametryczne_wiele){
      }else{
        for(grupa in 1:length(grupy)){
          Korelacja_Spearmana(DANE, grupy, grupa, i, j)
          print(Graficznie_Korelacja(DANE, grupy, grupa, i, j, "spearman"))
          cat("\n")
        }
      }
    }
  }
}


# TESTY KORELACJI DLA 2 GRUP

# TEST PEARSONA
print("TESTY PARAMETRYCZNE KORELACJI DLA DOKLADNIE 2 GRUP")
print("TEST PEARSONA")
start <- 2
for(i in 1:length(Dwie_grupy)-1){
  for(j in start:length(Dwie_grupy)){
    if(i != j && i-1 != j && is.numeric(Dwie_grupy[,i]) && is.numeric(Dwie_grupy[,j])){
      if( i %in% numery_parametryczne_2 && j %in% numery_parametryczne_2){
        for(grupa in 1:length(grupy2)){
          Korelacja_Pearsona(Dwie_grupy, grupy2, grupa, i, j)
          print(Graficznie_Korelacja(Dwie_grupy, grupy2, grupa, i, j, "pearson"))
          cat("\n")
        }
      }
    }
  }
}

#TEST SPEARMANA
print("TESTY NIEPARAMETRYCZNE KORELACJI DLA DOKLADNIE 2 GRUP")
print("TEST SPEARMANA")
start <- 2
for(i in 1:length(Dwie_grupy)-1){
  for(j in start:length(Dwie_grupy)){
    if(i != j && i-1 != j && is.numeric(Dwie_grupy[,i]) && is.numeric(Dwie_grupy[,j])){
      if( i %in% numery_parametryczne_2 && j %in% numery_parametryczne_2){
      }else{
        for(grupa in 1:length(grupy2)){
          Korelacja_Spearmana(Dwie_grupy, grupy2, grupa, i, j)
          print(Graficznie_Korelacja(Dwie_grupy, grupy2, grupa, i, j, "spearman"))
          cat("\n")
        }
      }
    }
  }
}

print("KONIEC")

