# Kvantitatiivsed meetodid
# Mitteparameetrilised testid


#### Kolmogorov-Smirnovi test ####
# Kahe teineteisest sõltumatu kogumi võrdlus pideva tunnuse alusel

# Teeme läbi Toodingu (2015) õpikus oleva näite Euroopa Parlamendi usaldushinnangute võrdlusest Põhja- ja Lääne-Eesti vahel (lk 174-175).

library(haven)
r6 <- read_spss("data/ESS6e02_4.sav")

ee6 <- r6 %>% 
  filter(cntry == "EE")

library(tidyverse)
ee6$region <- recode(as.factor(ee6$region), 
                     "EE001" = "Põhja-Eesti",
                     "EE004" = "Lääne-Eesti",
                     "EE006" = "Kesk-Eesti",
                     "EE007" = "Kirde-Eesti",
                     "EE008" = "Lõuna-Eesti")

library(summarytools)
ctable(ee6$trstep, ee6$region, prop = "c", useNA = "no")

# Kuidas teha samasugust joonist nagu Toodingu õpikus lk 174?

ee6kk <- ee6 %>% 
  select(trstep, region) %>% 
  filter(region == "Põhja-Eesti" | region == "Lääne-Eesti")

trstep_kesk <- ee6kk %>% 
  group_by(region) %>% 
  summarise(keskm = mean(trstep, na.rm = TRUE))

library(ggplot2)
ggplot(ee6kk, aes(trstep, col = region)) +
  stat_ecdf(geom = "line") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Usaldus Euroopa parlamendi vastu, punkti") +
  ylab("Kumulatiivne jaotus, %") +
  geom_vline(data = trstep_kesk, aes(xintercept = keskm, colour = region))

trstep_p <- ee6 %>% 
  subset(region == "Põhja-Eesti") %>% 
  pull(trstep)

trstep_l <- ee6 %>% 
  subset(region == "Lääne-Eesti") %>% 
  pull(trstep)

ks.test(trstep_p, trstep_l)

# t-test annaks järgmise tulemuse (sama, mis Tooding (2015: 163), Tabel 5.2, 3. andmeveerg):

t.test(trstep_p, trstep_l)

# Mida see 0.0817 ikkagi näitab?

freq(trstep_p)
frex <- freq(trstep_p)
cumx <- frex[1:11, 3]

frey <- freq(trstep_l)
cumy <- frey[1:11, 3]

erinevused <- cumx - cumy
max(erinevused)

# Tulebki välja, et suurimat erinevust tunnuse kumulatiivsetes jaotustes kahe grupi vahel


#### Kolmogorovi-Smirnovi testi ja t-testi võrdlus ####

# Ülesanne 1

# Mis on K-S testi eelis t-testi ees? Laadige R-i valimiandmed palkade kohta kahes grupis (andmed on väljamõeldud). Kas palgad nendes kahes grupis erinevad? Millise vastuseni jõuate Kolmogorovi-Smirnovi testi põhjal, millise vastuseni t-testi põhjal? Miks tulemused erinevad? Uurige selleks palkade jaotusi või jaotusparameetreid kahes grupis.

# Andmete laadimine
palgad <- read.csv("https://kodu.ut.ee/~indrekso/palgad.csv")
View(palgad)
# Edasi tehke iseseisvalt, vajadusel küsige abi. Kui jõuate ühele poole, teen Moodle's avalikuks ka ülesande võimaliku lahenduse.


#### Kolmogorovi-Smirnovi testi alusel jaotuse võrdlemine etteantud jaotusega ####

# Kuidas hinnata K-S testi alusel, kas tunnuse jaotus vastab normaaljaotusele? Vaatame kõigepealt, kas töötasu tunnus ESS9-s vastab normaaljaotusele.

ee9 <- import_country("Estonia", 9)

# Vaatame kõigepealt tunnuse jaotust graafikul (tihedusjaotus)

ggplot(ee9, aes(grspnum)) +
  geom_density()

# Kontrollime jaotusparameetreid

descr(ee9$grspnum)

# Jaotusparameetrite järgi peaks tegelikult asi selge olema, vaatame ka, mida Kolmogorov ja Smirnov kostavad
# Salvestame töötasu tunnuse jaotusparameetrid, et K-S testile ette anda normaaljaotuse parameetrid, millega võrrelda töötasu tegelikku jaotust

grspnum_descr <- descr(ee9$grspnum)

# ja kasutame neid käsus ks.test kolmanda ja neljanda argumendina (muidu võrdleb ks.test töötasu jaotust standardsega normaaljaotusega (m = 0, sd = 1), mis on kaugel võrdluse mõttest)

ks.test(ee9$grspnum, "pnorm", grspnum_descr[1], grspnum_descr[2])

# Nonii, Kolmogorovi-Smirnovi test nõustub eelnevaga. Vaatame ka graafiliselt, kuidas töötasu jaotus erineb normaaljaotusest 

ggplot(ee9, aes(grspnum)) +
  geom_density() +
  stat_function(fun = dnorm, args = list(mean = grspnum_descr[1], sd = grspnum_descr[2]))

# Teeme läbi ka Toodingu õpikus oleva näite Põhja-Eesti elanike usaldushinnangutega Euroopa Parlamendile aastal 2012 (lk 172-173)

ee6 %>% 
  subset(region == "Põhja-Eesti") %>% 
  pull(trstep) %>% 
  descr()

trstep1_descr <- ee6 %>% 
  subset(region == "Põhja-Eesti") %>% 
  pull(trstep) %>% 
  descr()

View(trstep1_descr)

ee6 %>% 
  subset(region == "Põhja-Eesti") %>% 
  pull(trstep) %>% 
  ks.test("pnorm", trstep1_descr[1, 1], trstep1_descr[2, 1])

# K-S testi järgi justkui ei oleks tegu normaaljaotusega, samas eelnevast on näha, et asümmeetriakordaja on -0,17 ja järskuskordaja -0,46. Probleem - suurema valimi puhul ilmneb lahknevus normaaljaotusest juba väikeste kõrvalekallete puhul. Jaotuse graafik näitab seda hästi:

ggplot(ee6[ee6$region == "Põhja-Eesti",], aes(trstep)) +
  geom_histogram(aes(y = ..density..), colour = "blue", fill = "white", bins = 11) +
  stat_function(fun = dnorm, args = list(mean = trstep1_descr[1], sd = trstep1_descr[2]))

# Oluline: statistilisi teste saab kasutada, et hinnata mingi tunnuse jaotuse vastavust etteantavale jaotusele (nt normaaljaotusele), aga tulemus (statistilise olulisuse poolest) sõltub indiviidide arvust. Kindlam on anda hinnang tunnuse jaotusparameetrite või jaotuse visuaalse vaatluse põhjal. See on subjektiivne, aga antud juhul parem kui pimesi olulisuse tõenäosuse usaldamine.


#### Kruskali-Wallise test ####

# Teeme läbi samad Kruskali-Wallise testi arvutused, mis Tooding (2015: 185) ehk uurime, kas Eestis regiooniti on erinevusi Riigikogule ja EP-le antud usaldushinnangutes ja üldises eluga rahulolus (kui vaadata jaotusparameetreid, siis ilmneb, et tunnuste jaotused ei ole normaaljaotusest kuigi kaugel, nii et siin ei ole otseselt vajadust mitteparameetriliste meetodite järele, aga teeme selle lihtsalt näite korras läbi)

kruskal.test(as.numeric(trstprl) ~ region, data = ee6)
kruskal.test(as.numeric(trstep) ~ region, data = ee6)
kruskal.test(as.numeric(stflife) ~ region, data = ee6)

# Kuidas saadud tulemusi tõlgendada?

# Tehniline selgitus 1: mida tähendab viimase testi tulemustes p-value = 6.179e-06? Vaata, millise tulemuse annavad järgmised tehted:

6.179 * 10^-6
6.179 * 0.000001
0.000006179

# Tehniline selgitus 2: miks kasutasime siin funktsiooni as.numeric? Vaata, mida ütleb testi abifail (argument x):

?kruskal.test

# Tunnuses peaks ju olema arvulised andmed? Vaata, mis tüüpi andmetega on tegu

class(ee6$stflife)
class(as.numeric(ee6$stflife))

# Kruskali-Wallise testiga saime teada, et vähemalt üks grupp erineb teistest eluga rahulolu poolest, aga milline/millised? Saaksime siin põhimõtteliselt uurida regioonide paare, et seda kindlaks teha. Näide:

# Wilcoxoni astaksummatest (aka Manni-Whitney U-test, sarnane Kruskali-Wallise testile, aga mõeldud kahe kogumi võrdlemiseks)

ee6 %>% 
  filter(region == "Lõuna-Eesti" | region == "Kirde-Eesti") %>% 
  wilcox.test(stflife ~ region, data = .)

# Kui teha Kruskali-Wallise test ainult kahele grupile, saame erineva teststatistiku, aga olulisuse tõenäosus on sama, nii et pmst annab sama tulemuse (teeme sama järelduse)

ee6 %>% 
  filter(region == "Lõuna-Eesti" | region == "Kirde-Eesti") %>% 
  kruskal.test(as.numeric(stflife) ~ region, data = .)

# Saaksime nii võrrelda kõiki regioone paariti, aga eeldab, et võtame arvesse mitmese testimise eripära. Üks võimalus selleks on kasutada funktsiooni pairwise.wilcox.test, mis viib läbi küll sama Wilcoxoni testi, mida eelnevalt kasutasime, aga korrigeerib olulisuse tõenäosused, võttes arvesse mitmese võrdluse aspekti. Kasutada saab erinevaid korrigeerimisalgoritme (Bonferroni on vanim ja klassikaline, aga äärmuslikult konservatiivne), vaikeseade on "holm".

pairwise.wilcox.test(ee6$stflife, ee6$region, p.adjust.method = "bonferroni")
pairwise.wilcox.test(ee6$stflife, ee6$region, p.adjust.method = "holm")


#### Märgitest ####

# Uurime, kas vanused, mida peetakse ideaalseks partneriga kooselu alustamiseks ja abiellumiseks, erinevad. Kuna jaotustes esineb mitmeid suuri sagedusi ja jaotus on ebaühtlane ega ole lähedane normaaljaotusele, ei ole siin hea kasutada paariskogumite t-testi. Mitteparameetriline alternatiiv ongi märgitest või Wilcoxoni astakmärgitest.

attributes(ee9$iaglptn)
freq(ee9$iaglptn)
attributes(ee9$iagmr)
freq(ee9$iagmr)

iage <- ee9 %>% 
  filter(iaglptn > 0 & iaglptn < 60 & iagmr > 2 & iagmr < 60) %>% 
  select(iaglptn, iagmr)

library(coin)
sign_test(iaglptn ~ iagmr, data = iage)

#### Wilcoxoni astakmärgitest

wilcox.test(Pair(iagmr, iaglptn) ~ 1, data = iage)

# Kui soovime ühepoolset hüpoteesipaari testida

wilcox.test(Pair(iagmr, iaglptn) ~ 1, data = iage, alternative = "greater")
wilcox.test(Pair(iagmr, iaglptn) ~ 1, data = iage, alternative = "less")


