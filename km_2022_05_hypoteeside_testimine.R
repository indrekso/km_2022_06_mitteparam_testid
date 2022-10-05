# Hüpoteeside testimine, t-testid: praktikum
# Kvantitatiivsed meetodid

library(haven)
library(tidyverse)
library(summarytools)

r8 <- read_spss("data/ESS8e02_2.sav")
ee8 <- r8 %>% 
  filter(cntry == "EE")

#### Sõltumatute kogumite t-test ####

# Kas mehed ja naised peavad vanadele inimestele rahuldava elustandardi tagamist samavõrd valitsuse kohustuseks?

attributes(ee8$gvslvol)
descr(ee8$gvslvol)
t.test(gvslvol ~ gndr, data = ee8)

# Kas mehed ja naised peavad töötutele rahuldava elustandardi tagamist samavõrd valitsuse kohustuseks?

attributes(ee8$gvslvue)
descr(ee8$gvslvue)
t.test(gvslvue ~ gndr, data = ee8)

# Kas mehed ja naised peavad lastehoiu teenuste tagamist samavõrd valitsuse kohustuseks?

attributes(ee8$gvcldcr)
descr(ee8$gvcldcr)
t.test(gvcldcr ~ gndr, data = ee8)

# Esimene näide kaaludega

ee8_gndr_1 <- filter(ee8, gndr == 1)
ee8_gndr_2 <- filter(ee8, gndr == 2)

library(weights)
wtd.t.test(ee8_gndr_1$gvslvol, ee8_gndr_2$gvslvol, weight = ee8_gndr_1$pspwght, weighty = ee8_gndr_2$pspwght)

# Ülesanne 1

# Kas inimeste, kes tajuvad end diskrimineerituna, usaldus erinevate institutsioonide suhtes keskmiselt erineb nendest, kes enda suhtes diskrimineerimist ei tunneta? Kuna teete t-testi R-s esimest korda, siis lihtsuse mõttes tehke arvutused andmeid kaalumata.

# Ülesanne 2

# Kas riigikogu valimistel hääletanute usaldus erinevate institutsioonide suhtes on keskmiselt erinev neist, kes omasid valimisõigust, aga valimistel ei hääletanud? Proovige siin teha arvutused kaalutud andmetega.


#### Ühepoole hüpoteesipaari testimine ####

# Uurime, kas meeste ja naiste eluga rahulolu keskmiselt erineb. Teeme testi läbi kaalutud andmetega.

library(haven)
library(tidyverse)
library(summarytools)

r8 <- read_spss("data/ESS8e02_2.sav")
ee8 <- r8 %>% 
  filter(cntry == "EE")

attributes(ee8$gvslvue)

ee8_gndr_1 <- filter(ee8, gndr == 1)
ee8_gndr_2 <- filter(ee8, gndr == 2)

library(weights)

# Vaikimisi testitakse kahepoolset hüpoteesipaari

wtd.t.test(ee8_gndr_1$stflife, ee8_gndr_2$stflife, weight = ee8_gndr_1$pspwght, weighty = ee8_gndr_2$pspwght)

# Saame seadistada ühepoolse hüpoteesipaari ehk täpsustada, kas alternatiivhüpoteesiga väidame, et esimese grupi keskmine on väiksem...

wtd.t.test(ee8_gndr_1$stflife, ee8_gndr_2$stflife, weight = ee8_gndr_1$pspwght, weighty = ee8_gndr_2$pspwght, alternative = "less")

# ...või suurem

wtd.t.test(ee8_gndr_1$stflife, ee8_gndr_2$stflife, weight = ee8_gndr_1$pspwght, weighty = ee8_gndr_2$pspwght, alternative = "greater")


#### Paariskogumite t-test ####

# Kas õnnelikkuse ja eluga rahulolu hinnangud on keskmiselt erinevad?

descr(ee8$happy)
descr(ee8$stflife)

t.test(Pair(happy, stflife) ~ 1, data = ee8)

# Kaaludega

ee8$happy_stflife <- ee8$happy - ee8$stflife
wtd.t.test(ee8$happy_stflife, 0, weight = ee8$pspwght)


# Harjutusülesanne: kas usaldus parlamendi vastu keskmiselt erineb usaldusest parteide vastu? Aga kas usaldus poliitikute vastu keskmiselt erineb usaldusest parteide vastu? Tehke ülesanne kaalutud andmetega.

#### Ühe kogumi t-test ####

# Kas eluga rahulolu on skaala keskpunktist erinev?

t.test(stflife ~ 1, data = ee8)

# Kaaludega

wtd.t.test(ee8$stflife, 5, weight = ee8$pspwght)
