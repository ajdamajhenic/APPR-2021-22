# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Stopnja brezposelnosti v Sloveniji in drugih evropskih državah

Pri projektni nalogi bom primerjala stopnjo brezposelnosti od leta 2011 do leta 2020 v Sloveniji in ostalih evropskih državah glede na spol in stopnjo izobrazbe, starostna doba bo pa med 20. in 64. letom. Pogledala bom, kako je v Sloveniji na stopnjo brezposelnosti vplivala stopnja izobrazbe ter kolikšna je brezposelnost glede na statistično regijo.


Tabela 1: Stopnja brezposelnosti glede na stopnjo izobrazbe
* leto (število)
* kohezijska regija (niz)
* spol (niz)
* izobrazba (niz)
* stopnja brezposelnosti (število)
* vir : https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0762112S.px

Tabela 2: Stopnja brezposelnosti po statističnih regijah
* leto (število)
* statistična regija (niz)
* stopnja brezposelnosti (število)
* vir: https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0762111S.px

Tabela 3: Stopnja brezposelnosti v evropskih državah
* leto (število)
* država (niz)
* spol (niz)
* stopnja izobrazbe (niz)
* stopnja brezposelnosti (število)
* vir: https://ec.europa.eu/eurostat/databrowser/view/une_educ_a/default/table?lang=en

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
