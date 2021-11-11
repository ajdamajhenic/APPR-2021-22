# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Stopnja brezposelnosti v Sloveniji in drugih evropskih državah

Pri projektni nalogi bom primerjala stopnjo brezposelnosti od leta 2008 do leta 2018 v Sloveniji in ostalih evropskih državah glede na spol in stopnjo izobrazbe, starostna doba bo pa med 20. in 64. letom. Pogledala bom, kako je v Sloveniji na stopnjo brezposelnosti vplivala stopnja izobrazbe ter kolikšna je brezposelnost glede na statistično regijo.

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
