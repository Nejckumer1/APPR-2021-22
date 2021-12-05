# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Tematika

### Analiza športno-vzgojnih kartonov

Analiziral bom športno-vzgojne kartone osnovnošolcev, pri čemer bom za parametre analize vzel:
- spol otroka,
- starost otroka,
- leto testiranja.
Podatke sem pridobil v arhivu Fakultete za šport v Ljubljani, cilj analize pa je pregled in razlike v dosežkih osnovnošolcev
ter okvirna napoved gibalno-fizičnih sposobnosti otrok v prihodnjih letih.

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
