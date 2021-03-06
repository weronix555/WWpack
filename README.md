README
================
Weronika Wojtanek
11 12 2020

## Wstęp

Pakiet został stworzony na zaliczenie ćwiczeń w ramach przedmiotu
“Metody Inwentaryzacji i Szacowania Emisji”. Kod źródłowy został
zaimplementowany w języku programowania R. Pakiet służy do wyznaczania
emisji zanieczyszczeń wytwarzanych przez pojazdy z silnikami
spalinowymi. Stworzone funkcje korzystają domyślnie z danych dołączonych
do pakietu. Plik źródłowy z surowymi danymi został na początku
przetworzony i zapisany do obiektu wskaźniki.

## Instalacja pakietu

``` r
#devtools::install_github("weronix555/WWpack",force=TRUE)
```

## Zastosowanie

Dzięki pakietowi mamy możliwość natychmiastowego obliczenia emisji
zanieczyszczeń wytwarzanych przez pojazdy z silnikami spalinowymi.
Dodatkowo druga funkcja pakietu narysuje nam wykres rozrzutu natężenia
do emisji wg.rodzaju zanieczyszczenia. Jedyne co trzeba zrobić to
uruchomić funkcje w następujący sposób:

``` r
#WWemisja()
#WWplot()
```

## Dokładny opis pakietu

Innym sposobem poszerzenia wiedzy na temat możliwości pakietu jest
zapoznania się z winietą, którą można uruchomić w programie RStudio za
pomocą polecenia:

``` r
#browseVignettes("WWpack")
```
