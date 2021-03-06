% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WWemisja.R
\name{WWemisja}
\alias{WWemisja}
\title{Funkcja liczaca emisje. Na podstawie danych z EEA.}
\usage{
WWemisja(
  dane = input,
  kategoria = "Passenger Cars",
  paliwo = "Petrol",
  segment = "Mini",
  euro = "Euro 5",
  technologia = "GDI",
  mode = "",
  substancja = c("EC", "CO")
)
}
\arguments{
\item{dane}{- data frame, sklada sie z 50 wierszy o kolumnach: (Nat, segment, paliwo, technologia)
- nat, to liczba losowo generowana
- segment, character w tej kolumnie losowane sa wartosci sposrod 4 dostepnych
parametrów: "Mini", "Small", "Medium", "Large-SUV-Executive"
- paliwo, okresla sie rodzaj paliwa
- technologia, okresla rodzaj technologii oczyszczania spalin}

\item{kategoria}{- character okresla kategorie pojazdu}

\item{euro}{- Europejski standard emisji spalin}

\item{mode}{- zawiera rodzaj drogi}

\item{substancja}{- wektor character, przyjmuje substancje dla ktorych chcemy szacowac emisje
aby zobaczyc liste dostepnych wartosci dla poszczegolnych parametrow nalezy uzyc
funkcji unique dla danych "wskazniki"
np. unique(wskazniki$Category)}
}
\value{
data frame z obliczona emisja
}
\description{
Przyjmuje nastpujepujace argumenty:
}
