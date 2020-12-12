#' Funkcja liczaca emisje. Na podstawie danych z EEA.
#'
#' Przyjmuje nastpujepujace argumenty:
#'
#' @param dane - data frame, sklada sie z 50 wierszy o kolumnach: (Nat, segment, paliwo, technologia)
#' - nat, to liczba losowo generowana
#' - segment, character w tej kolumnie losowane sa wartosci sposrod 4 dostepnych
#' parametrów: "Mini", "Small", "Medium", "Large-SUV-Executive"
#' - paliwo, okresla sie rodzaj paliwa
#' - technologia, okresla rodzaj technologii oczyszczania spalin
#' @param kategoria - character okresla kategorie pojazdu
#' @param euro - Europejski standard emisji spalin
#' @param mode - zawiera rodzaj drogi
#' @param substancja - wektor character, przyjmuje substancje dla ktorych chcemy szacowac emisje
#' aby zobaczyc liste dostepnych wartosci dla poszczegolnych parametrow nalezy uzyc
#' funkcji unique dla danych "wskazniki"
#' np. unique(wskazniki$Category)
#'
#' @return data frame z obliczona emisja
#' @import magrittr
#'
#' @export
WWemisja <- function(dane = input,
                     kategoria = "Passenger Cars",
                     paliwo = "Petrol",
                     segment = "Mini",
                     euro = "Euro 5",
                     technologia = "GDI",
                     mode = "",
                     substancja = c("EC", "CO")) {

  # Zabezpieczenia argumentów funkcji i danych wejscioweych.

  input <- data.frame(Nat = rnorm(50, mean = 100, sd = 50),
                      Segment = sample(c("Mini", "Small", "Medium", "Large-SUV-Executive"),
                                       size = 50, replace = T),
                      paliwo = "Petrol",
                      technologia = "GDI")

  input$Segment <- as.character(input$Segment)

  out <- wskazniki %>%
    filter(Category %in% kategoria) %>%
    filter(Fuel %in% paliwo) %>%
    filter(Euro.Standard %in% euro) %>%
    filter(Technology %in% technologia) %>%
    filter(Pollutant %in% substancja) %>%
    filter(Mode %in% mode)

  out <- inner_join(x = out, y = input, by = "Segment")

  out <- out %>%
    mutate(Emisja = Nat * ((Alpha * Procent ^ 2 + Beta * Procent + Gamma + (Delta/Procent))/
                             (Epsilon * Procent ^ 2 + Zita * Procent + Hta) * (1-Reduction))
    ) %>%
    select(Category, Fuel, Euro.Standard, Technology, Pollutant, Mode, Segment, Nat, Emisja)

  return(out)

}

