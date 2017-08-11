# Skrypt pobiera dane o kolejnych notowaniach Listy Przebojów Trójki
# Wynki to plik notowania_raw.RDS z zapisanymi notowaniami

library(rvest)
library(dplyr)
library(lubridate)

lp3_base_url <- "http://www.lp3.pl/alpt.phtml?m=1&nn="

max_nn <- 1825

notowania <- data.frame()

# dla każdej kolejnej strony
for(curr_nn in 1:max_nn) {

  # progress bar :)
  cat(paste0("\r", curr_nn, " / ", max_nn))


  # budujemy URL strony
  lp3_url <- paste0(lp3_base_url, curr_nn)

  # wczytujemy stronę
  page <- read_html(lp3_url)

  # informacje o notowaniu
  notowanie_info <- page %>%
    html_node("div#view-not") %>%
    html_nodes("p") %>%
    html_text()

  notowanie_info <- gsub(",", " ", notowanie_info)
  notowanie_info <- gsub("[ ]+", " ", notowanie_info)
  notowanie_info <- unlist(strsplit(notowanie_info, "[ .]"))


  # tabela z notowaniem
  notowanie_tabela <- page %>%
    html_node("div#view-not") %>%
    html_node("div#zestawienie") %>%
    html_node("table") %>%
    html_table()

  colnames(notowanie_tabela) <- c("PozAkt", "PozPop", "TygNotowan", "Song", "Kraj", "Zmiana")


  # odrzucamy piosenki z poczekalni
  notowanie_tabela <- filter(notowanie_tabela, PozAkt!="poczekalnia")


  # czy coś zostało? zapisujemy potrzebne dane
  if(nrow(notowanie_tabela) > 0) {
    notowanie_tabela$PozAkt <- as.integer(notowanie_tabela$PozAkt)
    notowanie_tabela$PozPop <- as.integer(notowanie_tabela$PozPop)
    notowanie_tabela$TygNotowan <- as.integer(notowanie_tabela$TygNotowan)

    notowanie_tabela$Song <- gsub(" &mdmdash; ", " - ",  notowanie_tabela$Song)

    notowanie_tabela$NotowanieNumer <- notowanie_info[2]
    notowanie_tabela$NotowanieRok <- as.integer(notowanie_info[6])
    notowanie_tabela$NotowanieMiesiac <- as.integer(notowanie_info[5])
    notowanie_tabela$NotowanieDzien <- as.integer(notowanie_info[4])
    notowanie_tabela$NotowanieProwadzacy <- paste(notowanie_info[10], notowanie_info[11])

    notowania <- rbind(notowania, notowanie_tabela)
  }
}


# uporządkowanie danych
notowania$NotowanieProwadzacy <- ifelse(notowania$NotowanieProwadzacy=="emisji NA", "brak emisji", as.character(notowania$NotowanieProwadzacy))
notowania$NotowanieProwadzacy <- as.factor(notowania$NotowanieProwadzacy)

notowania$Kraj <- as.factor(notowania$Kraj)
notowania$NotowanieData <- make_date(notowania$NotowanieRok,
                                     notowania$NotowanieMiesiac,
                                     notowania$NotowanieDzien)

# rozdzielamy piosenkę na Wykonwacę i Tytuł
notowania <- notowania %>%
  separate(Song, c("Artist", "Title"), sep = " — ", extra = "merge")

# usuwamy zbędne spacje
notowania$Title <- trimws(notowania$Title)
notowania$Artist <- trimws(notowania$Artist)


# modyfikujemy nazwę dla jednego wykonawcy - żeby ładnie mieściło się na wykresach
notowania$Artist <- gsub("SPECIAL GUITAR PERFORMANCE BY ", "", notowania$Artist, fixed = TRUE)


# zapisujemy dane do pliku
saveRDS(notowania, file = "notowania_raw.RDS")
