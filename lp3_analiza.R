# Skrypt z danych zgromadzonych przez get_lp3_data.R przygotowuje serie wykresow i tabelek
# Pobiera tez tagi z LastFM dla poszczegolnych utworow


lastkey <- "xxxx" # wpisz SWÓJ!

library(tidyverse)
library(lubridate)

library(ggrepel) # dla ładnego umiejscowienia labelek na wykresach


# dane zapisane w wyniku działania skryptu pobierajcego dane
notowania <- readRDS("notowania_raw.RDS")

notowania_analiza <- notowania %>%
  # zostawiamy tylko potrzebne kolumny
  select(PozAkt, Artist, Title, NotowanieNumer, NotowanieData, NotowanieProwadzacy, Kraj) %>%
  # nadanie punktacji
  filter(PozAkt <= 30) %>%
  mutate(Punkty = 31-PozAkt)



#### PROWADZĄCY NOTOWANIA ####

# prowadzący kolejne notowania
prowadzacy <- notowania_analiza %>%
  select(NotowanieData, NotowanieProwadzacy) %>%
  distinct()


# kto ile notowań prowadził?
prowadzacy %>%
  group_by(NotowanieProwadzacy) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(Prowadzacy=factor(NotowanieProwadzacy, levels=NotowanieProwadzacy)) %>%
  ggplot() +
  geom_bar(aes(Prowadzacy, n), stat="identity", fill = "lightgreen", color = "black") +
  theme(axis.text.x = element_text(angle=90, hjust=1))


# podział notowań w roku pomiędzy osoby
prowadzacy %>%
  mutate(Rok=year(NotowanieData)) %>%
  group_by(Rok, NotowanieProwadzacy) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  group_by(Rok) %>%
  mutate(p=100*n/sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(Rok, p, fill=NotowanieProwadzacy), stat="identity", color = "black") +
  theme(legend.position = "bottom")



#### PIOSENKI ####

# najopularniejsze piosenki (top wszechczasów)
notowania_analiza %>%
  group_by(Artist, Title) %>%
  summarise(Punkty = sum(Punkty)) %>%
  ungroup() %>%
  top_n(30, Punkty) %>%
  arrange(desc(Punkty)) %>%
  mutate(Song = paste(Artist, Title, sep = " - ")) %>%
  mutate(Song = factor(Song, levels = Song)) %>%
  ggplot() +
  geom_bar(aes(Song, Punkty), stat = "identity", fill = "lightgreen", color = "black") +
  theme(axis.text.x = element_text(angle=90, hjust=1))


# Top 30 polskie
notowania_analiza %>%
  filter(Kraj == "PL") %>%
  group_by(Artist, Title) %>%
  summarise(Punkty = sum(Punkty)) %>%
  ungroup() %>%
  top_n(30, Punkty) %>%
  arrange(desc(Punkty)) %>%
  mutate(Song = paste(Artist, Title, sep = " - ")) %>%
  mutate(Song = factor(Song, levels = Song)) %>%
  ggplot() +
  geom_bar(aes(Song, Punkty), stat="identity", fill = "lightgreen", color = "black") +
  theme(axis.text.x = element_text(angle=90, hjust=1))


# najpopularniejszy artysta całej listy - wszystko
notowania_analiza %>%
  group_by(Artist) %>%
  summarise(Punkty = sum(Punkty)) %>%
  ungroup() %>%
  arrange(desc(Punkty), Artist) %>%
  top_n(30, Punkty) %>%
  mutate(Artist = factor(Artist, levels = Artist)) %>%
  ggplot() +
  geom_bar(aes(Artist, Punkty), stat="identity", fill = "lightgreen", color = "black") +
  theme(axis.text.x = element_text(angle=90, hjust=1))


# Lista najlepszych artystów - odczytana z wykresu
top_artists <- c("U2", "HEY", "KULT", "MADONNA", "DEPECHE MODE",
                 "MAANAM", "COLDPLAY", "LADY PANK", "METALLICA",
                 "REPUBLIKA", "STING", "PEARL JAM")


# najpopularniejszy artysta całej listy - Polska
notowania_analiza %>%
  filter(Kraj=="PL") %>%
  group_by(Artist) %>%
  summarise(Punkty=sum(Punkty)) %>%
  ungroup() %>%
  arrange(desc(Punkty), Artist) %>%
  top_n(30, Punkty) %>%
  mutate(Artist=factor(Artist, levels = Artist)) %>%
  ggplot() +
  geom_bar(aes(Artist, Punkty), stat="identity", fill = "lightgreen", color = "black") +
  theme(axis.text.x = element_text(angle=90, hjust=1))



# Forma artystów na przestrzeni lat
notowania_analiza %>%
  #   filter(Artist %in% top_artists) %>%
  # liczba punktów artysty per rok
  mutate(Rok = year(NotowanieData)) %>%
  group_by(Rok, Artist) %>%
  summarise(Punkty=sum(Punkty)) %>%
  ungroup() %>%
  # liczba lat, w których artysta był notowany
  group_by(Artist) %>%
  mutate(n_years = n()) %>%
  ungroup() %>%
  # tylko ci, którzy są na liście co najmniej w 10 latach
  filter(n_years >= 10) %>%
  # kolejność artystów wg punktów dla każdego roku
  group_by(Rok) %>%
  arrange(desc(Punkty)) %>%
  mutate(npos = row_number()) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(Rok, npos, color=Artist), size = 2, show.legend = FALSE) +
  geom_smooth(aes(Rok, npos, color=Artist),  method = "loess",
              show.legend = FALSE, se = FALSE, size=1, alpha = 0.4) +
  facet_wrap(~Artist) +
  scale_y_reverse()


# najpopularniejsze piosenki w roku - wszystko
notowania_analiza %>%
  mutate(Rok=year(NotowanieData)) %>%
  group_by(Rok, Artist, Title) %>%
  summarise(Punkty=sum(Punkty)) %>%
  ungroup() %>%
  group_by(Rok) %>%
  arrange(desc(Punkty)) %>%
  mutate(PozRok = row_number()) %>%
  ungroup() %>%
  filter(PozRok == 1) %>%
  ggplot() +
  geom_bar(aes(Rok, Punkty), stat="identity", fill = "lightgreen", color = "black") +
  geom_text(aes(Rok, Punkty, label=paste(Artist, Title, sep=" - ")), angle=90,
            hjust = -0.1, vjust = 0.1) +
  expand_limits(y = c(0, 4000))


# najpopularniejsze piosenki w roku - Polska
notowania_analiza %>%
  filter(Kraj=="PL") %>%
  mutate(Rok=year(NotowanieData)) %>%
  group_by(Rok, Artist, Title) %>%
  summarise(Punkty=sum(Punkty)) %>%
  ungroup() %>%
  group_by(Rok) %>%
  arrange(desc(Punkty)) %>%
  mutate(PozRok = row_number()) %>%
  ungroup() %>%
  filter(PozRok==1) %>%
  ggplot() +
  geom_bar(aes(Rok, Punkty), stat="identity", color = "black", fill = "lightgreen") +
  geom_text(aes(Rok, Punkty, label=paste(Artist, Title, sep=" - ")),
            angle=90, hjust=-0.1, vjust=0.1) +
  expand_limits(y = c(0, 3000))


# wynik artysty dla kolejnych notowań
notowania_analiza %>%
  filter(Artist %in% top_artists) %>%
  group_by(NotowanieData) %>%
  mutate(Punkty = sum(Punkty)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(NotowanieData, Punkty),
             color = "gray", alpha = 0.4, show.legend = FALSE) +
  geom_smooth(aes(NotowanieData, Punkty),  method = "loess",
              show.legend = FALSE, se = FALSE, size = 1.4, color = "orange") +
  facet_wrap(~Artist)


# najpopularniejsze piosenki w danym roku wskazanego artysty
artysta <- "BUDKA SUFLERA"    # ;-)

notowania_analiza %>%
  filter(Artist==artysta) %>%
  mutate(Rok=year(NotowanieData)) %>%
  group_by(Rok, Title) %>%
  summarise(Punkty=sum(Punkty)) %>%
  ungroup() %>%
  group_by(Rok) %>%
  arrange(desc(Punkty)) %>%
  mutate(RokPoz=row_number()) %>%
  ungroup() %>%
  arrange(Rok, desc(Punkty), Title) %>%
  filter(RokPoz==1) %>%
  ggplot() +
  geom_bar(aes(Rok, Punkty), stat="identity",
           color = "black", fill = "lightgreen") +
  geom_text(aes(Rok, Punkty, label=Title),
            angle=90, hjust=-0.1, vjust=0.1) +
  expand_limits(y = c(0, 900))



# artysta i jego piosenki na przestrzeni lat
notowania_analiza %>%
  filter(Artist %in% top_artists) %>%
  ggplot() +
  geom_line(aes(NotowanieData, PozAkt, color=Title, group=Title)) +
  geom_point(aes(NotowanieData, PozAkt, color=Title), size = 2) +
  ylim(30,0) +
  facet_wrap(~Artist) +
  theme(legend.position = "none")


#### PIERWSZE MIEJSCE I TOP-3 ####

# najczęściej na 1 miejscu
notowania_analiza %>%
  filter(PozAkt == 1) %>%
  count(Artist, Title) %>%
  ungroup() %>%
  top_n(20, n) %>%
  arrange(desc(n))


# najczęściej w top3
notowania_analiza %>%
  filter(PozAkt %in% c(1,2,3)) %>%
  count(Artist, Title, PozAkt) %>%
  ungroup() %>%
  group_by(Artist, Title) %>%
  mutate(nn = sum(n)) %>%
  ungroup() %>%
  spread(key=PozAkt, value=n) %>%
  top_n(20, nn) %>%
  arrange(desc(nn), desc(`1`), desc(`2`), desc(`3`)) %>%
  rename(`W top3`=nn)


# jednorazowe hity
# utwor był na 1 miejscu, ale tylko raz w top5
notowania_analiza %>%
  filter(PozAkt <= 5) %>%
  count(Artist, Title, PozAkt) %>%
  ungroup() %>%
  group_by(Artist, Title) %>%
  mutate(wtop = sum(n)) %>%
  ungroup() %>%
  filter(wtop == 1, PozAkt==1) %>%
  select(-PozAkt, -n, -wtop)


# najdłużej na liście:
notowania_analiza %>%
  group_by(Artist, Title) %>%
  mutate(min_Data = min(NotowanieData), max_Data = max(NotowanieData), n_notowan = n()) %>%
  ungroup() %>%
  select(Artist, Title, min_Data, max_Data, n_notowan) %>%
  distinct() %>%
  mutate(dni = as.numeric(max_Data-min_Data)) %>%
  #filter(dni <= 1000) %>%
  top_n(20, dni) %>%
  arrange(dni) %>%
  mutate(Song = paste(Artist, Title, sep = " - ")) %>%
  mutate(Song = factor(Song, levels=Song)) %>%
  ggplot() +
  geom_segment(aes(x=min_Data, y=Song,
                   xend=max_Data, yend=Song, color = Song),
               size = 3, show.legend = FALSE) +
  geom_text(aes(x=max_Data, y=Song, label=sprintf("%d dni (%d)", dni, n_notowan)),
            hjust = -0.1, vjust = 0.2, show.legend = FALSE) +
  expand_limits(x = c(make_date(1982, 1, 1), make_date(2020, 1, 1)))



# średnia ocena vs ilość notowań na liście - per artysta
# liczba piosenek artysty
n_songs <- notowania_analiza %>%
  select(Artist, Title) %>%
  distinct() %>%
  count(Artist, sort = TRUE) %>%
  ungroup()
# średnia liczba punktów artysty
m_punkty <- notowania_analiza %>%
  group_by(Artist) %>%
  summarise(m_Punkty = mean(Punkty)) %>%
  ungroup()

left_join(n_songs, m_punkty, by = "Artist") %>%
  top_n(20, n) %>%
  ggplot() +
  geom_hline(aes(yintercept = mean(m_Punkty)), color = "red") +
  geom_vline(aes(xintercept = mean(n)), color = "red") +
  geom_point(aes(n, m_Punkty, color=Artist), show.legend = FALSE) +
  geom_text_repel(aes(n, m_Punkty, label = Artist, color=Artist), show.legend = FALSE)



# średnia ocena vs ilość notowań na liście - per piosenka
# ile raz piosenka była na liście
n_times_song <- notowania_analiza %>%
  count(Artist, Title) %>%
  ungroup()
# średnia liczba punktów artysty
m_punkty_song <- notowania_analiza %>%
  group_by(Artist, Title) %>%
  summarise(m_Punkty = mean(Punkty)) %>%
  ungroup()

left_join(n_times_song, m_punkty_song,
          by = c("Artist"="Artist", "Title"="Title")) %>%
  top_n(30, n) %>%
  ggplot() +
  geom_hline(aes(yintercept = mean(m_Punkty)), color = "red") +
  geom_vline(aes(xintercept = mean(n)), color = "red") +
  geom_point(aes(n, m_Punkty, color=Artist), show.legend = FALSE) +
  geom_label_repel(aes(n, m_Punkty,
                       label = sprintf("%s\n%s", Artist, Title),
                       color=Artist),
                   show.legend = FALSE)




# Licza notowań a liczba na pierwszym miejscu
n_first <- notowania_analiza %>%
  filter(PozAkt == 1) %>%
  count(Artist, Title) %>%
  ungroup()

left_join(n_times_song,
          n_first %>% rename(n_first=n),
          by = c("Artist"="Artist", "Title"="Title")) %>%
  filter(n_first >= 1) %>%
  ggplot() +
  geom_point(aes(n, n_first )) +
  geom_smooth(aes(n, n_first), show.legend = FALSE)


# dla tych co były najczęściej na pierwszym miejscu:
left_join(n_times_song,
          n_first %>% rename(n_first=n),
          by = c("Artist"="Artist", "Title"="Title")) %>%
  top_n(10, n_first) %>%
  ggplot() +
  geom_point(aes(n, n_first, color = Title), size=4, show.legend = FALSE) +
  geom_label_repel(aes(n, n_first,
                       label = sprintf("%s\n%s", Artist, Title),
                       color = Title),
                   max.iter = 50000, show.legend = FALSE) +
  expand_limits(y = c(0, 20))


# Jaki procent notowań dana piosenka była na pierwszym miejscu?
left_join(n_times_song,
          n_first %>% rename(n_first=n),
          by = c("Artist"="Artist", "Title"="Title")) %>%
  top_n(20, n_first) %>%
  mutate(p = 100*n_first/n) %>%
  arrange(p) %>%
  mutate(Song = sprintf("%s - %s", Artist, Title)) %>%
  mutate(Song = factor(Song, levels = Song)) %>%
  ggplot() +
  geom_bar(aes(Song, p), stat="identity", fill = "lightgreen", color = "black") +
  geom_hline(yintercept = c(0, 25, 50), color = "red") +
  coord_flip()


# Jak długo piosenki są na liście?
ggplot(n_times_song) +
  geom_histogram(aes(n), binwidth = 1, fill="lightgreen", color = "black")


# Ile czasu zajmuje dotarcie do pierwszego miejsca?
to_the_top <- notowania_analiza %>%
  select(Artist, Title, PozAkt, NotowanieData) %>%
  group_by(Artist, Title) %>%
  arrange(NotowanieData) %>%
  mutate(NotowanieN = row_number()) %>%
  ungroup() %>%
  select(-NotowanieData) %>%
  filter(PozAkt == 1) %>%
  group_by(Artist, Title) %>%
  filter(NotowanieN == min(NotowanieN)) %>%
  ungroup()

to_the_top %>%
  ggplot() +
  geom_histogram(aes(NotowanieN), binwidth = 1, fill="lightgreen", color = "black")


# Na pierwszym miejscu debiutowały:
to_the_top %>%
  filter(NotowanieN == min(NotowanieN)) %>%
  select(Artist, Title)


#### TAGI Z LASTFM ####
# https://cran.r-project.org/src/contrib/Archive/RLastFM/RLastFM_0.1-5.tar.gz
library(RLastFM)

# unikalna lista piosenek - tego szukamy w LastFM
piosenki <- notowania_analiza %>%
  select(Artist, Title) %>%
  distinct()

# miejsce na tag
piosenki$Tag <- NA

# dla wszystkich utowrów - przypisz znaleziony tag (tylko pierwszy z listy wszystkich)
len <- nrow(piosenki)
for(i in 1:len) {
  cat(paste0("\r", i, "/", len))

  # nie wszystkie piosenki są w bazie LastFM - stąd z obsługą błędów (niedoskonałą)
  # przerobić na purrr
  tag <- withCallingHandlers(
    tryCatch(
      track.getTopTags(track = piosenki[i, "Title"], artist = piosenki[i, "Artist"])$tag,
      error=function(e) e)
  )
  if(length(tag) != 0) {
    piosenki[i, "Tag"] <- tag[1]
  } else {
    piosenki[i, "Tag"] <- "notag"
  }
}

piosenki$Tag <- tolower(piosenki$Tag)

notowania_analiza <- left_join(notowania_analiza, piosenki,
                               by = c("Artist"="Artist", "Title"="Title"))



# najpopularniejszy tag - wszystko
notowania_analiza %>%
  group_by(Tag) %>%
  summarise(Punkty=sum(Punkty)) %>%
  ungroup() %>%
  arrange(desc(Punkty), Tag) %>%
  top_n(30, Punkty) %>%
  mutate(Tag=factor(Tag, levels = Tag)) %>%
  ggplot() +
  geom_bar(aes(Tag, Punkty), stat="identity", fill = "lightgreen", color = "black") +
  theme(axis.text.x = element_text(angle=90, hjust=1))


# najpopularniejszy tag wg roku
notowania_analiza %>%
  mutate(Rok=year(NotowanieData)) %>%
  group_by(Rok, Tag) %>%
  summarise(Punkty=sum(Punkty)) %>%
  ungroup() %>%
  group_by(Rok) %>%
  arrange(desc(Punkty)) %>%
  mutate(PozRok = row_number()) %>%
  ungroup() %>%
  arrange(Rok, PozRok, desc(Punkty), Tag) %>%
  filter(PozRok==1) %>%
  ggplot() +
  geom_bar(aes(Rok, Punkty, fill=Tag), stat="identity", show.legend = FALSE) +
  geom_text(aes(Rok, Punkty, label=Tag), angle=90, hjust=-0.1) +
  expand_limits(y = c(0, 11000))
