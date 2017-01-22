rm(list=ls())


library(rvest)
library(dplyr)

lp3_base_url <- "http://www.lp3.pl/alpt.phtml?m=1&nn="
max_nn <- 1795

notowania <- data.frame()

for(curr_nn in 1:max_nn) {
	lp3_url <- paste0(lp3_base_url, curr_nn)
	
	page <- read_html(lp3_url)
	
	notowanie_info <- page %>%
		html_node("div#view-not") %>%
		html_nodes("p") %>%
		html_text()
	
	notowanie_info <- gsub(",", " ", notowanie_info)
	notowanie_info <- gsub("[ ]+", " ", notowanie_info)
	notowanie_info <- unlist(strsplit(notowanie_info, "[ .]"))
	
	
	notowanie_tabela <- page %>%
		html_node("div#view-not") %>%
		html_node("div#zestawienie") %>%
		html_node("table") %>%
		html_table()
	
	colnames(notowanie_tabela) <- c("PozAkt", "PozPop", "TygNotowan", "Song", "Kraj", "Zmiana")
	notowanie_tabela <- filter(notowanie_tabela, PozAkt!="poczekalnia")
	notowanie_tabela$PozAkt <- as.integer(notowanie_tabela$PozAkt)
	notowanie_tabela$PozPop <- as.integer(notowanie_tabela$PozPop)
	notowanie_tabela$TygNotowan <- as.integer(notowanie_tabela$TygNotowan)
	
	notowanie_tabela$Song <- gsub(" &mdmdash; ", " — ",  notowanie_tabela$Song)
	if(nrow(notowanie_tabela) > 0) {
		notowanie_tabela$NotowanieNumer <- notowanie_info[2]
		notowanie_tabela$NotowanieRok <- as.integer(notowanie_info[6])
		notowanie_tabela$NotowanieMiesiac <- as.integer(notowanie_info[5])
		notowanie_tabela$NotowanieDzien <- as.integer(notowanie_info[4])
		notowanie_tabela$NotowanieProwadzacy <- paste(notowanie_info[10], notowanie_info[11])
		
		notowania <- rbind(notowania, notowanie_tabela)
	}
}


songs <- do.call(rbind, strsplit(notowania$Song, " — "))
songs <- as.data.frame(songs)
colnames(songs) <- c("Artist", "Title")
songs$Artist <- as.character(songs$Artist)
songs$Title <- as.character(songs$Title)

notowania <- cbind(notowania, songs)

notowania$Song <- NULL
notowania$Kraj <- as.factor(notowania$Kraj)
notowania$NotowanieProwadzacy <- as.factor(notowania$NotowanieProwadzacy)
