# Wczytaj dane tekstowe
# Wczytaj plik tekstowy z lokalnego dysku
text2 <- readLines(file.choose())
text2

install.packages("qdap")
library(qdap)


frequent_terms <- freq_terms(text2)
frequent_terms
frequent_terms <- freq_terms(text2, stopwords = Top200Words)
plot(frequent_terms)


# Tworzenie chmury słów za pomocą pakietu wordcloud
install.packages("wordcloud")
library(wordcloud)


# Utwórz chmurę słów
wordcloud(frequent_terms$WORD, frequent_terms$FREQ)


# Opcje chmury słów
?wordcloud
# Zmiana wartości min.freq i max.words w celu wyświetlenia mniejszej/większej liczby słów.
# min.freq: słowa o częstości poniżej tej wartości nie będą wyświetlane
# max.words: maksymalna liczba słów do wyświetlenia
# Ograniczenie liczby słów w chmurze poprzez określenie minimalnej częstości
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4)
# Ograniczenie liczby słów w chmurze poprzez określenie maksymalnej liczby słów
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5)


# Dodanie różnych palet kolorystycznych
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Blues"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Greens"))


# Optymalizacja i dostosowanie wyników
# Dodanie koloru do chmury słów dla lepszej wizualizacji
# Dodanie koloru
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2"))
# Dodanie koloru
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5, colors = brewer.pal(8,"Accent"))
?brewer.pal
brewer.pal.info


#na podstawie powyzszych wykresów i chmur widać, ze w pierwszym wystąpieniu jednym z glownych tematow
#był rynek pracy (wielkorotnie powtorzone slowo jobs) oraz rodziny (slowa child, families, care), 
#podczas gdy w drugim przemowieniu dominowaly podreslenia slow tj. amerykanie, przeszlosc, terazniejszosc
#i przyszlosc praz podatki
#tematyka jest wiec podobna natomiast priorytety sie troche zmienily
