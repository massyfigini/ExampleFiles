##### MARKET BASKET ANALYSIS

# OBIETTIVO
# Identificare prodotti acquistati frequentemente insieme per creare un
# algoritmo tipo "forse ti potrebbe interessare anche" a partire da 
# cosa è presente nel carrello

# groceries.csv contiene tutte le 9835 transazioni di un negozio in un mese

library(arules)



### 1. ANALISI ESPLORATIVA

# read.transaction salva le transazioni in una "sparse matrix" che contiene su 
# ogni riga la spesa della persona e un numero di colonne pari al numero di diversi 
# prodotti presenti nel file: 0 se non presente nella spesa, 1 se presente
# La matrice consente in questo caso di occupare meno memoria del dataframe 
# (solo gli 1 occupano memoria)
groceries <- read.transactions("C:/Users/figinim/Documents/Studies/Machine Learning with R/blog/groceries.csv", sep = ",")

# con summary vedo che gli acquisti più frequenti hanno un solo prodotto,
# che la media è di 4 prodotti per acquisto, che il più acquistato è il latte intero
summary(groceries)

# composizione prime 5 righe
inspect(groceries[1:5])

# frequenza di 3 prodotti (primi 3 in ordine alfabetico)
itemFrequency(groceries[, 1:3])

# grafico con i prodotti presenti in almeno il 10% degli acquisti
itemFrequencyPlot(groceries, support = 0.1)

# grafico con la top 20 dei prodotti più acquistati
itemFrequencyPlot(groceries, topN = 20)

# guardo la "sparse matrix" (100 record casuali). Se ordinati per data di acquisto
# può consentire di vedere qualche affinità tra i prodotti in base alla stagionalità
image(sample(groceries, 100))



### 2. MODELLO

# algoritmo "apriori" per cercare itemset frequenti per approssimazioni successive
# (approccio bottom up: sottoinsiemi frequenti costruiti aggiungendo un item per volta)
# lasciando i valori di default utilizza solo i prodotti presenti nel 10% dei record
# (support = 0.1) e imposta un intervallo di confidenza per le regole trovate
# del 80% (confidence = 0.8)
# Nel nostro caso non trova nulla naturalmente (10% abbiamo pochissimi prodotti)
apriori(groceries)

# imposto i parametri:
# SUPPORT: considero il prodotto se è acquistato mediamente almeno una volta al giorno,
# quindi almeno 30 volte. 30/9835 = 0.003
# CONFIDENCE: bisogna valutare caso per caso, settarlo troppo alto significa prendere
# solo le associazioni ovvie, troppo basso prendo anche quelle casuali.
# Di base si può partire con un valore alto e abbassarlo man mano quando testo l'algoritmo
# se noto che non dà risultati significativi.
# Metto il 25%: significa che per essere inclusa nel risultato, una regola deve essere
# vera in almeno un caso su quattro.
# MINLEN: lo imposto uguale a 2 per prendere solo le regole con almeno due prodotti 
# (elimino regole con 1 prodotto dovute ad acquisti singoli frequenti)
groceryrules <- apriori(groceries, parameter = list(support =
                          0.003, confidence = 0.25, minlen = 2))
groceryrules

# summary of grocery association rules
summary(groceryrules)
# il valore di lift indica la probabilità che un articolo sia acquistato insieme agli altri del gruppo
# rispetto alla sua probabilità media di acquisto. Quindi un valore di 2 significa che ha il doppio delle 
# probabilità di essere acquistato.

# prime 5 regole
inspect(groceryrules[1:5])

# prime 5 dal valore più alto di lift
inspect(sort(groceryrules, by = "lift")[1:5])

# se vogliamo solo le regole che includono birra in botiglia
Bbeer <- subset(groceryrules, items %in% "bottled beer")
inspect(Bbeer)
# nella formula subset per prendere solo i liquoti che compaiono a sinistra sostituisco items con lhs,
# viceversa con rhs.
# per vedere anche la birra in lattina uso pin al posto di in:
beer <- subset(groceryrules, items %pin% "beer")
inspect(beer)
# usando c("bottled beer", "liquor") vedo le regole con bottiglie di birra OR liquori. Con:
BBB <- subset(groceryrules, items %ain% c("bottled beer", "liquor"))
inspect(BBB)
# vedo le regole con bottiglie di birra AND liquori.
# Posso subsettare anche in base a support, confidence o lift. 
#· Se voglio le regole vere in almeno 3 casi su 4:
TOP <- subset(groceryrules, confidence > 0.75)
inspect(TOP)

# esporto le regole in un csv
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# converto le regole in un data frame
groceryrules_df <- as(groceryrules, "data.frame")
