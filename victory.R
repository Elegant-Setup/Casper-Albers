# Inzending Casper Albers
# Elegante Algoritmes - Programmeren als ambacht


# Stap 1, leveren van tekst
# Onderstaande tekst is de openingsparagraaf van het artikel over Mondriaan op Wikipedia
# Bron: http://nl.wikipedia.org/w/index.php?title=Piet_Mondriaan&oldid=34932505
tekst <- "Pieter Cornelis (Piet) Mondriaan (Amersfoort, 7 maart 1872 - New York, 
1 februari 1944) was een Nederlandse kunstschilder en kunsttheoreticus, die echter 
een groot deel van zijn leven in het buitenland woonde en werkte. Mondriaan wordt 
algemeen gezien als een pionier van de abstracte en non-figuratieve kunst. Vooral 
zijn latere geometrisch-abstracte werk, met de kenmerkende horizontale en verticale 
zwarte lijnen en primaire kleuren, is wereldberoemd en dient als inspiratiebron voor 
vele architecten en ontwerpers van toegepaste kunst. Hij was een van de belangrijkste 
medewerkers van het tijdschrift De Stijl en ontwikkelde een eigen kunsttheorie, die 
hij Nieuwe Beelding of Neo-plasticisme noemde. Hij is in het buitenland beter bekend 
als Mondrian, een verfransing (verengelsing) van zijn achternaam.
Mondriaan werd op 7 maart 1872 aan de Kortegracht in Amersfoort geboren als zoon 
van de streng christelijke hoofdonderwijzer, Pieter Cornelis Mondriaan en zijn vrouw 
Johanna Christina de Kok. Hij werd gedoopt in de Nederlandse Hervormde Kerk en werd 
christelijk opgevoed. Omdat zijn moeder vaak ziek was, leidde zijn zus Christien op 
nog geen achtjarige leeftijd het huishouden. Zijn vader was naast hoofdonderwijzer 
ook tekenleraar en leerde hem al vroeg tekenen. In april 1880 verhuisde het gezin 
Mondriaan naar Winterswijk, waar Mondriaans vader hoofd werd van de School voor 
Christelijk Nationaal Onderwijs." 

# Stap 2, de tekst bewerken en omzetten in kleurcodes 1 t/m 6
# Gooi alles dat geen 'gewone' letter is weg
tekst <- gsub("[^A-Za-z///']", "", tekst)
oldtekst <- tekst

# Geef aan waar de hoofdletters zitten, deze krijgen een Groter Vlak
hoofdletter <- tekst
hoofdletter <- gsub("[a-z]","0",hoofdletter)
hoofdletter <- gsub("[A-Z]","1",hoofdletter)

# vervang letters door kleurcodes 1 t/m 6, 
# zodat de verdeling redelijk gelijk is
tekst <- gsub("[A-Da-d]", "1", tekst)
tekst <- gsub("[Ee]", "2", tekst)
tekst <- gsub("[F-Lf-l]", "3", tekst)
tekst <- gsub("[M-Nm-n]", "4", tekst)
tekst <- gsub("[O-So-s]", "5", tekst)
tekst <- gsub("[T-Zt-z]", "6", tekst)

 

# Stap 3: definieer wat variabelen 
# kleuren: de kleurcodes
# M het aantal 'blokjes' dat (max.) per rij/kolom nodig is
# canvas: de matrix met kleurcodes per blokje
# bezet: nodig om vakjes over te slaan wanneer deze bezet zijn
#        door een groot vierkant
kleuren <- c("red", "black", "yellow","whitesmoke", "blue","grey") 
M       <- 53
canvas  <- array(4,dim=c(M+1+5,M+1+5))
bezet   <- array(FALSE,dim=c(M+1+5,M+1+5))

# Stap 4: jinterval bepaald bij welke verticale coordinaten blokjes nodig zijn
jinterval <- function(i,M){
  if(i <= ceiling(M/2)){ 
    jinterval <- (ceiling(M/2)-i+1):(ceiling(M/2)+i-1) 
  }else{ 
    jinterval <- (ceiling(M/2) - (M - i)):(ceiling(M/2)+ (M - i))
  }
  return(jinterval)
}

# Stap 5: vul het canvas volgens de volgende regels:
#         elke letter wordt omgezet in een bepaalde kleur
#         kleine letters worden een vierkant van 1x1
#         hoofdletters worden rxr, met r = 2 + teller modulo 3
#         waarbij 'teller' telt om de hoeveelste letter het gaat
teller <- 1
for(i in 1:M){
  for(j in jinterval(i,M)){
    r <-  2 + teller %% 3
    if(sum(bezet[i:(i+r),j:(j+r)])==0){
      if(as.numeric(substr(hoofdletter,teller,teller))){
        canvas[i:(i+r),j:(j+r)] <- as.numeric(substr(tekst,teller,teller))
        bezet[i:(i+r),j:(j+r)] <- TRUE
      }else{
        canvas[i,j] <- as.numeric(substr(tekst,teller,teller))
      }
      teller <- teller + 1
    }
  }
}

# Stap 6: teken de Victory Boogie Wiki en schrijf de tekening naar pdf
pdf(file="Mondriaan.pdf")
plot(c(-.5,M+.5),c(-.5,M+.5), bty="n",xaxt="n",xlab="",xaxs="i",yaxt="n",ylab="",yaxs="i",type="n")
for(i in 0:M){
  for(j in 0:M){
      rect(i-.5,j-.5,i+.5,j+.5,col=kleuren[canvas[i+1,j+1]],border= NA)
  }
}
# Teken driehoeken om de randen 'schuin af te snijden'
polygon(c(-M/2,M,-M/2),   c(-M/2,-M/2,M),   col="white", border=NA)
polygon(c(3*M/2,0,3*M/2), c(-M/2,-M/2,M),   col="white", border=NA)
polygon(c(-M/2,M,-M/2),   c(3*M/2,3*M/2,0), col="white", border=NA)
polygon(c(3*M/2,0,3*M/2), c(3*M/2,3*M/2,0), col="white", border=NA)
polygon(c(0,M/2,M,M/2),   c(M/2,0,M/2,M),   lwd=.1)
dev.off()

