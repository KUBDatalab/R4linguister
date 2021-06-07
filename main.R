# R for linguister 

Det allerførste:
  
install.packages("tidyverse")

# R er et generelt programmeringssprog. Det kan alt. Men det er særligt velegnet
# til statistik. I bred forstand

# R er et fortolket programmeringssprog. Det betyder at vi skriver kode i denne
# fil, som godt nok hedder R til efternavn. Men ellers er en helt almindelig 
# tekstfil.
# 
# Hvis vi vil skrive noget i filen der ikke er kode - alt det her eksempelvis,
# så sætter vi en # forrest - og så er det en kommentar, og ikke kode.

# Men ellers er princippet at vi skriver en linie kode - instruktioner til 
# programmeringssproget om hvad vi godt vil have det til at gøre.
# Det gør vi i RStudio - der gør tingene lidt lettere for os, men ikke i sig 
# selv er et programmeringssprog.
# Vores kodelinie, som vi har skrevet i RStudio, sender vi nu på snedig vis til
# programmeringssproget R, som, hvis ellers vi har skrevet koden rigtigt, 
# "eksekverer" koden, og giver os et resultat.
# I RStudio kan vi placere cursoren i den linie kode vi har skrevet, og trykke
# ctrl+enter (eller eller command+enter på en mac.)
# Lad os prøve:
1 + 1

# 1 + 1 bliver sendt fra vores kodeeditor ned på console - konsollen, og 
# eksekveret. R giver os resultatet - og det er selvfølgelig 2.

# Det er sjældent interessant at arbejde med tal på den måde. Vi vil godt kunne 
# gemme resultatet af en beregning, og arbejde videre med det.

# Det kan vi gøre ved at gemme resultatet af vores regnestykke i en variabel:
  
a <- 1 + 1

# og så ctrl+enter - det er sidste gang jeg skriver det - antag at det optræder
# hver gang.

# <- er en assignment operator. I kan nærmes visualisere at resultatet af 1 + 1
# bliver puttet ind i "a" ved hjælp af den lille pil.

# Bemærk hvad der skete ovre til højre i "Environment". Det giver et godt 
# overblik

# Nu kan vi så gøre ting med a. Simpel matematik eksempelvis

a * 2

# a er en såkaldt atomar variabel. Vi har kun en enkelt værdi i den. Her er det
# et tal. Vi taler engelsk, så "numeric" er den mere generelle betegnelse.
# vi har også tekststrenge. Det vil vi kalde "Character" for vi kan have et 
# eller flere tegn. Uden at det nødvendigvis er tekst som vi ville læse det på
# tekst-tv. Det angiver vi ved at sætte "" omkring teksten.

b <- "a"

# Vi er typisk interesserede i mere end en værdi. Så er variablen ikke længere
# atomar - den er en vektor. Det er hvad vi kalder den her.
# Når vi har flere værdier vi godt vil have samlet til en vektor, bruger vi 
# en særlig funktion til at gøre det:

c <- c(1,2,3,4)

# En funktion er et objekt, der tager noget input, gør noget ved det, og 
# returnerer et output.
# Her er det funktionen c. Den skal have noget input. Det skal vi have angivet
# på en eller anden måde. Konventionen er at det sker inde i en parantes.
# Funktionen c får her som input tallene fra 1 til 4. Og resultatet er en 
# numerisk vektor (fordi det var tal der blev hældt ind i parantesen). Den 
# bliver så assignet til variabel-navnet c

# Vi kan også lave matematik på de her vektorer:
  
c + 1


# R er glad for vektorer, de fleste funktioner er bygget til at arbejde med 
# vektorer

# En funktion der kunne arbejde på en vektor kunne være en beregning af en 
# middelværdi:
  
mean(c)

# Der er en kortere måde at få vektoren med tallene fra 1 til 4:
1:4

# Vi arbejder med statistik. På virkeligheden. Og undertiden mangler vi 
# simpelthen data. Det angiver R på en bestemt måde:
  
NA

# Lad os tilføje en NA-værdi til vores vektor c:

c <- c(c, NA)

# Hvad sker der så når vi beregner middelværdien?
  
mean(c)

# Det er fordi der er en observation der kunne være hvad som helst.
# Hvis vi godt vil have mean til at returnere middelværdien af de tal der ikke
# er NA - så kan vi fortælle mean hvordan den skal håndtere NA-værdier.
# 
# Hvordan? Læs manualen! Hvordan gør jeg det? Skriv:

?mean

# Og ovre i help, får vi så manualen. Og vi kan se at vi kan skrive:
  
mean(c, na.rm=T)

# na.rm = T er et argument til funktionen. En parameter, der fortæller funktionen
# hvordan den skal opføre sig. I dette tilfælde - at hvis der er NA-værdier i
# input - så skal de smides ud, før beregningen foretages.
# Det er nyttigt! 

# Vektorer kunne også være af typen character:

d <- LETTERS

# Her har vi snydt, og brugt en vektor som R har lavet for os. Alle de store 
# bogstaver.

# Hvad er det 10. bogstav?

d[10]


# Det kalder vi at subsette. Det er et nyttigt ord at kende.

# Lad os lave en vektor med 1000 tilfældige store bogstaver. Funktionen hedder
# sample. Start med at læse dokumentationen.
# Når vi udvælger tilfældige ting - så er det jo tilfældigt.
# Men hvis vi skriver:

set.seed(42)

# Alle sammen - så bliver det tilfældigt på den samme måde hver gang


tilf_bogstaver <- sample(LETTERS, 1000, replace = T)

# Hvor mange af de bogstaver er J'er? 

# når vi sammenligner ting får vi et ja/nej, sandt/falsk svar.
# er 1 større end 5? Nej, det er det ikke, svaret er at det vi har 
# skrevet er falsk. Eller FALSE
1 > 5


# Nu arbejder vi med bogstaver. Så der er det der med større eller mindre
# mere tvivlsomt. Men vi kan sammenligne, og se om to bogstaver er ens.
# Så skriver vi == 

"A" == "A"

# Det kan vi også gøre med vektorer:
tilf_bogstaver == "J"

# Så får vi 1000 "sand/falsk" svar

# Hvor mange J'er er der så? Vi kan udnytte, at R bagved giver TRUE værdien 1, og
# FALSE værdien 0

# Så hvis vi pakker resultatet af vores sammenligning ind i en funktion der lægger
# tal sammen - sum - kan vi få antallet af J'er i vores vektor:

sum(tilf_bogstaver == "J")

# Vi kan også bruge sådan en sand/falsk vektor til at pille observationer ud
# af vores vektor. Subsetting var hvad vi kaldte det:

tilf_bogstaver[tilf_bogstaver == "J"]

# Hvilket ikke er super interesant. Men 

c[c > 2]

# er mere anvendelig.

# det er meget fint. Men normalt har vi jo mange data. I tabeller. Og der er 
# både tekst og tal. Og vi kunne kun have en slags data i en vektor. Så hvad 
# gør man?
  
# Den struktur vi arbejder med hedder "dataframes"

# Lad os tage et eksempel:
  
mtcars

# Ikke specielt overskueligt:
    
View(mtcars)

# Meget bedre! Række - med observationer. Kolonner - det er vores variable  

# Hvordan subsetter man sådan en? Der er jo to dimensioner!
# det kan gøres på mange måder:

# Kantede paranteser. Komma mellem dimensionerne. Rækker er først, kolonner
# som nummer to. Angiv den eller de række/kolonne numre vi vil se. Angiver vi
# intet - får vi alt.

# Række nr. 1:
mtcars[1,]

# Kolonne nr 1:
mtcars[,1]

# Dollarnotation. Her angiver man kolonne med dens navn. Det er gør det 
# mere læsevenligt:

mtcars$cyl

# Flere? 
mtcars$c("cyl", "mpg")

# I stedet:
mtcars[,c("cyl","mpg")]

# Funktioner virker:
  
mean(mtcars$mpg)

# Andre måder at subsette på?
# Ja!

Men først! Funktioner

# Vi kan skrive vores egne. Hvad karakteriserer sådan en funktion?
# Den har et navn. Den tager noget input. Så gør den noget ved inputtet.
# Og spytter noget output ud. Når vi skal definere en funktion, bruger vi en
# særlig funktion, der hedder function

min_egen_funktion <- function(x){
  resultat <- x * 2
  resultat <- sqrt(resultat)
  return(resultat)
}

min_egen_funktion(47)

# Se - andre har skrevet funktioner før. En del af dem gør ting der er mere 
# nyttige end den funktion vi lige har skrevet.
# 
# De samles i pakker, der bliver kvalitetskontrolleret, og distribueret på
# en særlig hjemmeside på nettet, der hedder CRAN.
# 
# Når vi skal have fat i sådan en pakke funktioner, skal vi starte med at 
# installere pakken:

install.packages("dplyr")

# Og så skal vi indlæse den:

library(dplyr)

# Så får vi adgang til alle funktionerne i pakken dplyr. Og hvad kan den så?
# Jo, den kan manipulere dataframes. Eksempelvis kan den subsette dataframes.

# Lige før ville vi godt have de to kolonnner cyl og mpg pillet ud af vores
# datasæt:
  
mtcars[,c("cyl","mpg")]

# Det kan vi nu, med dplyr installeret gøre fiksere:
  
select(mtcars, cyl, mpg)

# Vi starter med at skrive den dataframe vi vil gøre ting ved som det første
# input i funktionen. Det er en vigtig pointe!
# 
# Lad os lige gemme resultatet:

cylindre_og_brændstoføkonomi <- select(mtcars, cyl, mpg)
    
# Nu vil vi så godt nøjes med at se på de bilver der har 8 cylindre:
#   
# Det er der også en funktion til. Den hedder filter:
  
filter(cylindre_og_brændstoføkonomi, cyl == 8)

# Det gemmer vi også:

kort_variabel_navn <- filter(cylindre_og_brændstoføkonomi, cyl == 8)

# Og nu kunne jeg godt tænke mig at vide hvad den gennemsnitlige 
# brændstoføkonomi er. Jeg vil godt have opsummeret noget data.
# 
# Den funktion hedder summarise. Den skal have vores dataframe som vi 
# lige lavede, og så skal den vide hvad vi vil gøre, her mean af mpg.
# Vi kan også give resultatet et navn:
  
summarise(kort_variabel_navn, middel_øko = mean(mpg))

# Det gemmer vi også:
otte_cylindere_økonomi <- summarise(kort_variabel_navn, middel_øko = mean(mpg))

# Og så kan vi gøre det for fire og seks cylindere også. Det er vældig bøvlet.
# Hvis nu vi kunne sende resultatet af en funktion, videre til den næste.
# Uden at skulle lave nye variable hver gang.
# 
# Det er der også nogen der har tænkt over. Pipen!
#   
# %>% 
#   
# Tager resultatet af hvad der er på venstre side, sender det videre til hvad
# der står på højre side. Hvis det er en funktion (og det er det næsten altid)
# sættes det ind på første plads i funktionen.
# 
# De her funktioner vi arbejder med ligger i "tidyverse". Og de udmærker sig 
# ved at de stort set alle sammen tager imod en dataframe som det første
# input. Gør noget ved dataframen, og spytter en ny dataframe ud i den anden
# ende. Det betyder at vi kan hægte funktionerne sammen med pipen. Smart!
#   
# I stedet for mellemregningerne:

mtcars %>% select(cyl, mpg) %>% 
  filter(cyl == 8) %>% 
  summarise(middel_øko = mean(mpg))

# Noget lettere at overskue. Synes jeg.

# Nu kunne vi godt tænke os at lave den samme øvelse for fire og seks cylindere.
# Det kunne gøres ved at ændre i linien med "filter".
# Vi kunne også sige - jeg vil gerne have splittet min dataframe op i forskellige
# grupper, en for hver værdi af cyl. Og beregnet middelværdien af mpg for 
# hver af de grupper.
# 
# Det er der også en funktion der gør: group_by

mtcars %>% 
  select(cyl, mpg) %>% 
  group_by(cyl)

# Bemærk output! tibble er "bare" en synonym for dataframe (næsten). Men 
# se linien der starter med "Groups:"
# 
# Nu sætter vi resten på:

mtcars %>% 
  select(cyl, mpg) %>% 
  group_by(cyl) %>% 
  summarise(middel_øko = mean(mpg))

# Lækkert!

# Nu er biler ikke så spændende. Vi vil gerne have vores egne data indlæst.
# Data kommer i mange formater. Her prøver vi med en excelfil.
# R i sig selv kan godt. Men...
library(readxl)
data <- read_excel("eksempeldata.xlsx")
# Lad os kigge på data:

head(data)

# Mere end et ark? Eller ark nr 3 i et regneark med 20 sheets?
?read_excel

# Nå. Var der andre dplyr-funktioner? Nu griner linguister sikkert af mig.
# Men, ville det være relevant at beregne forholdet mellem F1 og F2?
# Sikkert ikke. Men nu gør vi det. Jeg vil gerne have en kolonne i min dataframe
# der indeholder resultatet af F2 divideret med F1. Men jeg vil gerne
# beholde F1 og F2 kolonnnerne.
# Funktionen hedder mutate. Vi skal fortælle hvad den nye kolonne skal hedde
# og hvad indholdet skal være:

data %>% 
  mutate(forhold = F2/F1)

# Der er en variant. Hvis vi nu udelukkende er interesserede i den nye kolonnne,
# kunne vi skrive
data %>% 
  mutate(forhold = F2/F1) %>% 
  select(forhold)

# Så udvælger vi den kolonne vi lige beregnede.
# Alternativt:

data %>% 
  transmute(forhold = F2/F1) 

# Men det er mest fordi jeg er historisk interesseret kemiker at jeg synes 
# det er interessant

# Det var indlæsning, og manipulation af data. Der er uanede dybder i det her
# Men det var måske mere interessant at analysere på data end at indlæste og
# manipulere.

# Hvad kunne være nyttigt? Jeg spurgte og fik et svar:
#   
# Tabeller over gennemsnit og standardafvigelser mellem kategoriske variabel.
# Eksempelvis køn, socialgrupper, aldergrupper etc.


data %>% 
  group_by(Vokal) %>% 
  summarise(middel_F1 = mean(F1), 
            sd_F1 = sd(F1),
            middel_F2 = mean(F2),
            sd_F2 = sd(F2))

# Tovejs tabeller. Antal observationer.

table(data$Vokal, data$Socialklasse)

# 
# Vi vil også godt kunne plotte. Jeg har ladet mig fortælle at I har et særligt
# yndlingsplot. Så lad os se om vi kan lave det
# 
# R kan godt plotte selv. Og det er pæne og rene plots uden så mange dikkedarer
# Men det er ret begrænsede muligheder man har. 

# Det er baseret på andre data end dem vi har arbejdet med allerede. Så lad
# os indlæse dem:

data2 <- read.csv("formant_table_arr_r", sep="\t")

# Men vi skulle plotte!


# Der er et bibliotek til formålet. Det hedder ggplot2

library(ggplot2)

# Når vi opbygger plots i ggplot, så skal vi tænke additivt. Vi har et plot.
# Det viser noget allerede. Nu lægger vi noget nyt til det eksisterende plot.
# 
# Vi starter med et simpelt plot:
#   
ggplot()

# vi er nok nødt til at give noget data:
ggplot(data=data2)

# Vi er grundlæggende ude efter et plot hvor der er noget på x aksen og noget 
# andet på y-aksen. Og vi har noget data, der skal mappes til de to akser.
# Det gør vi ved hjælp af en hjælpefunktion, aes()
ggplot(data2 = data, mapping = aes(x=F2, y = F1))

# Det blev det ikke meget bedre af. Men vi har ikke fortalt hvad det egentlig
# er vi vil have i plottet. Vi vil godt have punkter. Et scatterplot.

ggplot(data = data2, mapping = aes(x=F2, y = F1)) +
  geom_point()


# Bemærk, + ikke pipe!

# Det var ikke punkter. Det var vokalerne vi ville have på.
# Så er det ikke geom_point, men geom_text. Og vi skal fortælle hvilken 
# kolonne i vores datasæt, der indeholder den tekst der skal plottes:

ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal))
str(data2)
# Farver!
ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal, color = Vokal))

# Skalaen er måske lidt overflødig. 
ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal, color = Vokal)) +
  theme(legend.position = "none")

ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal, color = Vokal)) +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top") +
  scale_y_reverse(position = "right")

# Den der grå baggrund:

ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal, color = Vokal)) +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top") +
  scale_y_reverse(position = "right") +
  theme_classic()

# Hov! theme_classic indeholder oplysninger om at signaturforklaringen skal være
# der, og at den skal stå ude til højre. Så nu tilføjer vi den igen!
  
ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal, color = Vokal)) +
  scale_x_reverse(position = "top") +
  scale_y_reverse(position = "right") +
  theme_classic() + 
  theme(legend.position = "none")


# Hvis vi vil styre hvor der er mærker på akserne:
ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal, color = Vokal)) +
  scale_x_reverse(position = "top", 
                  breaks = seq(0,3000, 250)) +
  scale_y_reverse(position = "right") +
  theme_classic() + 
  theme(legend.position = "none")  

# Og hvor lange skal de være?
  
ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal, color = Vokal)) +
  scale_x_reverse(position = "top", 
                  breaks = seq(0,3000, 250),
                  limits = c(2500, 250)) +
  scale_y_reverse(position = "right") +
  theme_classic() + 
  theme(legend.position = "none")  


# Hvis vi godt vil have sat Hz på tallene - og det vil vi nok, skal vi bruge 
# et ekstra bibliotek, der hedder "scales"

library(scales)

ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal, color = Vokal)) +
  scale_x_reverse(position = "top", 
                  breaks = seq(0,3000, 250),
                  limits = c(2500, 250),
                  labels = unit_format(unit = "Hz")) +
  scale_y_reverse(position = "right") +
  theme_classic() + 
  theme(legend.position = "none") 


# Vi skulle også have de der kurver på. Fænomenet er et densitetsplot.
# og funktionen i ggplot2 er derfor geom_density. Det er i to dimensitoner
# så det er geom_density_2d
ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal, color = Vokal)) +
  scale_x_reverse(position = "top", 
                  breaks = seq(0,3000, 250),
                  limits = c(2500, 250),
                  labels = unit_format(unit = "Hz")) +
  scale_y_reverse(position = "right") +
  theme_classic() + 
  theme(legend.position = "none")  +
  geom_density_2d(aes(color = Vokal))  

# Det ser noget anderledes ud end eksemplet jeg viste før. Hvorfor det?
# Jo, det er et relativt lille datasæt der oprindeligt blev plottet.
# Og siden da, er geom_density_2d blevet opgraderet. Der foretages nu en
# mere robust, og statistisk korrekt beregning af tætheden når der ikke
# er mange observationer. Så jeg har tilføjet nogen flere datapunkter.
# Men har ikke kunne tilføje præcist de punkter der skulle til for at 
# give samme resultat som tidligere.

ggplot(data2, aes(F2, F1)) +
  geom_text(aes(label = Vokal, color = Vokal)) +
  scale_x_reverse(position = "top", 
                  breaks = seq(0,3000, 250),
                  limits = c(2500, 250),
                  labels = unit_format(unit = "Hz")) +
  scale_y_reverse(position = "right") +
  theme_classic() + 
  theme(legend.position = "none")  +
  stat_ellipse(aes(color=Vokal), type = "norm", level=0.95)

# Hvad gør den?
# Beregner en ellipse, for hver vokal (det styrer vi i aes()), som, 
# givet 95% signifikans, omkranser de punkter vi har observeret.
# 
# Givet at de er normalfordelte.
# 
# type kan vælges anderledes. Se ?stat_ellipse for detaljer.
# og hvorfor stat_ellipse i stedet for geom_ellipse? Dæleme et godt 
# spørgsmål...

# De der vokaler optræder rigtig mange gange. Hvis vi kun vil have dem 
# en gang

data2 %>% 
  group_by(Vokal) %>% 
  mutate(meanF1 = mean(F1), meanF2 = mean(F2)) %>% 
  ungroup() %>% 
  ggplot(aes(F2, F1)) +
  geom_label((aes(meanF2, meanF1, label=Vokal))) +
  scale_x_reverse(position = "top", 
                  breaks = seq(0,3000, 250),
                  limits = c(2500, 250),
                  labels = unit_format(unit = "Hz")) +
  scale_y_reverse(position = "right") +
  theme_classic() + 
  theme(legend.position = "none")  +
  stat_ellipse(aes(color=Vokal), type = "norm", level=0.95)


# Hvad ellers?
# I data har vi oplysninger på socialklasse. Er der forskel på hvordan de
# udtaler "a"?

# Hvordan gør vi det? Vi starter med at beregne den gennemsnitlige frekvens
# hvormed vokalen udtales - opdelt på socialklasse:

data %>% 
  filter(Vokal == "a") %>% 
  group_by(Socialklasse) %>% 
  summarise(F1 = mean(F1), F2 = mean(F2))

# Det er fint. Hvordan plotter vi?
# ggplot er glad for at have data i såkaldt "tidy format". Et format hvor
# vi har en række for hver observation, og en kolonne for hver variabel.
# Her har vi lidt noget rod. Vi har her fire observationer. Men kun to 
# rækker. Det kan godt gøres. Men det bliver mere elegant hvis vi får 
# data på et andet format. Vi vil godt have et længere format. Lad os 
# lige gøre det, se på resultatet, og så kigge på hvordan vi nåede frem til
# det.

library(tidyr)
data %>% 
  filter(Vokal == "a") %>% 
  group_by(Socialklasse) %>% 
  summarise(F1 = mean(F1), F2 = mean(F2)) %>% 
  pivot_longer(cols = c("F1", "F2"),
               names_to = "formant",
               values_to = "frekvens")

# Vi skal bruge et ekstra bibliotek - tidyr. Data formatet kaldes for tidy,
# deraf navnet.
# Vi fortæller vores funktion pivot_longer, at vi skal arbejde med kolonnerne
# F1 og F2. Overskrifterne, navnene på variblene skal roteres ned i en
# kolonne med overskriften "formant", og værdierne samles i en kolonne
# med overskriften "frekvens".
# Nu er vi klar til at plotte:
  data %>% 
  filter(Vokal == "a") %>% 
  group_by(Socialklasse) %>% 
  summarise(F1 = mean(F1), F2 = mean(F2)) %>% 
  pivot_longer(cols = c("F1", "F2"),
               names_to = "formant",
               values_to = "frekvens") %>% 
    ggplot(aes(Socialklasse, frekvens)) + 
    geom_col()
  
# Ikke specielt kønt, lad os få farvelagt efter formanten:
    
data %>% 
  filter(Vokal == "a") %>% 
  group_by(Socialklasse) %>% 
  summarise(F1 = mean(F1), F2 = mean(F2)) %>% 
  pivot_longer(cols = c("F1", "F2"),
               names_to = "formant",
               values_to = "frekvens") %>% 
  ggplot(aes(Socialklasse, frekvens)) + 
  geom_col(aes(fill=formant))

# Stakkede søjlediagrammer...

data %>% 
  filter(Vokal == "a") %>% 
  group_by(Socialklasse) %>% 
  summarise(F1 = mean(F1), F2 = mean(F2)) %>% 
  pivot_longer(cols = c("F1", "F2"),
               names_to = "formant",
               values_to = "frekvens") %>% 
  ggplot(aes(Socialklasse, frekvens)) + 
  geom_col(aes(fill=formant), position="dodge")

# Jeg er måske ikke helt overbevist om at jeg kan se forskellen. Men 
# mon ikke vi under alle omstændigheder skulle bruge en statistisk metode
# til at se om der er forskel.

# Herfra ser vi hvad der er af spørgsmål. Ellers:
# Fremragende introduktion til R: https://r4ds.had.co.nz/
#   
# Cheatsheets! https://www.rstudio.com/resources/cheatsheets/
#   
# Ekstra pakker til ekstra fancy ting i ggplot: http://www.ggplot2-exts.org/gallery/

