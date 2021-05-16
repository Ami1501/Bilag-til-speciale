# README: Bilag-til-speciale

Til specialet er brugt følgende data (som ligger i mappen data):
Ligevægtede industriafkast fra Frenchs hjemmeside
Værdivægtede industriafkast fra Frenchs hjemmeside
Markedsafkast fra Frenchs hjemmeside
BAB afkast fra AQRs hjemmeside
TED-spread fra FRED hjemmeside
Citeringer fra Google Scholar


Til genskabelse af resultaterne i BAB er R dokumentet "Genskabelse af BAB" brugt
Til scenariet LigeLige er R dokumentet "Version LigeIndustri LigeMarked" brugt
Til scenariet VærdiVærdi er R dokumentet "Version VærdiIndustri VærdiMarked" brugt
Til scenariet LigeVærdi + videreudvikling er R dokumentet "Version LigeIndustri VærdiMarked" brugt

Disse scipts skal køres fra start til slut, da variable overskrives løbende.


I R koderne for de forskellige scenarier findes følgende sektioner:
Rådata
-Indlæsning af data

Data transformering
-Sortering af data og defination af variable

Simpel beregning
-Estimering af beta

Kompliceret beregning
-Estimering af beta

P1-P10
-Sortering og porteføljedannelse baseret på beta-værdier 

BAB
-Implementering af BAB-faktoren

wit shrinkingfaktor
-Konstruktion af wit



For R-dokumentet der indeholder "+ videre arbejde" er yderligere sektioner i koden:
WT
-Opdatering af betaværdier med ny Shrinkingfaktor

P1-P10
-Porteføljerne opdateres på barrgund af nye wt

BAB PF
-BAB porteføljen opdateres på barrgund af nye wt

TedSpread
-Beregning af TED-spread

Pre og Post CAPM P1-P10
-Opdeling af porteføljerne i nævnte periode

BAB pre og post CAPM
-Opdeling af BAB-afkastet i nævnte periode

SML pre og post
-Forskellen mellem SML før og efter CAPM

BAB præ og post smart beta
-Før og efter 1993

BAB præ og post BAB
-Før og efter 2013

Sammenligning med AQR datasæt
-Opdelinger i AQRs datasæt, så vores tal kan sammenlignes i de forskellige perioder.