Projekt pri predmetu matematika z računalnikom
Naslov: Verjetnost prekomernega prileganja
Avtor: Tina Janša
Mentor: Rok Erman

####################
DATA:
V datoteki S&P500.txt so podatki, katera podjetja so v indeksu S&P 500 
	vir Wikipedija: http://en.wikipedia.org/wiki/List_of_S%26P_500_companies [9.11.2013]
V datoteki SP500.rda so podatki za cene delnic vseh 500 podjetij v indeksu S&P 500 od 1.1.2000 do 1.11.2013
V datoteki SP.rda so podatki za ceno indeksa S&P 500 od 1.1.2000 do 1.11.2013
V datoteki podatki.rda so obdelani podatki:
	samo close vrednosti za delnice, ki obstajajo dovolj dolgo in nimajo več kot 3 NA zapored
	ostale manjkajoče vrednosti so linearno interpolirane
Postopek obdelave podatkov je v datoteki obdelavaPodatkov.R

######################
strategije.R
V tej datoteki so funkcije za nekaj trgovalnih strategij: SMA, RSI, Buy&Hold, Bollinger, Random
v vseh strategijah samo kupujem, nič ne prodajam
kot rezultat strategije, vrnem dnevne vrednosti:
	če je pogoj, da kupiš delnico, je vrednost close tistega dne / close prejšnjega dne * vrednost prejšnjega dne
	če ne kupiš delnice, je vrednost enaka vrednsoti prejšnjega dne
	pri buy & hold je vrednost vedno taka kot, če bi kupil

1. SMA = Simple Moving Average
	strategija: če je close prejšnjega dne nad SMA prejšnjega dne, kupi, (če je pod, prodaj)
	SMAstrategy(close, SMAn, n, zacetek=1, budget = 1000)
	close = vektor close cen
	SMAn = vektor moving averages izračunanih na n podatkih
		zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
		n = dolžina na katerih je izračunan moving average
		budget = znesek, ki ga vložimo v dani papir
  
2. RSI = Relative Strength Index
	strategija: če je RSI prejšnjega dne pod 30, kupi, če je nad 70, prodaj
	RSI = 100 + 100/(1 + RS)
	RS = Relative Strength = Average Gain / Average Loss
	First Average Gain = Sum of Gains over the past n periods / n
	First Average Loss = Sum of Losses over the past n periods / n
	Average Gain = [(previous Average Gain) x (n-1) + current Gain] / n
	Average Loss = [(previous Average Loss) x (n-1) + current Loss] / n
	RSIstrategy(close, RSIn, n, zacetek = 1, budget = 1000)
		close = vektor close cen
		RSIn = RSI indeks izračunan na dolžini n
		zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
		n = dolžina na kateri računam povprečno izgubo/dobiček
		budget = znesek, ki ga vložimo v dani papir
 
3. Buy & Hold
	strategija: na začetku kupiš delnico in potem jo imaš
	vrednosti računaš, kot če bi jo vsak dan kupil
	BuyHoldStrategy(close, zacetek = 1, budget = 1000)
		close = vektor close cen
		zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
		budget = znesek, ki ga vložimo v dani papir
  
4. Bollinger
	strategija: če je close prejšnjega dne pod spodnjim pasom prejšnjega dne, kupi, (če je nad zgornjim pasom prejšnjega dne, prodaj)
	spodnji pas = SMA - STD * faktor
	zgornji pas = SMA + STD * faktor
	STD = stadnardna deviacija na n podatkih
	BollingerStrategy(close, upBand, lowBand, n, zacetek = 1, budget = 1000)
		close = vektor close cen
		upBand = zgornji pas bollinger indeksa izračunanega na n podatkih
		lowBand = spodnji pas bollinger indeksa izračunanega na n podatkih
		zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
		n = dolžina na kateri računam povprečno izgubo/dobiček
		budget = znesek, ki ga vložimo v dani papir
  
5. Random
	strategija: slučajno izberi ali kupiš, prodaš ali nič ne narediš
	randomStrategy(close, zacetek = 1, budget = 1000)
		close = vektor close cen
		zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
		budget = znesek, ki ga vložimo v dani papir

#########################
uporabaStrategij.R
na delnicah, ki so v indeksu S&P 500, oz. na podatkih, ki jih imam v datoteki prodatki.rda uporabim naslednje strategije:
	SMA5, 25, 50, 150
	RSI2, 14
	Buy&Hold
	Bollinger za n = 20 in faktor = 2
	Random
za vsako strategijo izračunam dnevne vrednosti za vsako delnico
začnem trgovati pri dnevu 150, ker imam SMA150, kjer je prvih 150 vrednosti enako NA
vsaki delnici na začetku namenim enak delež v portfelju
izračunam dnevne vrednosti celotnega portfelja za vsako strategijo (vsota dnevnih vrednosti za vsako delnico pri posamezni strategiji)
te vsote združim v matriko
izračunam stopnjo donosa portfelja za vsako strategijo (današnja vrednost / včerajšnja vrednost)
izračunam donos portfelja za vsako strategijo (stopnja donosa - 1 oz. (današnja vrednost - včerajšnja vrednost) / včerajšnja vrednost)
odstranim prvih 150 vrstic, ki so NA (ker je začetek 150)

############################
CSCV.R

#########################
uporabaCSCV.R

#######################
analizaCSCV.R

##########################
holdOut.R