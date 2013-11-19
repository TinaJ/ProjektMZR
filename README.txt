Projekt pri predmetu matematika z računalnikom
Naslov: Verjetnost prekomernega prileganja
Avtor: Tina Janša
Mentor: Rok Erman

V datoteki S&P500.txt so podatki, katera podjetja so v indeksu S&P 500 
	vir Wikipedija: http://en.wikipedia.org/wiki/List_of_S%26P_500_companies [9.11.2013]
V datoteki SP500.rda so podatki za cene delnic vseh 500 podjetij v indeksu S&P 500 od 1.1.2000 do 1.11.2013
V datoteki data.rda so obdelani podatki:
	samo close vrednosti za delnice, ki obstajajo dovolj dolgo (imajo manj kot 10 missing values) in za tiste dni, ko noben papir nima missing value
Postopek obdelave podatkov je v datoteki obdelavaPodatkov.R