La soluzione adottata presenta alcune peculiarità derivanti dalla nostra personale interpretazione della traccia che intendiamo chiarire in questo file di testo.

1)	La logica dietro alla nostra implementazione è quella di costruire un parser lineare, che legge carattere dopo carattere e ne 
	stabilisce la validità. Ciascuno "stato" (scheme, userinfo, host, port, path, query, fragment) cerca di estrapolare dalla stringa passata ciò che riesce a riconoscere come proprio, poi inoltra il resto della stringa allo "stato" successivo. L'ultimo "stato" si occuperà, tra l'altro, di controllare che la stringa ricevuta, a seguito alla sua elaborazione, sia vuota. In caso contrario vorrà dire che una certa sequenza di caratteri della stringa non sarà stata accettata (e quindi estrapolata) da alcuno stato e perciò la stringa totale non si tratta di una URI valida.
	
2)	I predicati uri_parse fungono da 'root', contenendo tutti i sottogoal che estrapolano dalla stringa "la propria parte" e 
	convogliando il resto della stringa scartata dal sottogoal precedente verso quello successivo.

3) 	Tutti i caratteri previsti dalla tabella ASCII (salvo quelli eventualmente non supportati dall'interprete Prolog) sono
	da considerarsi accettabili. Perciò non sono previsti ulteriori "filtri" sui caratteri ammessi se non quelli specificati dalla traccia.

4)	Il carattere space viene gestito in modo tale che, qualora venisse letto dal predicato che si occupa di gestire i caratteri,
	questo verrà convertito nella sequenza '%20'.
	
5)	L'interpretazione dei predicati uri_parse è strutturata in modo tale da fornire 2 possibili tipi di risposta a seguito di 	
	un'invocazione: 
	- ritorno di un termine composto contenente i campi di una URI e corrispondenti i valori estrapolati
	- false nel caso in cui la stringa passata come parametro non corrispondesse ad una URI valida.

6)	La componente 'path' nel caso di schema 'zos' è obbligatoria solo se prima viene letto uno '/', che introdurrebbe la parte di 
	path-query-fragment.