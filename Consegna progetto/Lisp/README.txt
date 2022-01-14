La soluzione adottata presenta alcune peculiarità derivanti dalla nostra personale interpretazione della traccia che 
intendiamo chiarire in questo file di testo.

1)	La logica dietro alla nostra implementazione è quella di costruire un parser lineare, che legge carattere dopo 
	carattere e ne controlla la validità. Ciascuno 'stato' (scheme, userinfo, host, port, path, query, fragment) cerca 
	di estrapolare dalla stringa passata ciò che riconosce come proprio, poi inoltra il resto della stringa allo 'stato' successivo. L'ultimo 'stato' si occuperà inoltre di controllare che la stringa ricevuta, a seguito della sua 
	elaborazione, sia vuota. Il caso contrario significa che una certa sequenza di caratteri della stringa non è stata 
	accettata (e quindi estrapolata) da alcuno stato e perciò la stringa totale non risulta essere una URI valida.
	
2)	Le funzioni uri-parse fungono da 'root', cioè contengono tutte le chiamate alle funzioni che estrapolano dalla 
	stringa "la propria parte" e convogliano il resto della stringa scartata dalla funzione precedente verso la funzione successiva.

3) 	Tutti i caratteri previsti dalla tabella ASCII (salvo quelli eventualmente non supportati dall'interprete Lisp) sono
	da considerarsi accettabili. Perciò non sono previsti ulteriori "filtri" sui caratteri ammessi, se non quelli 
	specificati dalla traccia.

4)	Il carattere space viene gestito in modo tale che, qualora venisse letto dalla funzione che si occupa di gestire i 
	caratteri, questo venga convertito nella sequenza '%20'.
	
5)	L'interpretazione delle funzioni uri-parse è strutturata in modo tale da fornire 2 possibili tipi di risposta a 
	seguito di un'invocazione: 
	- ritorno di una struct denominata 'uri-structure', contenente i campi di una URI e i corrispondenti valori 
	  estrapolati
	- 'NIL' nel caso in cui la stringa passata come parametro non corrisponda ad una URI valida.
	
6)	L'interpretazione delle funzioni uri-scheme, uri-userinfo, ecc. è strutturata in modo tale da fornire 2 possibili 
	tipi di	risposta a seguito di un'invocazione: 
	- ritorno della stringa o intero corrispondente al valore del campo richiesto
	- 'NIL' nel caso in cui il paramentro passato non sia di tipo uri-structure (non valido), oppure il valore richiesto 
	  associato al campo sia proprio 'NIL'.

7)	La componente 'path' nel caso di schema 'zos' è obbligatoria solo se prima viene letto uno '/', che introduce la 
	parte di path-query-fragment.