partecipanti:	
	Magazzù Gaetano 829685

introduzione:
	LMC(Little Man Computer) è
	un semplice modello di computer con:
		- una memoria composta da 100 celle di memoria
		  ogni cella comprende valori da 0 a 999.
		- un solo registro, chiamato accumulatore
		  che prevede valori da 0 a 99.
		- una coda di input e una di output.

utilizzo:
	il programma simula il lmc(little man computer)
	il programma parte con la funzione:
		(lmc-run filename '(x1, x2, .... , xn))
		
		la funzione richide in input:
			- il file con le istruzioni assembly da eseguire
			- l'input è una lista di n numeri
			  tali che xi abbia valore tra 0 e 999.
		
		la funzione da in output l'esecuzione del file assembly,
		il risultato della funzione sarà anche il risultato del programma.
