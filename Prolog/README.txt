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
	il programma parte con il predicato:
		lmc_run(filename, [x1, x2, .... , xn], Output).
		
		il predicato richide in input:
			- il file con le istruzioni assembly da eseguire
			- l'input è una lista di n numeri
			  tali che xi abbia valore tra 0 e 999.
		
		il programma darà in output l'esecuzione del file assembly
		nella variabile Output del predicato.
