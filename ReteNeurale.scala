/*il codice non è ottimizzato ne punta a esserlo,
lo scopo è quello di sperimentare e di poter cambiare ogni sua parte*/
object ReteNeurale {
	/*alfa è impostato a 0.1 quindi tiene molto conto del passato e devo fare molte piu prove*/
	/*con livello di apprendimento pari a 0.15*/

	def main (args: Array[String]): Unit = {
		val prove=5000
		import Par._
		//avvio del apprendimento
		//	par {
		ApprendimentoOperazioniBoolean.xor(prove,"DataSet/PerceptroneXor.xml")
		//	} {
		ApprendimentoOperazioniBoolean.or(prove,"DataSet/PerceptroneOr.xml")
		//	}
		//avvio esecuzione
		//	par {
		OperazioniBoolean.eseguiXor("DataSet/PerceptroneXor.xml")
		//	} {
		OperazioniBoolean.eseguiOr("DataSet/PerceptroneOr.xml")
		//	}

		/*il tris ha diversi tipi di apprendimento : 1 partite casuali(prove viene usato anche per i cicli di apprendimento),
		2 cerca le partite in cui vince, 3 tutte le permutazioni possibili
		tris(numero di prove,scelta,cartella dove salvare)
		inoltre come vincite lui considera solo il giocatore x dato che il giocatore O è sempre se stesso*/
		//ApprendimentoTris.tris(5000,1,"DataSet/")
		/* eseguiTris richiede numero di partite da provare e cartella dei file dataset "nomecartella"*/
		GiocoTris.eseguiTris(1000,"DataSet/")

	}

}
