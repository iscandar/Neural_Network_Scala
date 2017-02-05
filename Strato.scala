/*il codice non è ottimizzato ne punta a esserlo,
lo scopo è quello di sperimentare e di poter cambiare ogni sua parte*/
class Strato(val inputNum: Int, val neuroni : Array[Neurone]) {
	val outputs = new Array[Double](neuroni.size)
	val errori = new Array[Double](neuroni.size)
	var erroriAccumulati : Double = _
	/*esegui strato hidden*/
	def esegui (inputs: Array[Double]) = {
		for(i <- 0 until outputs.size) {
			outputs(i) = neuroni(i).esegui(inputs)
		}
	}

	def this(inputNum: Int, neuronNum: Int) = {
		/*L'ultimo neurone è il neurone di bias*/
		this(inputNum, Array.fill(neuronNum)(new Neurone(inputNum + 1)))
	}
	/*esegui del buffer per i neuroni*/
	override def toString() : String = {
		var stringa = new StringBuilder("[")
		neuroni.foreach { neurone =>
			stringa.append(neurone)
			stringa.append(",")
		}
		stringa.append("\n]\n")
		stringa.toString
	}
}
