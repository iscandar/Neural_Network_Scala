/*il codice non è ottimizzato ne punta a esserlo,
lo scopo è quello di sperimentare e di poter cambiare ogni sua parte*/
import scala.math._
class Neurone (val pesi : Array[Double]){

	val erroriPassati = new Array[Double](pesi.size)
	def esegui(inputs: Array[Double]) : Double = {

		/*Calcolo del potenziale di attivazione*/
		var potenziale : Double = 0
		for(i <- 0 until inputs.size){
			potenziale += inputs(i) * pesi(i)
		}
		sigmoid(potenziale)
	}
	def this (inputNum: Int) = {
		/*[-5.5, 5.5] come dominio per i pesi iniziali*/
		this(Array.fill(inputNum)(random * (5.5 *2)- 5.5))
	}


	/*creo il buffer da stampare*/
	override def toString() : String = {
		var stringa = new StringBuilder("\n\t(")
		for(i <- pesi.indices){
			stringa.append("p")
			stringa.append(i)
			stringa.append(":")
			stringa.append(pesi(i))
			stringa.append(", ")
		}
		return stringa + ")"
	}

	def sigmoid(valore : Double) : Double = {
		1/(1 + exp(-valore))
	}
}
