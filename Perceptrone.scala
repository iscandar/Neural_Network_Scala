/*il codice non è ottimizzato ne punta a esserlo,
lo scopo è quello di sperimentare e di poter cambiare ogni sua parte*/
import scala.math._
import scala.xml._
import scala.collection.mutable.ListBuffer
class Perceptrone (val strati : Array[Strato]){
	def esegui (inputs: Array[Double]) : Array[Double] = {
		/* l'input per il primo strato sono gli input dei perceptroni*/
		var stratoInput = inputs
		for(i <- 0 until strati.size) {
			/*calcolo del output di ogni strato*/
			strati(i).esegui(stratoInput)
			/*gli outputs sono gli inputs del prossimo strato*/
			stratoInput = strati(i).outputs
		}
		/*ritorno l'output del ultimo strato*/
		return stratoInput
	}

	def this(perceptroneInputNum : Int, neuroniStratoNum: Array[Int]) = {
		this(new Array[Strato](neuroniStratoNum.size))
		/*Inserimento del primo Strato uguale alle quantita di perceptroni*/
		var stratoInputNum = perceptroneInputNum
		/*esegui di ogni Strato con il giusto numero di neuroni*/
		for(i <- 0 until neuroniStratoNum.size){
			/*esegui singolo Strato*/
			strati(i) = new Strato(stratoInputNum, neuroniStratoNum(i))
			/*il numero di inputs del prossimo strato sono l'attuale quantita di neuroni*/
			stratoInputNum = neuroniStratoNum(i)
		}
	}
	/*funzione di apprendimento QUI CI VA LA FORMULA!!!!*/
	def apprendimento (inputs: Array[Double], outputs: Array[Double]) : Double = {

		val livelloApprendimento : Double = 0.1
		val alfa : Double =  0.15
		/*apprendimento per tutti gli strati tranne il primo*/
		for(stratoIndex <- strati.size - 1 to(1, -1)) {
			/*strato corrente*/
			val strato = strati(stratoIndex)
			/*strato precedente*/
			val stratoPrecedente = strati(stratoIndex - 1)
			/*per ogni neurone dello strato corrente*/
			for(neuroneIndex <- 0 until strato.neuroni.size){

				/*neurone corrente e il suo errore*/
				val neurone = strato.neuroni(neuroneIndex)
				val erroreNeurone = strato.errori(neuroneIndex)

				/*per ogni neurone nel precedente strato*/
				for(neuronePrecedenteIndex <- 0 until stratoPrecedente.neuroni.size){

					neurone.erroriPassati(neuronePrecedenteIndex) =livelloApprendimento * erroreNeurone * stratoPrecedente.outputs(neuronePrecedenteIndex) +alfa * neurone.erroriPassati(neuronePrecedenteIndex)
					neurone.pesi(neuronePrecedenteIndex) = neurone.erroriPassati(neuronePrecedenteIndex) + neurone.pesi(neuronePrecedenteIndex)
				}
				/*aggiornamento del neurone di bias*/
				neurone.erroriPassati(stratoPrecedente.neuroni.size) =(-livelloApprendimento) * erroreNeurone + alfa * neurone.erroriPassati(stratoPrecedente.neuroni.size)
				neurone.pesi(stratoPrecedente.neuroni.size) = neurone.erroriPassati(stratoPrecedente.neuroni.size) + neurone.pesi(stratoPrecedente.neuroni.size)
			}
		}

		/*per il primo strato*/
		val strato = strati.head
		for(neuroneIndex <- 0 until strato.neuroni.size){

			/* neurone corrente è il suo errore*/
			val neurone = strato.neuroni(neuroneIndex)
			val erroreNeurone = strato.errori(neuroneIndex)

			/*per qualunque input*/
			for(inputIndex <- 0 until inputs.size){

				neurone.erroriPassati(inputIndex) =
					livelloApprendimento * erroreNeurone * inputs(inputIndex) +
					alfa * neurone.erroriPassati(inputIndex)

					neurone.pesi(inputIndex) = neurone.erroriPassati(inputIndex) + neurone.pesi(inputIndex)
				}

				/*aggiornamento del bias*/
				neurone.erroriPassati(inputs.size) =(-livelloApprendimento) * erroreNeurone + alfa * neurone.erroriPassati(inputs.size)
				neurone.pesi(inputs.size) = neurone.erroriPassati(inputs.size) + neurone.pesi(inputs.size)
			}
			/*calcolo del errore tra l'output e il valore atteso (notare che è quadratico)*/
			var somma : Double = 0
			for (i <- 0 until outputs.size) {
				somma += pow(outputs(i) - strati.last.outputs(i), 2)
			}
			sqrt(somma)
		}

		def calcoloErrori (inputs: Array[Double], outputs: Array[Double]) = {

			/*per ogni strato partendo dal ultimo */
			for (stratoIndex <- strati.size - 1 to(0,-1)) {
				/*strato corrente*/
				val strato = strati(stratoIndex)
				/*per ogni neurone dello strato*/
				for(neuroneIndex <- 0 until strato.neuroni.size){

					/*neurone corrente*/
					val neurone = strato.neuroni(neuroneIndex)
					/*output neurone corrente*/
					val neuroneOutput = strato.outputs(neuroneIndex)
					/*ultimo strato calolo il fattore di errore*/
					if(stratoIndex == strati.size - 1){
						strato.errori(neuroneIndex) = neuroneOutput * (1 - neuroneOutput) * (outputs(neuroneIndex) - neuroneOutput)
					} else {
						/*strati nascosti*/
						var tmp : Double = 0
						for(prossimoNeurone <- 0 until strati(stratoIndex + 1).neuroni.size)
						tmp += strati(stratoIndex + 1).errori(prossimoNeurone) * strati(stratoIndex + 1).neuroni(prossimoNeurone).pesi(neuroneIndex)
						strato.errori(neuroneIndex) = neuroneOutput * (1 - neuroneOutput) * tmp
					}
				/*	calcolo l'errore cumulativo dello strato*/
					strato.erroriAccumulati =neurone.pesi.reduceLeft( _ + strato.errori(neuroneIndex) * _)
				//	println("errore fin ora per strato "+ stratoIndex +" è pari ha " +strato.erroriAccumulati)
					}

				}
			}

			override def toString() : String = {
				var stringa = new StringBuilder
				strati.foreach { strato => stringa.append(strato.toString) }
				stringa.toString
			}

			def toXml() : Elem = {
				val perceptroneXml =
					<Perceptrone strati={strati.size.toString}>
					{for (strato <- strati) yield <Strato inputs={strato.inputNum.toString} neuroni={strato.neuroni.size.toString}>
					{for (neurone <- strato.neuroni) yield <Neurone inputs={neurone.pesi.size.toString}>
					{for (pesi <- neurone.pesi) yield
						<pesi>{pesi.toString}</pesi>
					}
					</Neurone>
				}
				</Strato>
			}
			</Perceptrone>

			return perceptroneXml
		}

		def saveXml(filePath : String) = {
			XML.save(filePath, toXml(), "UTF-8", true, null)
		}
	}
	object Perceptrone {
		def loadXml(filePath : String) : Perceptrone = {
			val perceptroneXml = XML.load(filePath)
			val strati = new ListBuffer[Strato]

			for(stratoXml <- perceptroneXml \\ "Strato") {
				val neuroni = new ListBuffer[Neurone]
				val inputs : Int = (stratoXml \ "@inputs").text.toInt

				for(neuroneXml <- stratoXml \\ "Neurone") {

					val pesi = new ListBuffer[Double]

					for(pesiXml <- neuroneXml \\ "pesi") {
						pesi += pesiXml.text.toDouble
					}

					neuroni += new Neurone(pesi.toArray)
				}

				strati += new Strato(inputs, neuroni.toArray)
			}

			new Perceptrone(strati.toArray)
		}
	}
