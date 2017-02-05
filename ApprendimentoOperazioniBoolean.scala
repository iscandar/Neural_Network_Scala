/*il codice non è ottimizzato ne punta a esserlo,
lo scopo è quello di sperimentare e di poter cambiare ogni sua parte*/
object ApprendimentoOperazioniBoolean{
  def or(prove: Int,nomeFile: String)={
    val perceptrone = new Perceptrone(2,Array[Int](2,10,20,1))
    /*gli input da analizzare*/
    /*devo considerare che linsieme di apprendimento deve essere lineramente separabile*/
    val inputs = Array[Array[Double]](
      Array[Double](0,0),
      Array[Double](1,0),
      Array[Double](0,1),
      Array[Double](1,1)
    )
    /*output atteso*/
    val outputs = Array[Double](0,1,1,1)
    println("Apprendimento OR su prove="+prove)
    /*iterazioni per l'apprendimento*/
    for(i <- 0 until prove) {
      for(i <- 0 until inputs.size) {
        perceptrone.esegui(inputs(i))
        perceptrone.calcoloErrori(inputs(i), Array[Double](outputs(i)))
        perceptrone.apprendimento(inputs(i), Array[Double](outputs(i)))
      }
    }

    println("Salvo la configurazione del perceptrone nel file"+nomeFile)
    perceptrone.saveXml(nomeFile)
  }
  def xor (prove: Int,nomeFile: String)= {
    val perceptrone = new Perceptrone(2,Array[Int](2,100,50,1))
    /*gli input da analizzare*/
    val inputs = Array[Array[Double]](
      Array[Double](0,0),
      Array[Double](0,1),
      Array[Double](1,0),
      Array[Double](1,1)
    )
    /*output atteso*/
    val outputs = Array[Double](0,1,1,0)
    println("Apprendimento su prove="+prove)
    /*iterazioni per l'apprendimento*/
    for(i <- 0 until prove) {
      for(i <- 0 until inputs.size) {
        perceptrone.esegui(inputs(i))
        perceptrone.calcoloErrori(inputs(i), Array[Double](outputs(i)))
        perceptrone.apprendimento(inputs(i), Array[Double](outputs(i)))
      }
    }

    println("Salvo la configurazione del perceptrone nel file "+nomeFile)
    perceptrone.saveXml(nomeFile)
  }
}
