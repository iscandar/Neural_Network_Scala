/*il codice non è ottimizzato ne punta a esserlo,
lo scopo è quello di sperimentare e di poter cambiare ogni sua parte*/
object OperazioniBoolean{
  def eseguiOr (nomeFile: String)= {

    println("Carico la configurazione"+nomeFile)
    var perceptrone = Perceptrone.loadXml(nomeFile)

    println("Valutazione degli input= ")
    print("0,0 => ")
    perceptrone.esegui(Array[Double](0,0)).foreach(i =>{println(i)})

    print("0,1 => ")
    perceptrone.esegui(Array[Double](0,1)).foreach(i =>{println(i)})

    print("1,0 => ")
    perceptrone.esegui(Array[Double](1,0,1)).foreach(i =>{println(i)})

    print("1,1 => ")
    perceptrone.esegui(Array[Double](1,1)).foreach(i =>{println(i)})

  }
  def eseguiXor (nomeFile: String)= {

    println("Carico la configurazione "+nomeFile)
    val perceptrone = Perceptrone.loadXml(nomeFile)

    println("Valutazione degli input= ")
    print("0,0 => ")
    perceptrone.esegui(Array[Double](0, 0)).foreach(i =>{println(i)})

    print("0,1 => ")
    perceptrone.esegui(Array[Double](0, 1)).foreach(i =>{println(i)})

    print("1,0 => ")
    perceptrone.esegui(Array[Double](1, 0)).foreach(i =>{println(i)})

    print("1,1 => ")
    perceptrone.esegui(Array[Double](1, 1)).foreach(i =>{println(i)})
  }

}
