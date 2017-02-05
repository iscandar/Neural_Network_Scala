/*il codice non è ottimizzato ne punta a esserlo,
lo scopo è quello di sperimentare e di poter cambiare ogni sua parte*/
import java.io._
object ApprendimentoTris{
  def calcoloProssimaPosizione(tavola: Array[Double]):Int={
    val random=scala.util.Random
    while(true){
      if(tavola.contains(0.0)){
        val r=random.nextInt(9)
        if(tavola(r).equals(0.0)){
          return r
        }
      }else{
        return -1
      }
    }
    return -1
  }

  def calcoloVincita(tavola:Array[Double]):Double={
    val v=0.0
    if(tavola(0).equals(tavola(1)) && tavola(0).equals(tavola(2)) && !tavola(0).equals(v))
    return tavola(0)
    if(tavola(3).equals(tavola(4)) && tavola(3).equals(tavola(5))&& !tavola(3).equals(v))
    return tavola(3)
    if(tavola(6).equals(tavola(7)) && tavola(6).equals(tavola(8))&& !tavola(6).equals(v))
    return tavola(6)
    if(tavola(0).equals(tavola(3)) && tavola(0).equals(tavola(6))&& !tavola(0).equals(v))
    return tavola(0)
    if(tavola(1).equals(tavola(4)) && tavola(1).equals(tavola(7))&& !tavola(1).equals(v))
    return tavola(1)
    if(tavola(2).equals(tavola(5)) && tavola(2).equals(tavola(8))&& !tavola(2).equals(v))
    return tavola(2)
    if(tavola(0).equals(tavola(4)) && tavola(0).equals(tavola(8))&& !tavola(0).equals(v))
    return tavola(0)
    if(tavola(2).equals(tavola(4)) && tavola(2).equals(tavola(6))&& !tavola(2).equals(v))
    return tavola(2)
    return 0
  }
  def calcoloPartita(numeroPartite: Int,nomeCartella: String){
    val random = scala.util.Random
    var giocatore=0
    for(n <- 0 until numeroPartite){
      var tavola =Array[Double](0,0,0,0,0,0,0,0,0)
      var posizione = calcoloProssimaPosizione(tavola)
      val r =random.nextBoolean()
      /*scelta del giocatore in modo casuale per ogni partita*/
      if (r){
        giocatore=1
      }else{
        giocatore=2
      }
      while(posizione> -1 && calcoloVincita(tavola).equals(0.0)){
      //  println(tavola.mkString("<",",",">"))
        tavola(posizione)=giocatore
        if(giocatore.equals(1)){
          giocatore=2
        }else{
          giocatore=1
        }
        posizione=calcoloProssimaPosizione(tavola)
        scala.tools.nsc.io.File(nomeCartella+"DataSetTrisMosseInput.txt").appendAll(tavola.mkString("", " ", "\n"))
        if(calcoloVincita(tavola).equals(1.0)){
          scala.tools.nsc.io.File(nomeCartella+"DataSetTrisRisultatoOutput.txt" ).appendAll("1\n")
        }else{
          scala.tools.nsc.io.File(nomeCartella+"DataSetTrisRisultatoOutput.txt" ).appendAll("0\n")
        }
      }
      println("Sto calcolando "+n)
      //  scala.tools.nsc.io.File(nomeCartella+"DataSetTrisPartite.txt" ).appendAll(tavola.mkString("", " ", "\n"))
    }
  }
 def calcoloPermutazioni(scelta: Int,nomeCartella: String):Unit={
    var n=0
    for( a <- 0 until 3){
      for( b <- 0 until 3){
        for( c <- 0 until 3){
          for( d <- 0 until 3){
            for( e <- 0 until 3){
              for( f <- 0 until 3){
                for( g <- 0 until 3){
                  for( h <- 0 until 3){
                    for( i <- 0 until 3){
                      val tavola= Array[Double](a,b,c,d,e,f,g,h,i)
                      n=n+1
                      if(scelta.equals(0)){
                        if(calcoloVincita(tavola).equals(1.0)){
                          scala.tools.nsc.io.File(nomeCartella+"DataSetTrisRisultatoOutput.txt" ).appendAll("1\n")
                          scala.tools.nsc.io.File(nomeCartella+"DataSetTrisMosseInput.txt.txt" ).appendAll(tavola.mkString("", " ", "\n"))
                        }
                      }else{
                        scala.tools.nsc.io.File(nomeCartella+"DataSetTrisMosseInput.txt.txt" ).appendAll(tavola.mkString("", " ", "\n"))
                        if(!calcoloVincita(tavola).equals(0.0)){
                          scala.tools.nsc.io.File(nomeCartella+"DataSetTrisRisultatoOutput.txt" ).appendAll("1\n")
                        }else{
                          scala.tools.nsc.io.File(nomeCartella+"DataSetTrisRisultatoOutput.txt" ).appendAll("0\n")
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    println("tutte le prove sono "+n)
  }
  def letturaDataSetInput(nomeCartella: String):Array[Array[Double]]={
    var inputs=Array[Array[Double]]()

    for (linea <- scala.io.Source.fromFile(nomeCartella+"DataSetTrisMosseInput.txt").getLines()) {
      val riga= linea.split(" ").map(_.toDouble)
      inputs = (inputs :+ riga)
println("Sto caricando inputs" + linea.toString)
    }
    inputs
  }
  /*la scelta del tipo txt rispetto al xml è sul fatto che la rampresentazione è piu semplice in txt*/
  /*per i neuroni e la loro rappresentazione è meglio scegliere una rapresentazione xml*/
  def letturaDataSetOutput(nomeCartella: String):Array[Double]={
    var output=Array[Double]()

    for (linea <- scala.io.Source.fromFile(nomeCartella+"DataSetTrisRisultatoOutput.txt").getLines()) {
      output = (output :+ linea.toDouble)
println("Sto caricando outputs"+linea.toString)
    }
    output
  }
  def tris(prove: Int,scelta: Int,nomeCartella: String)={
   if(scelta.equals(1)){
      calcoloPartita(prove,nomeCartella)
    }else if( scelta.equals(2)){
      calcoloPermutazioni(0,nomeCartella)
    }else if(scelta.equals(3)){
      calcoloPermutazioni(1,nomeCartella)
    }else{}
    val perceptrone = new Perceptrone(9,Array[Int](9,27,1))
    val inputs=letturaDataSetInput(nomeCartella)
    val outputs=letturaDataSetOutput(nomeCartella)
    for(i <- 0 until prove) {
      for(i <- 0 until inputs.size) {
        /*iterazioni per l'apprendimento*/
        perceptrone.esegui(inputs(i))
        perceptrone.calcoloErrori(inputs(i), Array[Double](outputs(i)))
        perceptrone.apprendimento(inputs(i), Array[Double](outputs(i)))
      }
      println("Sto imparando partita numero = "+i)
    }

    println("Salvo la configurazione del perceptrone nel file PerceptroneTris.xml")
    perceptrone.saveXml(nomeCartella+"PerceptroneTris.xml")
  }

  def main (args: Array[String]): Unit = {
//  numero delle partite massime(con corrispondenti mosse)= 9
  tris(5000,1,"DataSet/")
}
}
