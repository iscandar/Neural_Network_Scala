/*il codice non è ottimizzato ne punta a esserlo,
lo scopo è quello di sperimentare e di poter cambiare ogni sua parte*/
object GiocoTris{
  def verificaPosizione(tavola: Array[Double],pos: Int):Boolean={
    if(tavola(pos).equals(0.0)){
      return true
    }
    return false
  }
  def caricamentoGiocoTris(nomeCartella: String):Double = {
  	//	println("Carico la configurazione PerceptroneTris.xml")
    //  println("esegui giocatore1")
      val mente = Perceptrone.loadXml(nomeCartella+"PerceptroneTris.xml")
  //    println("Inizializzazione delle vaariabili")
      /*ricorda che entrabi i giocatori vedono il gioco come se fossero delle x*/
      var tavola =Array[Double](0,0,0,0,0,0,0,0,0)
      var giocatore=0
      val random = scala.util.Random
      var r =random.nextBoolean()
      if (r){
        giocatore=1
      }else{
        giocatore=2
      }
      //prima mossa random da un giocatore random
      val t=random.nextInt(9)
      tavola(t)=giocatore
    /*  println("Scelta del giocatore iniziare inmodalita random, il giocatore iniziale è: "+giocatore.toString )
      println("inizio gioco: tavola = "+tavola.mkString("<",",",">")+"\nGiocatore= ")*/
      while(tavola.contains(0.0) && ApprendimentoTris.calcoloVincita(tavola).equals(0.0)){
        var pScelta=Array[Double](0,0,0,0,0,0,0,0,0)
        //println("STO GIOCANDO 0: tavola = "+tavola.mkString("<",",",">")+"\nGiocatore= "+giocatore.toString)
          for (n <- 0 until 9){
            val tavolaTemp= Array[Double](0,0,0,0,0,0,0,0,0)
            Array.copy(tavola, 0, tavolaTemp, 0, tavola.size)
            if(verificaPosizione(tavolaTemp,n)){
              if(giocatore.equals(2.0)){
                /*inverto la tavola*/
                tavolaTemp.map(e => if(e==1) 3 else e)
                tavolaTemp.map(e => if(e==2) 1 else e)
                tavolaTemp.map(e => if(e==3) 1 else e)
              }
              tavolaTemp(n)=1.0
              val x=mente.esegui(tavolaTemp)
              pScelta(n)=x(0)
              //println("....: pScelta = "+pScelta.mkString("<",",",">")+"\nGiocatore= "+giocatore.toString)
            }else{
              pScelta(n)=0
            }
          }
          //println("STO GIOCANDO 1: tavola = "+tavola.mkString("<",",",">")+"\nGiocatore= "+giocatore.toString)
          if(giocatore.equals(1)){
          tavola(pScelta.indexOf(pScelta.max))=1.0
          giocatore=2
        }else{
          tavola(pScelta.indexOf(pScelta.max))=2.0
          giocatore=1
        }

        //println("STO GIOCANDO 2: tavola = "+tavola.mkString("<",",",">")+"\nGiocatore= "+giocatore.toString)
      }
      println("Gioco  terminato risultato= "+tavola.mkString("<",",",">")+"\n Vincitore= "+ApprendimentoTris.calcoloVincita(tavola).toString)
      return ApprendimentoTris.calcoloVincita(tavola)
  }
  def eseguiTris(prove: Int,nomeCartella: String){
    for(n <- 0 until prove){
     caricamentoGiocoTris(nomeCartella)
    }
}
  def main (args: Array[String]): Unit = {
      /*questo gioco è giocatore macchina contro se stessa, implementando un
      data set non completamente finito, per garantire una realistica battaglia */
      var g:Array[Double] = Array[Double](0,0,0)
      var x:Double=0.0;
      for(n <- 0 until 10000){
        x = caricamentoGiocoTris("DataSet/")
        if (x==1.0) g(1)+=1
        else if (x==2.0) g(2)+=1
        else g(0)+=1
      }
      println("risultato ricerca: " + g.mkString("<", ",", ">"))
   }
}
