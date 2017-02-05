object Par {
    def seq[A,B](a: =>A)(b: =>B):(A,B) = (a,b)
    def par[A,B](a: =>A)(b: =>B):(A,B) = {
        var resA: Option[A] = None
        val r = new Runnable {
            def run() = resA = Some(a)
        }
        val t = new Thread(r)
        t.start()
        //println(t)
        val resB = b
        t.join()
        (resA.get, resB)
    }
}
