package mi

import scala.io.Source

object Main extends MIParser  {
  def main(args: Array[String]) {
    val p=new MIParser;
    val source=Source.fromFile("test.txt")
    val text=source.mkString
    
    println(parseAll(PROGRAM,text))
    //val res=p.parseAll(p.PROGRAM,text);
    //if (res.successful) println(res.get) else println("fail")

  }
}