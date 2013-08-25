package mi



import scala.util.parsing.combinator.RegexParsers

class MIParser extends RegexParsers {
  val number = "[0-9]+".r
  val identifier = "[A-Za-z_-]+[A-Za-z_0-9-]*".r;
  val inheritance = "INHERITANCE.".r;
  val syntax = "SYNTAX.".r;
  val start = "Start".r;
  val string="\"[a-zA-Z0-9_ :=><,.+-/*();\"\n\t]*\"".r;

  def PROGRAM: Parser[Any] = {
    inheritance ~ INHERITANCE ~ syntax ~ SYNTAX | inheritance ~ INHERITANCE
  }

  def INHERITANCE: Parser[Any] = {
    rep1(OBJECT_TYPE_DECLARATION ~ ".")     
  }

  def OBJECT_TYPE_DECLARATION: Parser[Any] = {
    OBJECT_TYPE ~ ":" ~ "<" ~ OBJECT_TYPE_LIST~ ">" 
  }

  def SEPARATOR: Parser[Any] = {
    "." | ";"
  }

  def OBJECT_TYPE: Parser[Any] = {
    identifier
  }

  def OBJECT_TYPE_LIST: Parser[Any] = {
    OBJECT_TYPE_ITEM ~ rep(";"~OBJECT_TYPE_ITEM)    
  }

  def OBJECT_TYPE_ITEM: Parser[Any] = {
    OBJECT_TYPE_DECLARATION | OBJECT_TYPE
  } 
  
  def SYNTAX: Parser[Any] = {
    //opt(start ~ "=" ~ identifier ~ ".") ~ rep1(RULE)
    start ~ "=" ~ identifier ~ "." ~ rep1(RULE)
  }

  def RULE:Parser[Any] ={
    identifier ~ "::="~ RH_LIST ~ ";"
  }

  def RH_LIST: Parser[Any] ={
    rep1(RH_ITEM)
  }

  def RH_ITEM:Parser[Any] ={
     ATTRIBUTE_PAIR | CONSTR | identifier | number | string
  }

  def ATTRIBUTE_PAIR: Parser[Any]={
    identifier ~ "=>" ~ identifier
  }

  def CONSTR:Parser[Any]={
    "["~ RH_LIST~ "]" ~ QUAL ~ string
  }

  def QUAL: Parser[Any]={
    "+" | "*" | "!"
  }
}