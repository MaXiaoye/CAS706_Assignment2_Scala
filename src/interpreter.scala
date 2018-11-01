

object interpreter {
  def main(args: Array[String]): Unit ={               
     //test case (\x.(x+2)) 4 
    val exp1 = new Apply2(Lambda(Var("x"),Calc("+",Var("x"),Val(2))), Val(4))
    println("\nResult of test case (\\x.(x+2)) 4")
    interpE(exp1);
    
    //test case ((\x.\y. (x-y))6) ((\x.(x+2))4)
    val exp2 = new Apply2(Apply2(Lambda(Var("x"),Lambda(Var("y"),Calc("-",Var("x"),Var("y")))),Char("a")),exp1)
    println("\nResult of test case ((\\x.\\y. (x-y))6) ((\\x.(x+2))4)")
    interpE(exp2)
    
    //test case (\x.\y. y x) y
    val exp3 = new Apply2((Lambda(Var("x"),Lambda(Var("y"),Apply1(Var("y"),Var("x"))))),Char("y"))
    println("\nResult of test case (\\x.\\y. y x) y")
    interpE(exp3)
    
    //test case (\x. x y) (\x. y x)
    val exp4 = new Lambda(Var("x"),Apply1(Var("x"),Char("y")))
    val exp5 = new Lambda(Var("x"),Apply1(Char("y"),Var("x")))
    val exp6 = new Apply2(exp4,exp5)
    println("\nResult of test case (\\x. x y) (\\x. y x)")
    interpE(exp6)
    
    //test case (\x.\y. x y) (\x. y x)
    val exp7 = new Lambda(Var("x"),Lambda(Var("y"),Apply1(Var("x"),Var("y"))))
    val exp8 = new Apply2(exp7,exp5)
    println("\nResult of test case (\\x.\\y. x y) (\\x. y x)")
    interpE(exp8)
    
    //test case (\x.(true and (Not x)))false
    val exp9 = new Lambda(Var("x"),And(Bool(true),Not(Var("x"))))
    val exp10 = new Apply2(exp9,Bool(false))
    println("\nResult of test case (\\x.(true and (Not x)))false")
    interpE(exp10)
    
    //test case (Let x=5 in (x+1))
    println("\nResult of test case (Let x=5 in (x+1))")
    val exp11 = new Let(Var("x"),Val(5),Calc("+",Var("x"),Val(1)))
    interpE(Let(Var("x"),Val(5),Calc("+",Var("x"),Val(1))))
    
    //test case (Let x=5 in (x+1)) < 2
    println("\nResult of test case ((Let x=5 in (x+1)) < 2)")
    interpE(Lt(exp11,Val(2)))
    
    //test case If ((\x.(true and (Not x)))false) then (Let x=5 in (x+1)) else false
    val exp12 = If(exp10,exp11,Bool(false))
    println("\nResult of test case (If ((\\x.(true and (Not x)))false) then (Let x=5 in (x+1)) else false)")
    interpE(exp12)
    
    //Invalid input: application(abc 2)
    println("\nResult of test case (application(abc 2))")
    interpE(Apply2(Char("abc"),Val(2)))
    
  }
    
  //Class definition for terms.
  sealed trait Expr  
  //Classs for lambda item.
  case class Lambda(arg: Var, body: Expr) extends Expr
  //Class for variable
  case class Var(name: String) extends Expr
  //Class for let
  case class Let(exp1: Var, exp2: Expr, exp3: Expr) extends Expr
  //Class for apply such as (\x \y a b)
  case class Apply1(arg1: Expr, arg2: Expr) extends Expr
  //Class for application
  case class Apply2(exp1: Expr, exp2: Expr) extends Expr
  //Class for arithmetic expression.
  case class Calc(op: String, arg1: Expr, arg2: Expr) extends Expr
  //Class for value.
  case class Val(value: Int) extends Expr
  //Class for Char arg
  case class Char(value: String) extends Expr
  //Class for closure
  case class Closure2(exp: Lambda, env: scala.collection.mutable.Map[String,Any]) extends Expr
  //Class for Bool
  case class Bool(value: Boolean) extends Expr
  //Class for Not
  case class Not(exp1:Expr) extends Expr
  //Class for Or
  case class Or(exp1:Expr, exp2:Expr) extends Expr
  //Class for And
  case class And(exp1:Expr, exp2:Expr) extends Expr
  //Class for Equal
  case class Equal(exp1:Expr, exp2:Expr) extends Expr
  //Class for Less than
  case class Lt(exp1:Expr, exp2:Expr) extends Expr
  //Class for Less than or equal
  case class Lte(exp1:Expr, exp2:Expr) extends Expr
  //Class for If
  case class If(exp1:Expr, exp2:Expr, exp3:Expr) extends Expr
  
  //Rename variables. And handle "stuck" here !
  def rename(exp: Expr): Expr = exp match {
      case Var(name: String) => Var(name.concat("0"))
      case Lambda(arg: Var, body: Expr) => Lambda(Var(arg.name.concat("0")), rename(body))
      case Let(exp1: Var, exp2: Expr, exp3: Expr) => Let(Var(exp1.name.concat("0")),rename(exp2),rename(exp3))
      case Apply1(arg1: Expr, arg2: Expr) => Apply1(rename(arg1),rename(arg2))
      case Apply2(exp1: Expr, exp2: Expr) => Apply2(rename(exp1),rename(exp2))
      case Calc(op: String, arg1: Expr, arg2: Expr) => Calc(op, rename(arg1), rename(arg2))
      case Val(value: Int) => Val(value)
      case Char(value: String) => Char(value)
      case Bool(value: Boolean) => Bool(value)
      case Not(exp1:Expr) => Not(rename(exp1))
      case Or(exp1:Expr, exp2:Expr) => Or(rename(exp1),rename(exp2))
      case And(exp1:Expr, exp2:Expr) => And(rename(exp1),rename(exp2))
      case Equal(exp1:Expr, exp2:Expr) => Equal(rename(exp1),rename(exp2))
      case Lt(exp1:Expr, exp2:Expr) => Lt(rename(exp1),rename(exp2))
      case Lte(exp1:Expr, exp2:Expr) => Lte(rename(exp1),rename(exp2))
      case If(exp1:Expr, exp2:Expr, exp3:Expr) => If(rename(exp1),rename(exp2),rename(exp3))
      case _ => Char("Invalid Input type1!")
    }
    
  //User interface, create an empty env as initial env and do renaming
  def interpE(input: Expr): Any = {
      val result = interp2(rename(input), scala.collection.mutable.Map[String,Any]())
      result match {
        case Closure2(exp: Lambda, env: scala.collection.mutable.Map[String,Any]) => println("Lambda("+exp.arg.name+")."+interp2(exp.body,env))
        case _ => println(result)
    }
  }
    
  //Eval rules for bool expression
  def interpBool(term: Expr, env: scala.collection.mutable.Map[String,Any]): Boolean = term match {
    case Bool(value: Boolean) => value
    case Not(exp1:Expr) => !interpBool(exp1,env)
    case And(exp1:Expr, exp2:Expr) => interpBool(exp1,env) && interpBool(exp2,env)
    case Or(exp1:Expr, exp2:Expr) => interpBool(exp1,env) || interpBool(exp2,env)
    case Equal(exp1:Expr, exp2:Expr) => interpBool(exp1,env) == interpBool(exp2,env)
    case Var(name: String) => {
      if (env.contains(name)) {
        if (env(name) == true)
          true
        else
          false
      }else {
        println("Invalid input type2!")
        false
      }
    }
    case _ => {
      println("Invalid input type2!")
      false
    }
  }
  
  /*Eval rules for non-bool
    look for value in env when deal with variable.
    return value or bool when deal with value.
    return closure containing lambda and env when deal with lambda.
    Apply1: return just "a b" when deal with a b if a and b are just value or can't be reducted, if a is a Lambda then do recursively reduction that apply a to b. 
    recusively deal with applicaion (Apply2) and store Val=Value in env.
    recusively deal with arithmetic and bool expression.*/
  def interp2(term: Expr, env: scala.collection.mutable.Map[String,Any]): Any = term match {
    case Val(value: Int) => value
    case Var(name: String) => {
      if (env.contains(name)) {
        env(name)
      }else {
        name
      }
    }
    case Char(value: String) => value
    case Bool(value: Boolean) => value
    case Lambda(arg: Var, body: Expr) => Closure2(Lambda(arg,body),env)
    case Let(exp1: Var, exp2: Expr, exp3: Expr) => {
      env.put(exp1.name,interp2(exp2,env))
      interp2(exp3,env)
    }
    case Apply1(arg1: Expr,arg2: Expr) => {
      val a1 = interp2(arg1,env)
      val a2 = interp2(arg2,env)      
      a1 match {
        case Lambda(arg: Var, body: Expr) => interp2(Apply2(Lambda(arg,body),arg2),scala.collection.mutable.Map[String,Any]())
        case Apply2(exp1: Expr, exp2: Expr) => interp2(Apply2(exp1,exp2),scala.collection.mutable.Map[String,Any]())
        case _ => a1.toString().concat(" ").concat(a2.toString())
      }
    }
    case Apply2(exp1: Expr, exp2: Expr) => {
      val v1 = interp2(exp1, env)
      var v2 = interp2(exp2, scala.collection.mutable.Map[String,Any]())
      v2 match {
        case Closure2(exp: Lambda, env2: scala.collection.mutable.Map[String,Any]) => {
          v2 = exp
        }
        case _ => v2=v2
      }
      v1 match {
        case Closure2(exp: Lambda, env1: scala.collection.mutable.Map[String,Any]) => {
          env1.put(exp.arg.name,v2)
          interp2(exp.body, env1)          
        }
        case _ => Char("Invalid Input type3!")
      }
    }
    case Calc(op: String, arg1: Expr, arg2: Expr) => {
      val e1 = interp2(arg1,env) 
      val e2 = interp2(arg2,env)
      if (e1.getClass.toString() == "class java.lang.Integer" && e2.getClass.toString() == "class java.lang.Integer") {                
        op match {
          case "+" =>Integer.parseInt(e1.toString())+Integer.parseInt(e2.toString())
          case "-" =>Integer.parseInt(e1.toString())-Integer.parseInt(e2.toString())
          case "*" =>Integer.parseInt(e1.toString())*Integer.parseInt(e2.toString())
          case "/" =>Integer.parseInt(e1.toString())/Integer.parseInt(e2.toString())
          case _ => Char("Invalid Input type4!")
        }
      }
      else {
        op match {
          case "+" => e1.toString().concat("+").concat(e2.toString())
          case "-" => e1.toString().concat("-").concat(e2.toString())
          case "*" => e1.toString().concat("*").concat(e2.toString())
          case "/" => e1.toString().concat("/").concat(e2.toString())
          case _ => Char("Invalid Input type5!")
        }        
      }
    }
    case Not(exp1:Expr) => interpBool(term,env)      
    case And(exp1:Expr, exp2:Expr) => interpBool(term,env)
    case Or(exp1:Expr, exp2:Expr) => interpBool(term,env)
    case Equal(exp1:Expr, exp2:Expr) => interpBool(term,env)
    case Lt(exp1:Expr, exp2:Expr) => Integer.parseInt(interp2(exp1,env).toString()) < Integer.parseInt(interp2(exp2,env).toString())
    case Lte(exp1:Expr, exp2:Expr) => Integer.parseInt(interp2(exp1,env).toString()) <= Integer.parseInt(interp2(exp2,env).toString())
    case If(exp1:Expr, exp2:Expr, exp3:Expr) => {
      if (interp2(exp1,env) == true)
        interp2(exp2,env)
      else
        interp2(exp3,env)
    }
  }
}