import TwilioEvaluator.Utils
import com.google.common.collect.{ImmutableList, ImmutableMap}
import ognl.{Ognl, OgnlContext}
import org.mvel2.MVEL

class TwilioEvaluator {
  def buildEvaluationStringMVEL(expression: String): String = {
    var expr = expression.replaceAll("\\sOR\\s", " || ")
      .replaceAll("\\sAND\\s", " && ")

    "([\\w.]+)\\s+HAS\\s+([\"?\\w.-_]*)".r.findAllMatchIn(expr).toList.foreach(m => {
      expr = expr.replace(m.toString(), s"Utils.contains(${m.group(1).split("\\.").mkString(".?")}, ${m.group(2)})")
    })

    "([\\w.]+)\\s+CONTAINS\\s+([\"?\\w.-_]*)".r.findAllMatchIn(expr).toList.foreach(m => {
      expr = expr.replace(m.toString(), s"Utils.contains(${m.group(1).split("\\.").mkString(".?")}, ${m.group(2)})")
    })

    "([\\w.]+)\\s+NOT\\s+IN\\s+\\[([\"?\\w.-_,\\s]*)]".r.findAllMatchIn(expr).foreach(m => {
      expr = expr.replace(m.toString(), s"Utils.notIn(${m.group(1).split("\\.").mkString(".?")}, [${m.group(2)}])")
    })

    "([\\w.]+)\\s+IN\\s+\\[([\"?\\w.-_,\\s]*)]".r.findAllMatchIn(expr).foreach(m => {
      expr = expr.replace(m.toString(), s"Utils.in(${m.group(1).split("\\.").mkString(".?")}, [${m.group(2)}])")
    })
    expr
  }

  def parseExpressionMVEL(expression: String, context: java.util.Map[String, _ <: Object]): Boolean = {
    val finalContext = new java.util.HashMap[String, Object]()
    finalContext.put("Utils", new Utils)
    finalContext.putAll(context)
    MVEL.executeExpression(MVEL.compileExpression(expression), finalContext, classOf[Object]).asInstanceOf[Boolean]
  }

  def buildEvaluationStringOGNL(expression: String): String = {
    expression.replaceAll("\\sOR\\s", " || ").replaceAll("\\sAND\\s", " && ")
      .replaceAll("\\s+NOT\\s+IN\\s+\\[([\"?\\w.-_,\\s]*)]", " not in {$1}")
      .replaceAll("\\s+IN\\s+\\[([\"?\\w.-_,\\s]*)]", " in {$1}")
      .replaceAll("\\s+HAS\\s+([\"?\\w.-_]*)", ".contains($1)")
      .replaceAll("\\s+CONTAINS\\s+([\"?\\w.-_]*)", ".contains($1)")
  }

  def parseExpressionOGNL(expression: String, context: java.util.Map[String, _ <: Object]): Boolean = {
    Ognl.getValue(expression, new OgnlContext(), context, classOf[Boolean]).asInstanceOf[Boolean]
  }
}


object TwilioEvaluator extends App {

  class Utils {
    def in(item: String, array: java.util.List[_ <: Object]): Boolean = {
      array.contains(item)
    }

    def notIn(item: String, array: java.util.List[_ <: Object]): Boolean = {
      !in(item, array)
    }

    def contains(item: String, secondItem: String): Boolean = {
      Option(item).exists(i => i.contains(secondItem))
    }
  }

  private val taskQueueExpression = "(routing.phone CONTAINS \"12\" OR routing.channels HAS \"Messaging\") " +
    "AND routing.skills HAS \"Skill1\" AND routing.language IN [\"en\", \"nl\"] " +
    "OR (routing.language NOT IN [\"FR\"] AND routing.phone == '123')"

  val workerWithoutLanguage = ImmutableMap.of("routing",
    ImmutableMap.of("channels", ImmutableList.of("Messaging", "Voice"), "skills", ImmutableList.of("Skill1"),
      "phone", "123"))
  val workerWithPhone = ImmutableMap.of("routing",
    ImmutableMap.of("channels", ImmutableList.of("Messaging", "Voice"), "skills", ImmutableList.of("Skill1"),
      "phone", "123"))
  val workerWithPhoneAndLanguage = ImmutableMap.of("routing",
    ImmutableMap.of("channels", ImmutableList.of("Chat", "Voice"), "skills", ImmutableList.of("Skill1"),
      "phone", "Chatting", "language", "nl"))
  val workerWithNLLanguage = ImmutableMap.of("routing",
    ImmutableMap.of("channels", ImmutableList.of("Messaging", "Voice"), "skills", ImmutableList.of("Skill1"),
      "phone", "123",
      "language", "nl"))
  val workerWithFRLanguage = ImmutableMap.of("routing",
    ImmutableMap.of("channels", ImmutableList.of("Messaging", "Voice"), "skills", ImmutableList.of("Skill1"),
      "phone", "123",
      "language", "FR"))

  val workers = Seq(workerWithPhone, workerWithoutLanguage, workerWithPhoneAndLanguage, workerWithNLLanguage, workerWithFRLanguage)

  val evaluator = new TwilioEvaluator

  // MVEL
  val expressionMVEL: String = evaluator.buildEvaluationStringMVEL(taskQueueExpression)
  println(expressionMVEL)
  //  println(evaluator.parseExpressionMVEL(expressionMVEL, workerWithPhoneAndLanguage))
  //  println(evaluator.parseExpressionMVEL(expressionMVEL, workerWithPhone))
  //  println(evaluator.parseExpressionMVEL(expressionMVEL, workerWithoutLanguage))
  workers.foreach(w => println(evaluator.parseExpressionMVEL(expressionMVEL, w)))

  // OGNL
  //  val expressionOGNL = evaluator.buildEvaluationStringOGNL(taskQueueExpression)
  //  println(expressionOGNL)
  //  evaluator.parseExpressionOGNL(expressionOGNL, workerWithoutLanguage)
  //  evaluator.parseExpressionOGNL(expressionOGNL, customMap)
  //  workers.foreach(w => println(evaluator.parseExpressionOGNL(expressionOGNL, w)))
}
