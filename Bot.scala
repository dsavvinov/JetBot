import akka.actor._
import scala.collection.mutable


case class User(id: String, name: String) {
}

case class Token(id: String) {
}

sealed trait Message

case class TextMessage(text: String, destination: User) extends Message {
    def unapply(pattern: String): Unit = {
        pattern == text
    }
}

case class TalkStarted() extends Message

case class TalkEnded() extends Message


case class TestMessage(text: String, sender: User)

package object JetBot {
    type HandlerType = List[Action]
    type Action = (TextMessage) => (Talk) => Any
}

class Talk(user: User, StatesToHandlers: collection.mutable.Map[String, State]) extends Actor {

    var currentState: String = "State1"

    def say(text: String): Unit = {
        //        BotManager ! Message(text, user)
        Console.println(text)
    }

    def moveTo(newState: String) = {
        currentState = newState
    }

    def receive = {
        case msg: TextMessage =>
            StatesToHandlers(currentState).unboundHandler.boundToTalk(this)
            StatesToHandlers(currentState).unboundHandler.handler(msg)
    }
}

trait UnboundHandler {
    var talk: Talk = null

    def handler(msg: TextMessage): Unit

    def boundToTalk(t: Talk) = {
        talk = t
    }

    def say(text: String): Unit = {
        talk.say(text)
    }

    def moveTo(newState: String) = {
        talk.moveTo(newState)
    }
}

object State {
    def apply(stateName : String)(ub : UnboundHandler): State = {
        new State(stateName)(ub)
    }
}
class State(val stateName: String)(val unboundHandler: UnboundHandler) {

}

class Bot(token: Token, name: String)(system: ActorSystem) {
    private val usersToTalks: collection.mutable.Map[User, ActorRef] = collection.mutable.Map()
    val statesList = mutable.Map[String, State]()

    var botActor: ActorRef = null

    def +(newState: State): Bot = {
        statesList += (newState.stateName -> newState)
        this
    }

    object BotActor {
        def props(): Props = Props(new BotActor())
    }

    def launch(): Unit = {
        botActor = system.actorOf(BotActor.props(), name)
    }

    class BotActor extends Actor {
        def receive = {
            case TestMessage(text, sender) =>
                usersToTalks.get(sender) match {
                    case Some(talk) => talk ! TextMessage(text, sender)
                    case None =>
                        val newTalk = context.actorOf(Props(classOf[Talk], sender, statesList), "talk-with-%s".format(sender.id))
                        usersToTalks += sender -> newTalk
                        //                        newTalk ! TextMessage("TalkStarted", sender)
                        newTalk ! TextMessage(text, sender)
                }
        }
    }

}

class Desugarizer {
    val fieldsToInt: collection.mutable.Map[String, String] = collection.mutable.Map()

    def collectDefinitions(text: String): Unit = {
        val pattern = """(.*)storesState(\s)*\[(\w*)](\s)*\((\s)*\"(\s)*(\w*)""".r
        (pattern findAllIn text).matchData.toList foreach {
            case m =>
                val fieldType = m.group(3)
                val fieldName = m.group(7)
                fieldsToInt += (fieldName -> fieldType)
        }
    }

}

class Test {
    def foo[T](name: String, value: T) :Unit = {
        println(name)
    }
}

object main extends App {

    import scala.reflect.runtime.universe._
    val t = new Test()


    println(showRaw(reify[Unit](t.foo[Int]("int_field", 0))))

    def test(): Any = {
        val startState = State("State1")(new UnboundHandler {
            override def handler(msg: TextMessage) = {
                msg.text match {
                    case "hello" => {
                        say("hello!")
                        moveTo("State2")
                    }
                    case other => {
                        say("You should greet me first, human.")
                    }
                }
            }
        })

        val anotherState = State("State2")(new UnboundHandler {
            override def handler(msg: TextMessage) = {
                if (msg.text == "bye") {
                    say("bye")
                    moveTo("State3")
                }
                else
                    say(msg.text)
            }
        })

        val finishState = State("State3")(new UnboundHandler {
            override def handler(msg: TextMessage) = {
                say("I don't want to talk with you anymore")
            }
        })
        val s : String = ""
        val system = ActorSystem("TestBot")
        val myBot = new Bot(Token("123"), "myBot")(system)

        myBot + startState + anotherState + finishState
        myBot.launch()

        val d = new Desugarizer()
        val lines = io.Source.fromFile("""./src/main/resources/test""").mkString
        d.collectDefinitions(lines)
        println(d.fieldsToInt)
    }
}