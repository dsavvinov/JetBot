import JetBot._
import akka.actor._

import scala.collection.mutable

case class User(id: String, name: String) {
}

case class Token(id: String) {
}

sealed trait Message
case class TextMessage(text: String, destination: User) extends Message
case class TalkStarted() extends Message
case class TalkEnded() extends Message



case class TestMessage(text: String, sender: User)

package object JetBot {
    type HandlerType = List[Action]
    type Action = (TextMessage) => (Talk) => Any
    type HandlersTableType = collection.mutable.Map[State, HandlerType]

    def say(text: String) : (Talk => Any) = {
        (talk : Talk) => talk.say(text)
    }

    def moveTo(state: State) : (Talk => Any) = {
        (talk : Talk) =>  talk.changeState(state)
    }

    implicit def stringToState(stateName: String) : State = {
        State(stateName)
    }
}

class Talk(user: User, StatesToHandlers : collection.mutable.Map[State, HandlerType]) extends Actor {

    var currentState : State = State("TalkStarted")

    def say (text: String): Unit = {
//        BotManager ! Message(text, user)
        Console.println(text)
    }

    def changeState (newState: State) = {
        currentState = newState
    }

    def receive = {
        case msg : TextMessage =>
            StatesToHandlers(currentState).foreach(
                (action : Action) => action(msg)(this)
            )
    }
}


class StateHandlerChanger(statesToHandlers: HandlersTableType, changingState: State) {
    def use (handler: HandlerType) = {
        statesToHandlers(changingState) = handler
    }
}

case class State(stateName: String)

class Bot(token: Token, name: String)(system: ActorSystem) {
    private val usersToTalks: collection.mutable.Map[User, ActorRef] = collection.mutable.Map()
    val statesToHandlers: collection.mutable.Map[State, HandlerType] =
        collection.mutable.Map(
            (State("TalkStarted"), List ((msg : Message) => (talk: Talk) => sys.error("State handler not implemented!"))),
            (State("TalkEnded"), List ( (msg : Message) => (talk: Talk) => sys.error("State handler not implemented!")))
        )

    val botActor = system.actorOf(BotActor.props(), name)


    def addState(state: State) : Unit = {
        statesToHandlers += state -> List((msg : Message) => (talk: Talk) => sys.error("Not implemented"))
    }

    def when(state: State) : StateHandlerChanger = {
        new StateHandlerChanger(statesToHandlers, state)
    }

    object BotActor {
        def props(): Props = Props(new BotActor())
    }

    class BotActor extends Actor {
        def receive = {
            case TestMessage(text, sender) =>
                usersToTalks.get(sender) match {
                    case Some(talk) => talk ! TextMessage(text, sender)
                    case None =>
                        val newTalk = context.actorOf(Props(classOf[Talk], sender, statesToHandlers), "talk-with-%s".format(sender.id))
                        usersToTalks += sender -> newTalk
                        newTalk ! TextMessage("TalkStarted", sender)
                        newTalk ! TextMessage(text, sender)
                }
        }
    }
}


object main extends App {
    test()

    def test() : Any = {
        implicit val system = ActorSystem("TestBot")
        val token = Token("Sometokentodo")
        val myBot = new Bot(token, "bot1")(system)

        myBot addState "Echoing"

        myBot when "TalkStarted" use List(
            (message : TextMessage) => say("Hello, I'll echo everything you write until you say \"Good bye\""),
            (message : TextMessage) => moveTo("Echoing")
        )

//        // works fine, cause it's easy
//        myBot when "Echoing" use List(
//            (message : TextMessage) => say(message.text)
//        )

//        // works incorrect
//        myBot when "Echoing" use List(
//            (message : TextMessage) =>
//                if (message.text == "Goodbye") {
//                    say("Goodbye, that was pleasure to echo with you :) ")
//                    moveTo("TalkEnded")
//                }
//                else {
//                    say(message.text)
//                }
//        )

        // works correct, but has terrible syntax
        myBot when "Echoing" use List (
            (message : TextMessage) => if (message.text == "Goodbye")
                    say("Goodbye, that was pleasure to echo with you :) ")
                else
                    say(message.text),
            (message : TextMessage) =>
                if (message.text == "Goodbye")
                    moveTo("TalkEnded")
                else
                    moveTo("Echoing")

        )

        /* Проблемы, имеющиеся на данный момент:
         * 1. Как красиво предоставлять пользователям доступ к сообщению?
         * 2. Как предоставить возможность исполнять любую последовательность операторов?
         * 3. Как убрать лишние синтаксические нагромождения типа List (...), скобочек и прочего
         *
         * Возможные решения:
         * 1. Предоставить пользователям стого ограниченный набор функций, которые сами будут разруливать все что угодно.
         *    Негибко!
         * 2. Отделить переход между состояниями от вызова сайдэффекта. Cинтаксис типа:
         *      when State moveTo State' yielding SideEffect
         *    Будет велосипед FSM эрланга (и, как следствие, акки)
         * 3. Сказать, чтобы пользователь сам писал PartialFunction[Any, Unit] в receive -- придется выдать ему
         *    набор типовых сообщений, а дальше пусть он сам их разворачивает и че хочет вообще делает.
         *
         * Выглядит пока что так, что велосипед выигрывает (большое преимущество -- позволяем хранить некоторую информацию Data,
         * и при этом абстрагируемся от деталей реализации класса)
         */
        myBot when "TalkEnded" use List(
            (message : TextMessage) => say("Goodbye, that was pleasure to echo with you :) ")
        )

        val sender = User("id123", "Boss")
        while(true) {
            val messageToSend = io.StdIn.readLine()
            myBot.botActor ! TestMessage(messageToSend, sender)
        }
    }
}