import JetBot.HandlerType
import akka.actor._

import scala.collection.mutable

case class User(id: String, name: String) {
}

case class Token(id: String) {
}

case class Message(text: String, destination: User)
case class TalkStarted()
case class TalkEnded()
case class ReceivedMessage(text: String)
case class TestMessage(text: String, sender: User)

package object JetBot {
    type HandlerType = (Talk) => Actor.Receive
}


class Talk(user: User, StatesToHandlers : collection.mutable.Map[String, HandlerType]) extends Actor {

    var currentState : String = "TalkStarted"

    def say (text: String): Actor.Receive = {
//        BotManager ! Message(text, user)
        case _ =>
            Console.println(text)
    }

    def changeState (newState: String) = {
        currentState = newState
    }

    def receive = {
        case TalkStarted =>
            StatesToHandlers("TalkStarted")(this)
        case TalkEnded =>
            StatesToHandlers("TalkEnded")(this)
        case msg =>
            StatesToHandlers(currentState)(this)(msg)
    }
}


class StateDescriptionHelper(statesToHandlers: collection.mutable.Map[String, HandlerType],
                             stateToAdd: String) {
    private var nextState : String = ""
    def use(handler: HandlerType) : StateDescriptionHelper = {
        statesToHandlers(stateToAdd) = handler
        this
    }

    def moving(nextState: String) : StateDescriptionHelper = {
        this.nextState = nextState
        this //TODO: think about proper implementation; see line 112 for details
    }
}


class Bot(token: Token, name: String)(system: ActorSystem) {
    private val usersToTalks: collection.mutable.Map[User, ActorRef] = collection.mutable.Map()
    val statesToHandlers: collection.mutable.Map[String, HandlerType] =
        collection.mutable.Map(
            ("TalkStarted", (talk: Talk) => sys.error("State handler not implemented!")),
            ("TalkEnded", (talk: Talk) => sys.error("State handler not implemented!"))
        )

    val botActor = system.actorOf(BotActor.props(), name)


    def say (text: String): HandlerType = {
        (talk: Talk) => talk.say(text)
    }

    def goto (newState: String) : Unit = {
        (talk: Talk) => talk.changeState(newState)
    }

    def addState(stateName: String) : Unit = {
        statesToHandlers += stateName -> ((talk : Talk) => sys.error("Not implemented"))
    }

    def when(stateName: String) : StateDescriptionHelper = {
        new StateDescriptionHelper(statesToHandlers, stateName)
    }

    object BotActor {
        def props(): Props = Props(new BotActor())
    }

    class BotActor extends Actor {
        def receive = {
            case TestMessage(text, sender) =>
                usersToTalks.get(sender) match {
                    case Some(talk) => talk ! ReceivedMessage(text)
                    case None =>
                        val newTalk = context.actorOf(Props(classOf[Talk], sender, statesToHandlers), "talk-with-%s".format(sender.id))
                        usersToTalks += sender -> newTalk
                        newTalk ! TalkStarted()
                        newTalk ! ReceivedMessage(text)
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
        myBot when "TalkStarted" use {
            myBot say "Hello"
        } movingTo
        /* TODO: think about how implement this properly.
         * Possible solutions:
         * - Assume that we can call only one function per message recieved, and move between states only once too.
         *   Then we can just add movingTo method to helper-object (but we have to invoke this move properly in Talk-actor)
         * - Do not make such assumption; then we have to change implementation of handlers, so that handler could chain
         *   many actions (i.e. say something, write to log, and then move to next state). Maybe something like
         *   TransformerHelper class, which will return itself on call of transforming methods, so we could write like
         *   "myBot say "Hello" writeToLog "said hello" moveTo "Echoing"
         *   Note that this implementation restricts clients to use only pre-defined methods
         * - Write some wrapper for custom functions that can invoke them one after one. Think about monads.
         */


        myBot when "TalkEnded" use {
            myBot say "Goodbye"
        }

        val sender = User("id123", "Boss")
        myBot.botActor ! TestMessage("Hello", sender)
    }
}

//class Bot extends Actor {
//    /* Вообще в акторе бота хочется наплодить много дочерних акторов, реализующих абстракцию беседы с
//     * отдельным пользователем. Это выглядит всесторонне удобно - мы получаем бесплатную скалируемость ботов,
//     * не задумываемся о параллельности, естественный персистенс отдельных бесед, и еще бог весть что.
//     * В таком разе по сообщению хочется понимать, в какую беседу его перенаправить. Значит, каждый бот должен
//     * хранить отображение Users -> Talks.
//     *
//     * Была мысль использоваться роутер, но он, видимо, решает немного другие задачи и не совсем подходит сюда, поэтому
//     * решил взять обычный ассоциативный контейнер.
//     */
//    // TODO: используем immutable-map. Тогда приходится использовать var, чтобы менять его. Вроде это норм, но нужно уточнить
//    // TODO: Подумать, как быть, если юзер присутствует в нескольких беседах?
//    // TODO: Можно перписать на mutable-map!
//    var UsersToTalks: Map[String, ActorRef] = Map()
//
//    // TODO: разобраться с props. Что это (конкретней, чем "рецепт для создания актора")? Почему такой синтаксис создания?
//
//    def receive = {
//        case Message(text, userSender) =>
//            // TODO: Что если юзер уже помер (и, как следствие, его нет в мапе), пока шло сообщение от сервера до нас?
//            // TODO: Или, что намного хуже - если у нас ЕЩЕ НЕТ беседы с этим юзером, но сообщение StartTalk уже поехало, но еще не дошло? Но вроде так не бывает
//            UsersToTalks.get(userSender) match {
//                case Some(userTalk) => userTalk ! Message(text, userSender)
//                case None =>
//                    val newUserTalk = context.actorOf(Props[Talk], "talk-with-%s".format(userSender))
//                    UsersToTalks = UsersToTalks + (userSender -> newUserTalk)
//                    newUserTalk ! Message(text, userSender)
//            }
//
//        case Terminated(child) =>
//            val user_name = child.path.name.split("talk-with-", 1)(0) // TODO: ПЕРЕПИСАТЬ ЭТОТ ОТВРАТИТЕЛЬНЫЙ КОСТЫЛЬ!
//            UsersToTalks = UsersToTalks - user_name
//    }
//}
