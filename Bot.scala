import akka.actor._

/* Используем sealed trait как своего рода аналог enum
 * (более безопасный паттерн-матчинг - будем получать предупреждения
 * об inexhaustive-match
 *
 * Так написано где-то в интернетах.
 */
sealed trait AbstractMessage
case class Message(text: String, userSender: String) extends AbstractMessage
case class StartTalk(user: String) extends AbstractMessage

sealed trait TalkState
case object Uninitialized extends TalkState
case object Greetings extends TalkState
case object Echoing extends TalkState
case object Terminating extends TalkState

sealed trait TalkData
case object SomeData extends TalkData
case class Interlocutor(userID : String) extends TalkData
case class MessageFromUser(text: String) extends TalkData

class Talk extends FSM[TalkState, TalkData] {
    startWith(Uninitialized, SomeData)

    when(Uninitialized) {
        case Event(Message(text, userSender), SomeData) =>
            goto (Echoing) using Interlocutor(userSender)
    }

    onTransition{
        case Uninitialized -> Echoing =>
            nextStateData match {
                case Interlocutor(username) =>
                    println("Hello, Mr. %s! \n\n".format(username))
            }
        case Echoing -> Echoing =>
            nextStateData match {
                case MessageFromUser(text) =>
                     println("Echoing: \n %s \n\n".format(text))
            }
        case Echoing -> Terminating =>
            println ("Goodbye! Thas was pleasant to echo with you :)\n\n")
    }

    when (Echoing) {
        case Event(Message(text, userSender), _) =>
            if (text == "Good bye, Bot") {
                println("Good bye u2 <3 \n")
                stop()
            }
            else {
                println("Echoing: \n %s \n\n".format(text))
                stay using MessageFromUser(text)
            }
    }
}

class Bot extends Actor {
    /* Вообще в акторе бота хочется наплодить много дочерних акторов, реализующих абстракцию беседы с
     * отдельным пользователем. Это выглядит всесторонне удобно - мы получаем бесплатную скалируемость ботов,
     * не задумываемся о параллельности, естественный персистенс отдельных бесед, и еще бог весть что.
     * В таком разе по сообщению хочется понимать, в какую беседу его перенаправить. Значит, каждый бот должен
     * хранить отображение Users -> Talks.
     *
     * Была мысль использоваться роутер, но он, видимо, решает немного другие задачи и не совсем подходит сюда, поэтому
     * решил взять обычный ассоциативный контейнер.
     */
    // TODO: Можно перписать на mutable-map!
    var UsersToTalks: Map[String, ActorRef] = Map()

    // TODO: разобраться с props. Что это (конкретней, чем "рецепт для создания актора")? Почему такой синтаксис создания?

    def receive = {
        case Message(text, userSender) =>
            // TODO: Что если юзер уже помер (и, как следствие, его нет в мапе), пока шло сообщение от сервера до нас?
            // TODO: Или, что намного хуже - если у нас ЕЩЕ НЕТ беседы с этим юзером, но сообщение StartTalk уже поехало, но еще не дошло? Но вроде так не бывает
            UsersToTalks.get(userSender) match {
                case Some(userTalk) => userTalk ! Message(text, userSender)
                case None =>
                    val newUserTalk = context.actorOf(Props[Talk], "talk-with-%s".format(userSender))
                    UsersToTalks = UsersToTalks + (userSender -> newUserTalk)
                    newUserTalk ! Message(text, userSender)
            }

        case Terminated(child) =>
            val user_name = child.path.name.split("talk-with-", 1)(0) // TODO: ПЕРЕПИСАТЬ ЭТОТ ОТВРАТИТЕЛЬНЫЙ КОСТЫЛЬ!
            UsersToTalks = UsersToTalks - user_name
    }
}

object main extends App {
    test()

    def test() = {
        val system = ActorSystem("TestBot")
        val bot    = system.actorOf(Props[Bot], "bot1")
        var text = ""
        while(true) {
            text = scala.io.StdIn.readLine()
            bot ! Message(text, "god-of-the-actor-universe")
        }
    }
}