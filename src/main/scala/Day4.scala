import scala.io.Source

object Day4 extends App {
  val events = Source.fromResource("input-day4.txt").getLines().toList.map(parseLine).sortBy(x => x.timestamp.time)

  case class TimeStamp(time: String) {
    val minute: Int = Integer.parseInt(time.takeRight(2))
  }

  sealed abstract class Event(val timestamp: TimeStamp)
  case class WakeUpEvent(override val timestamp: TimeStamp) extends Event(timestamp)
  case class FallAsleepEvent(override val timestamp: TimeStamp) extends Event(timestamp)
  case class BeginShiftEvent(override val timestamp: TimeStamp, guard: String) extends Event(timestamp)

  def parseLine(line: String): Event = {
    val timeStamp = TimeStamp(line.slice(1,17))
    val event = line.drop(19)

    event match {
      case "wakes up" => WakeUpEvent(timeStamp)
      case "falls asleep" => FallAsleepEvent(timeStamp)
      case _ => BeginShiftEvent(timeStamp, line.dropWhile(!_.equals('#')).drop(1).takeWhile(!_.equals(' ')))
    }
  }

  def eventsPerGuard(events: List[Event], currentGuard: String, acc: Map[String, List[Event]]): Map[String, List[Event]] = {
    if (events.isEmpty) {
      acc
    } else {
      val currentGuardEvents = acc.getOrElse(currentGuard, List.empty)
      events.head match {
        case BeginShiftEvent(_, guard) =>
          eventsPerGuard(events.tail, guard, acc)
        case event =>
          val newAcc = acc + (currentGuard -> (currentGuardEvents :+ event))
          eventsPerGuard(events.tail, currentGuard, newAcc)
      }
    }
  }

  def minutesAsleep(events: List[Event]): Int = {
    events.grouped(2).map{ case List(start, end) => end.timestamp.minute - start.timestamp.minute}.sum
  }

  case class MostPopularMinute(minute: Int, occurrences: Int)
  def mostPopularMinute(events: List[Event]): MostPopularMinute = {
    val tuple = events
      .grouped(2)
      .toList
      .flatMap{case List(start, end) => (start.timestamp.minute until end.timestamp.minute).toList}
      .groupBy(identity)
      .values
      .map(minute => minute.head -> minute.length)
      .maxBy(_._2)

    MostPopularMinute.tupled(tuple)
  }

  val eventsPerGuardMap = eventsPerGuard(events, "", Map.empty)
  val minutesPerGuard = eventsPerGuardMap.mapValues(minutesAsleep)

  // Part1
  val (guardMostAsleep, _) = minutesPerGuard.maxBy {case (_, minutes) => minutes}
  val mostPopularMinuteOfGuard = mostPopularMinute(eventsPerGuardMap(guardMostAsleep))
  println(Integer.parseInt(guardMostAsleep) * mostPopularMinuteOfGuard.minute)

  // Part2
  val (guard2, mostPopularMinuteOfGuard2) = eventsPerGuardMap.mapValues(mostPopularMinute).maxBy(_._2.occurrences)
  println(Integer.parseInt(guard2) * mostPopularMinuteOfGuard2.minute)
}
