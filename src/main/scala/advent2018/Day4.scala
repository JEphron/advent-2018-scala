package advent2018

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDateTime}

import util.Utils

object Day4 {
  // https://adventofcode.com/2018/day/4

  def part1(input: String): Unit = {
    // Strategy 1: Find the guard that has the most minutes asleep.
    // What minute does that guard spend asleep the most?
    val (worstGuardId, sleepEvents) = makeSleepMinutes(input)
      .groupBy(_.guardId)
      .maxBy({ case (_, group) => group.length })

    val (mostSleptMinute, _) = sleepEvents.groupBy(_.minute)
      .maxBy({ case (_, group) => group.length })
    println("worst guard ID: ", worstGuardId)
    println("most slept minute: ", mostSleptMinute)
    println("final answer:", worstGuardId * mostSleptMinute)
  }

  def part2(input: String): Unit = {
    // Strategy 2: Of all guards, which guard
    // is most frequently asleep on the same minute?
    val (minute, (guardId, count)) = makeSleepMinutes(input)
      .groupBy(_.minute)
      .map({ case (key, values) =>
        val newValues = Utils.countBy[GuardSleepMinute, Int](_.guardId)(values)
        (key, newValues.maxBy(_._2))
      }).maxBy({ case (minute, (guardId, count)) => count })
    println(s"guard $guardId was found asleep a total of $count times on minute $minute")
    println("final answer:", guardId * minute)
  }

  private def makeSleepMinutes(input: String): Seq[GuardSleepMinute] = {
    val linePattern = "\\[(.+)\\] (.+)".r
    val guardBeginsShiftPattern = "Guard #(\\d+).*".r

    case class SleepEvent(sleepStartTime: LocalDateTime,
                          duration: Duration, guardId: Int)

    case class Accumulator(activeGuardId: Int = -1,
                           latestSleepStart: LocalDateTime = null,
                           sleepEvents: List[SleepEvent] = List())
    input.lines
      .map({ case linePattern(dateString, rest) => (parseDateTime(dateString), rest) })
      .toStream
      .sortWith({ case ((time1, _), (time2, _)) => time1.compareTo(time2) < 0 })
      .foldLeft(Accumulator())({
        case (acc, (_, guardBeginsShiftPattern(guardId))) =>
          acc.copy(activeGuardId = guardId.toInt)
        case (acc, (currentTime, "falls asleep")) =>
          acc.copy(latestSleepStart = currentTime)
        case (acc, (currentTime, "wakes up")) =>
          val event = SleepEvent(acc.latestSleepStart,
            Duration.between(acc.latestSleepStart, currentTime),
            acc.activeGuardId)
          acc.copy(sleepEvents = acc.sleepEvents :+ event)
      }).sleepEvents
      .flatMap({
        case SleepEvent(sleepStartTime, duration, guardId) =>
          for (i <- 0L until duration.toMinutes)
            yield GuardSleepMinute(guardId, sleepStartTime.plusMinutes(i).getMinute)
      })
  }

  val dateTimeFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def parseDateTime(str: String) = LocalDateTime.parse(str, dateTimeFormat)

  case class GuardSleepMinute(guardId: Int, minute: Int)

  def main(args: Array[String]): Unit = {
    val input = Utils.getInput(2018, 4)
    part1(input)
    part2(input)
  }
}
