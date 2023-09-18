package org.scalacheck.time

import java.time._

import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.time.granularity.instances

object TimeGenProperties extends Properties("Java 8 Generators") {

  // Guards against generating values well outside of expected ranges, as users may run into JDK bugs
  implicit val yearRange: YearRange = YearRange.between(0, 10000)

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(100000)

  property("arbitrary generation creates valid ZonedDateTimes (with no granularity)") = {
    import Arbitrary._
    forAll((_: ZonedDateTime) => passed)
  }

  property("arbitrary generation creates valid LocalDateTimes (with no granularity)") = {
    import Arbitrary._
    forAll((_: LocalDateTime) => passed)
  }

  property("arbitrary generation creates valid LocalDates (with no granularity)") = {
    import Arbitrary._
    forAll((_: LocalDate) => passed)
  }

  property("arbitrary generation creates valid Instants (with no granularity)") = {
    import Arbitrary._
    forAll((_: Instant) => passed)
  }

  val granularitiesAndPredicates: List[(Granularity[ZonedDateTime], ZonedDateTime => Boolean)] = {

    import java.time.temporal.ChronoField._

    // Defines handling the weird scenario where normalizing is impossible due to a sudden timezone switch.
    def timezoneSwitch(dt: ZonedDateTime) = {
      (dt.withHour(0).getHour > 0) ||
      (dt.withMinute(0).getMinute > 0) ||
      (dt.withSecond(0).getSecond > 0) ||
      (dt.withNano(0).getNano > 0)
    }

    def zeroNanos(dt: ZonedDateTime) = timezoneSwitch(dt) || dt.get(NANO_OF_SECOND) == 0

    def zeroSeconds(dt: ZonedDateTime) =
      timezoneSwitch(dt) || (zeroNanos(dt) && dt.get(SECOND_OF_MINUTE) == 0)

    def zeroMinutes(dt: ZonedDateTime) =
      timezoneSwitch(dt) || (zeroSeconds(dt) && dt.get(MINUTE_OF_HOUR) == 0)

    def zeroHours(dt: ZonedDateTime) =
      timezoneSwitch(dt) || (zeroMinutes(dt) && {
        // Very very rarely, some days start at 1am, rather than 12am
        // In this case, check that the minute before is in the day before.
        dt.get(HOUR_OF_DAY) match {
          case 0 => true
          case 1 =>
            val prevMinute = dt.plusMinutes(-1)
            val prevDay = dt.plusDays(-1)

            (prevMinute.get(DAY_OF_YEAR) == prevDay.get(DAY_OF_YEAR)) && (prevMinute
              .get(YEAR) == prevDay.get(YEAR))
          case _ => false
        }
      })

    def firstDay(dt: ZonedDateTime) =
      timezoneSwitch(dt) || (zeroHours(dt) && dt.get(DAY_OF_YEAR) == 1)

    List(
      (instances.seconds, zeroNanos),
      (instances.minutes, zeroSeconds),
      (instances.hours, zeroMinutes),
      (instances.days, zeroHours),
      (instances.years, firstDay)
    )
  }

  val granularitiesAndPredicatesWithDefault: List[
    (Granularity[ZonedDateTime], ZonedDateTime => Boolean)
  ] =
    (Granularity.identity[ZonedDateTime], (_: ZonedDateTime) => true) :: granularitiesAndPredicates

  property("arbitrary generation with a granularity generates appropriate ZonedDateTimes") =
    forAll(Gen.oneOf(granularitiesAndPredicates)) { case (granularity, predicate) =>
      import Arbitrary._

      implicit val generatedGranularity: Granularity[ZonedDateTime] = granularity

      forAll((dt: ZonedDateTime) => predicate(dt) :| s"${granularity.description}: $dt")
    }

  property("arbitrary generation with a granularity generates appropriate LocalDateTimes") =
    forAll(Gen.oneOf(granularitiesAndPredicates)) { case (granularity, predicate) =>
      import Arbitrary._

      implicit val generatedGranularity: Granularity[ZonedDateTime] = granularity

      forAll { (dt: LocalDateTime) =>
        predicate(dt.atZone(ZoneOffset.UTC)) :| s"${granularity.description}: $dt"
      }
    }

  property("arbitrary generation with a granularity generates appropriate LocalDates") =
    forAll(Gen.oneOf(granularitiesAndPredicates)) { case (granularity, predicate) =>
      import Arbitrary._

      implicit val generatedGranularity: Granularity[ZonedDateTime] = granularity

      forAll { (dt: LocalDate) =>
        predicate(dt.atStartOfDay(ZoneOffset.UTC)) :| s"${granularity.description}: $dt"
      }
    }

  property("arbitrary generation with a granularity generates appropriate Instants") =
    forAll(Gen.oneOf(granularitiesAndPredicates)) { case (granularity, predicate) =>
      import Arbitrary._

      implicit val generatedGranularity: Granularity[ZonedDateTime] = granularity

      forAll { (instant: Instant) =>
        predicate(instant.atZone(ZoneOffset.UTC)) :| s"${granularity.description}: $instant"
      }
    }
}
