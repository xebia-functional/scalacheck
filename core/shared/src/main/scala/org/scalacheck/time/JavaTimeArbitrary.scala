/*
 * ScalaCheck
 * Copyright (c) 2007-2021 Rickard Nilsson. All rights reserved.
 * http://www.scalacheck.org
 *
 * This software is released under the terms of the Revised BSD License.
 * There is NO WARRANTY. See the file LICENSE for the full text.
 */

package org.scalacheck.time

import org.scalacheck._

import java.time._

/** [[Arbitrary]] instances for `java.time` types.
  *
  * @note
  *   [[Arbitrary]] instances for `java.time` types which are Java enum types are provided by ScalaCheck's general Java
  *   enum support.
  */
private[scalacheck] trait JavaTimeArbitrary {

  private def genZonedDateTime(maybeZone: Option[ZoneId] = None)(implicit
      granularity: Granularity[ZonedDateTime],
      yearRange: YearRange
  ): Gen[ZonedDateTime] =
    for {
      year <- Gen.choose(yearRange.min, yearRange.max)
      month <- Gen.choose(1, 12)
      maxDaysInMonth = Month.of(month).length(year.isLeap)
      dayOfMonth <- Gen.choose(1, maxDaysInMonth)
      hour <- Gen.choose(0, 23)
      minute <- Gen.choose(0, 59)
      second <- Gen.choose(0, 59)
      nanoOfSecond <- Gen.choose(0, 999999999)
      zoneId <- maybeZone.map(Gen.const).getOrElse(arbZoneId.arbitrary)
    } yield granularity.normalize(
      ZonedDateTime.of(year.getValue, month, dayOfMonth, hour, minute, second, nanoOfSecond, zoneId)
    )

//  private[this] val utcZoneId: ZoneId = ZoneId.of("UTC")

  // Duration

  // Java duration values are conceptually infinite, thus they do not expose
  // Duration.MAX/Duration.MIN values, but in practice they are finite,
  // restricted by their underlying representation a long and an int.

  implicit final lazy val arbJavaDuration: Arbitrary[Duration] = {
    val minJavaDuration = Duration.ofSeconds(Long.MinValue)
    val maxJavaDuration = Duration.ofSeconds(Long.MaxValue, 999999999L)
    Arbitrary(Gen.choose(minJavaDuration, maxJavaDuration))
  }

  // Instant

  implicit def arbInstant(implicit
      granularity: Granularity[ZonedDateTime],
      yearRange: YearRange
  ): Arbitrary[Instant] =
    Arbitrary(genZonedDateTime(Some(ZoneOffset.UTC)).map(_.toInstant))

  // Year

  implicit def arbYear(implicit yearRange: YearRange): Arbitrary[Year] =
    Arbitrary(Gen.choose(yearRange.min, yearRange.max))

  // LocalDate

  implicit def arbLocalDate(implicit
      granularity: Granularity[ZonedDateTime],
      yearRange: YearRange
  ): Arbitrary[LocalDate] =
    Arbitrary(genZonedDateTime(Some(ZoneOffset.UTC)).map(_.toLocalDate))

  // LocalTime

  implicit final lazy val arbLocalTime: Arbitrary[LocalTime] =
    Arbitrary(Gen.choose(LocalTime.MIN, LocalTime.MAX))

  // LocalDateTime

  implicit def arbLocalDateTime(implicit
      granularity: Granularity[ZonedDateTime],
      yearRange: YearRange
  ): Arbitrary[LocalDateTime] =
    Arbitrary(genZonedDateTime(Some(ZoneOffset.UTC)).map(_.toLocalDateTime))

  // MonthDay

  implicit final lazy val arbMonthDay: Arbitrary[MonthDay] =
    Arbitrary(Gen.choose(MonthDay.of(Month.JANUARY, 1), MonthDay.of(Month.DECEMBER, 31)))

  // ZoneOffset

  implicit final lazy val arbZoneOffset: Arbitrary[ZoneOffset] =
    Arbitrary(
      Gen.oneOf(
        Gen.oneOf(ZoneOffset.MAX, ZoneOffset.MIN, ZoneOffset.UTC),
        Gen.choose(ZoneOffset.MAX, ZoneOffset.MIN) // These look flipped, but they are not.
      )
    )

  // ZoneId

  /** ''Technically'' the available zone ids can change at runtime, so we store an immutable snapshot in time here. We
    * avoid going through the scala/java collection converters to avoid having to deal with the scala 2.13 changes and
    * adding a dependency on the collection compatibility library.
    */
  private final lazy val availableZoneIds: Set[ZoneId] =
    ZoneId.getAvailableZoneIds.toArray(Array.empty[String]).toSet.map((value: String) => ZoneId.of(value))

  // ZoneIds by themselves do not describe an offset from UTC (ZoneOffset
  // does), so there isn't a meaningful way to define a choose as they can not
  // be reasonably ordered.

  implicit final lazy val arbZoneId: Arbitrary[ZoneId] =
    Arbitrary(Gen.oneOf(Gen.oneOf(availableZoneIds), arbZoneOffset.arbitrary))

  // OffsetTime

  implicit final lazy val arbOffsetTime: Arbitrary[OffsetTime] =
    Arbitrary(Gen.choose(OffsetTime.MIN, OffsetTime.MAX))

  // OffsetDateTime

  implicit final lazy val arbOffsetDateTime: Arbitrary[OffsetDateTime] =
    Arbitrary(Gen.choose(OffsetDateTime.MIN, OffsetDateTime.MAX))

  // Period

  implicit final lazy val arbPeriod: Arbitrary[Period] =
    Arbitrary(
      for {
        years <- Arbitrary.arbitrary[Int]
        months <- Arbitrary.arbitrary[Int]
        days <- Arbitrary.arbitrary[Int]
      } yield Period.of(years, months, days))

  // YearMonth

  implicit final lazy val arbYearMonth: Arbitrary[YearMonth] =
    Arbitrary(Gen.choose(YearMonth.of(Year.MIN_VALUE, Month.JANUARY), YearMonth.of(Year.MAX_VALUE, Month.DECEMBER)))

  // ZonedDateTime

  implicit def arbZonedDateTime(implicit
      granularity: Granularity[ZonedDateTime],
      yearRange: YearRange
  ): Arbitrary[ZonedDateTime] = Arbitrary(genZonedDateTime())
}
