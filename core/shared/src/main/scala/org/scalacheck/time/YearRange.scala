package org.scalacheck.time

import java.time.Year

trait YearRange {
  val min: Year
  val max: Year
}

object YearRange {

  /**
   * The default range of years. Set to a safe subset shared by both `java.time` and
   * `org.joda.time`.
   */
  implicit val default: YearRange = YearRange.between(Year.MIN_VALUE, Year.MAX_VALUE)

  /** Defines a year range between 1970 and the max year you specify. */
  def epochTo(year: Int): YearRange = YearRange.between(1970, year)

  /** Defines a year range between your own defined min and max years. */
  def between(minYear: Int, maxYear: Int): YearRange = new YearRange {
    override val min: Year = Year.of(minYear)
    override val max: Year = Year.of(maxYear)
  }
}