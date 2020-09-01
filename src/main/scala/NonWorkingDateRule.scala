import java.time.{LocalDate, MonthDay}

import Month.Month

sealed trait NonWorkingDateRule {
  def isWorkingDay(date: LocalDate): Boolean
}

case class DayOfWeekRule(value: Int) extends NonWorkingDateRule {
  def isAfter(dayOfWeek: DayOfWeekRule) = value > dayOfWeek.value
  def isBefore(dayOfWeek: DayOfWeekRule) = ! isAfter(dayOfWeek)
  def to(dayOfWeek: DayOfWeekRule) = RangedRule(this, dayOfWeek)
  override def isWorkingDay(date: LocalDate): Boolean = ! (date.getDayOfWeek.getValue == value)
}

case class DayOfMonthRule(day: Int, month: Month) extends NonWorkingDateRule {
  def of(year: Int): DateRule = DateRule(day, month, year)
  def to(dayOfMonth: DayOfMonthRule): RangedRule = RangedRule(this, dayOfMonth)
  def isAfter(localDate: LocalDate): Boolean = MonthDay.of(month.monthValue(), day).isAfter(MonthDay.of(localDate.getMonth, localDate.getDayOfMonth))
  def isBefore(localDate: LocalDate): Boolean = ! isAfter(localDate)
  override def isWorkingDay(date: LocalDate): Boolean = !(date.getDayOfMonth == day && date.getMonthValue == month.monthValue())
}

case class DateRule(day: Int, month: Month, year: Int) extends NonWorkingDateRule {
  def isAfter(localDate: LocalDate): Boolean = LocalDate.of(year, month.monthValue(), day).isAfter(localDate)
  def isBefore(localDate: LocalDate): Boolean = ! isAfter(localDate)
  override def isWorkingDay(date: LocalDate): Boolean = ! LocalDate.of(year, month.monthValue(), day).equals(date)
}

case class RangedRule(from: NonWorkingDateRule, to: NonWorkingDateRule) extends NonWorkingDateRule {
  def to(dayOfMonth: DayOfMonthRule): RangedRule = RangedRule(this, dayOfMonth)
  override def isWorkingDay(date: LocalDate): Boolean = (from, to) match {
    case (dayOne:DayOfWeekRule, dayTwo: DayOfWeekRule) =>
      date.getDayOfWeek.getValue <= dayOne.value || date.getDayOfWeek.getValue >= dayTwo.value
    case (dayOfMonthRuleInf: DayOfMonthRule, dayOfMonthRuleSup: DayOfMonthRule) =>
      !(dayOfMonthRuleInf.isBefore(date) && dayOfMonthRuleSup.isAfter(date))
    case (dateRuleInf: DateRule, dateRuleSup: DateRule) =>
      !(dateRuleInf.isBefore(date) && dateRuleSup.isAfter(date))
    case _  => throw new Exception(s"Ranges must be from same type. ${from.getClass} is different from ${to.getClass}")
  }
}


case class NonWorkingRule(nonWorkingDateRule: NonWorkingDateRule) extends (LocalDate => Boolean) {
  override def apply(localDate: LocalDate): Boolean = nonWorkingDateRule.isWorkingDay(localDate)
  def to(to: NonWorkingRule) = RangedRule(nonWorkingDateRule, to.nonWorkingDateRule)
}

object NonWorkingDateRule {
  val Monday = NonWorkingRule(DayOfWeekRule(1))
  val Thursday = NonWorkingRule(DayOfWeekRule(4))
  val Saturday = NonWorkingRule(DayOfWeekRule(6))
  val Sunday = NonWorkingRule(DayOfWeekRule(7))
}


object Month extends Enumeration {
  case class Month(firstDay: Int, lastDay: Int, number: Int) {
    def monthValue() = number
  }
  val January = Month(1, 31, 1)
  val February = Month(1, 28, 2)
  val March = Month(1, 31, 3)
  val April = Month(1, 30, 4)
  val May = Month(1, 31, 5)
  val June = Month(1, 30, 6)
  val July = Month(1, 31, 7)
  val August = Month(1, 31, 8)
  val September = Month(1, 30, 9)
  val October = Month(1, 31, 10)
  val November = Month(1, 30, 11)
  val December = Month(1, 31, 12)
  val August2 = DayOfMonthRule.curried.apply(_).apply(August)
}
