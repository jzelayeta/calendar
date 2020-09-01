import java.time.LocalDate

import Month.Month

sealed trait CalendarDate

class Calendar {

  private var rules: List[NonWorkingRule] = List.empty

  def setupNonWorkingDay(day: NonWorkingRule): Calendar = {
    rules = rules :+ day
    this
  }

  def isWorkingDay(localDate: LocalDate) = rules.map(_.apply(localDate)).forall(identity)
}

object Calendar {

  implicit class IntToCalendarDate(day: Int) {
    def of(month: Month): DayOfMonthRule = DayOfMonthRule(day, month)
  }

  implicit class MonthToDayOfMonth(month: Month) extends (Int => DayOfMonthRule) {
    override def apply(v1: Int): DayOfMonthRule = DayOfMonthRule(v1, month)
  }

  def from(from: NonWorkingDateRule): RangedRule = RangedRule(from, null)


  implicit def toLocalDate(date: DateRule):LocalDate  = LocalDate.of(date.year, date.month.monthValue(), date.day)
  implicit def toNonWorkingRule(dayOfWeek: DayOfWeekRule): NonWorkingRule  = NonWorkingRule(dayOfWeek)
  implicit def toNonWorkingRule(dayOfMonth: DayOfMonthRule): NonWorkingRule  = NonWorkingRule(dayOfMonth)
  implicit def toNonWorkingRule(date: DateRule): NonWorkingRule  = NonWorkingRule(date)
  implicit def toNonWorkingRule(range: RangedRule): NonWorkingRule  = NonWorkingRule(range)

  def setUpNonWorkingDays(nonWorkingDays: NonWorkingRule*): Calendar =
    nonWorkingDays.foldLeft(new Calendar)(_.setupNonWorkingDay(_))
}