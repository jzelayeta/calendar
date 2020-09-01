import Calendar._
import Month._
import NonWorkingDateRule.{Monday, Saturday, Sunday, Thursday}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpecLike

class CalendarSpec extends AsyncWordSpecLike with Matchers {

  "Calendar" should {

    "Any Sunday should be non-working day" in {
      val calendar = Calendar.setUpNonWorkingDays(Monday)
      calendar.isWorkingDay(August(31) of 2020) shouldBe false
    }

    "Any Saturday should be non-working day" in {
      val calendar = Calendar.setUpNonWorkingDays(Saturday)
      calendar.isWorkingDay(August(29) of 2020) shouldBe false
    }

    "Any Friday should be working day" in {
      val calendar = Calendar.setUpNonWorkingDays(Sunday)
      calendar.isWorkingDay(August(28) of 2020) shouldBe true
    }

    "Configure multiple days" in {
      val calendar = Calendar.setUpNonWorkingDays(Saturday, Sunday)
      calendar.isWorkingDay(August(8) of 2020) shouldBe false
    }

    "configure a day of month" in {
      val calendar = Calendar.setUpNonWorkingDays(September(25))
      calendar.isWorkingDay(September(25) of 1990) shouldBe false
    }

    "configure day of week and day of months" in {

      val calendar = Calendar.setUpNonWorkingDays(Saturday, Sunday, December(25), July(9))

      calendar.isWorkingDay(July(1) of 1998) shouldBe true
      calendar.isWorkingDay(July(9) of 2000) shouldBe false
    }

    "configure date as non-wowking day" in {
      val calendar = Calendar
        .setUpNonWorkingDays(Saturday, Sunday, December(25), July(9), April(2) of 2012)

      calendar.isWorkingDay(April(2) of 2012) shouldBe false
      calendar.isWorkingDay(April(3) of 2012) shouldBe true
    }

    "ranged day of week" in {
      val calendar = Calendar
        .setUpNonWorkingDays(Thursday to Sunday)

      calendar.isWorkingDay(August(28) of 2020) shouldBe false
      calendar.isWorkingDay(August(26) of 2020) shouldBe true
    }

    "ranged day of Month" in {
      val calendar = Calendar
        .setUpNonWorkingDays(August(1) to August(4))

      calendar.isWorkingDay(August(2) of 1998) shouldBe false
      calendar.isWorkingDay(August(5) of 1998) shouldBe true
      calendar.isWorkingDay(July(31) of 2002) shouldBe true
    }

    "ranged date" in {
      val calendar = Calendar
        .setUpNonWorkingDays(August(1) of 2020 to (August(4) of 2020))

      calendar.isWorkingDay(August(3) of 2020) shouldBe false
      calendar.isWorkingDay(August(3) of 2021) shouldBe true
      calendar.isWorkingDay(August(5) of 2020) shouldBe true
    }

    "ranged different types" in {
      val calendar = Calendar
        .setUpNonWorkingDays(Sunday to August(1))

      calendar.isWorkingDay(November(8) of 2020) shouldBe true
    }

  }

}
