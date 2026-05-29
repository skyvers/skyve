package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateTime;

import modules.test.AbstractSkyveTest;

class JobScheduleDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void newInstanceCreatesJobSchedule() {
		JobSchedule bean = JobSchedule.newInstance();
		assertNotNull(bean);
		assertEquals("admin", bean.getBizModule());
		assertEquals("JobSchedule", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void jobNameSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setJobName("BackupJob");
		assertEquals("BackupJob", bean.getJobName());
	}

	@Test
	@SuppressWarnings("static-method")
	void cronExpressionSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setCronExpression("0 0 * * * ?");
		assertEquals("0 0 * * * ?", bean.getCronExpression());
	}

	@Test
	@SuppressWarnings("static-method")
	void startTimeSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		DateTime dt = new DateTime();
		bean.setStartTime(dt);
		assertEquals(dt, bean.getStartTime());
	}

	@Test
	@SuppressWarnings("static-method")
	void endTimeSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		DateTime dt = new DateTime();
		bean.setEndTime(dt);
		assertEquals(dt, bean.getEndTime());
	}

	@Test
	@SuppressWarnings("static-method")
	void jobScheduledImmediatelySetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setJobScheduledImmediately(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getJobScheduledImmediately());
	}

	@Test
	@SuppressWarnings("static-method")
	void disabledSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setDisabled(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getDisabled());
	}

	@Test
	@SuppressWarnings("static-method")
	void minuteFieldsSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setAllMinutes("*");
		bean.setMinute0(Boolean.TRUE);
		bean.setMinute15(Boolean.TRUE);
		bean.setMinute30(Boolean.TRUE);
		bean.setMinute45(Boolean.TRUE);
		bean.setMinute59(Boolean.TRUE);
		assertEquals("*", bean.getAllMinutes());
		assertEquals(Boolean.TRUE, bean.getMinute0());
		assertEquals(Boolean.TRUE, bean.getMinute15());
		assertEquals(Boolean.TRUE, bean.getMinute30());
		assertEquals(Boolean.TRUE, bean.getMinute45());
		assertEquals(Boolean.TRUE, bean.getMinute59());
	}

	@Test
	@SuppressWarnings("static-method")
	void allMinuteFieldsSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setMinute1(Boolean.TRUE);
		bean.setMinute2(Boolean.TRUE);
		bean.setMinute3(Boolean.TRUE);
		bean.setMinute4(Boolean.TRUE);
		bean.setMinute5(Boolean.TRUE);
		bean.setMinute6(Boolean.TRUE);
		bean.setMinute7(Boolean.TRUE);
		bean.setMinute8(Boolean.TRUE);
		bean.setMinute9(Boolean.TRUE);
		bean.setMinute10(Boolean.TRUE);
		bean.setMinute11(Boolean.TRUE);
		bean.setMinute12(Boolean.TRUE);
		bean.setMinute13(Boolean.TRUE);
		bean.setMinute14(Boolean.TRUE);
		bean.setMinute16(Boolean.TRUE);
		bean.setMinute17(Boolean.TRUE);
		bean.setMinute18(Boolean.TRUE);
		bean.setMinute19(Boolean.TRUE);
		bean.setMinute20(Boolean.TRUE);
		bean.setMinute21(Boolean.TRUE);
		bean.setMinute22(Boolean.TRUE);
		bean.setMinute23(Boolean.TRUE);
		bean.setMinute24(Boolean.TRUE);
		bean.setMinute25(Boolean.TRUE);
		bean.setMinute26(Boolean.TRUE);
		bean.setMinute27(Boolean.TRUE);
		bean.setMinute28(Boolean.TRUE);
		bean.setMinute29(Boolean.TRUE);
		bean.setMinute31(Boolean.TRUE);
		bean.setMinute32(Boolean.TRUE);
		bean.setMinute33(Boolean.TRUE);
		bean.setMinute34(Boolean.TRUE);
		bean.setMinute35(Boolean.TRUE);
		bean.setMinute36(Boolean.TRUE);
		bean.setMinute37(Boolean.TRUE);
		bean.setMinute38(Boolean.TRUE);
		bean.setMinute39(Boolean.TRUE);
		bean.setMinute40(Boolean.TRUE);
		bean.setMinute41(Boolean.TRUE);
		bean.setMinute42(Boolean.TRUE);
		bean.setMinute43(Boolean.TRUE);
		bean.setMinute44(Boolean.TRUE);
		bean.setMinute46(Boolean.TRUE);
		bean.setMinute47(Boolean.TRUE);
		bean.setMinute48(Boolean.TRUE);
		bean.setMinute49(Boolean.TRUE);
		bean.setMinute50(Boolean.TRUE);
		bean.setMinute51(Boolean.TRUE);
		bean.setMinute52(Boolean.TRUE);
		bean.setMinute53(Boolean.TRUE);
		bean.setMinute54(Boolean.TRUE);
		bean.setMinute55(Boolean.TRUE);
		bean.setMinute56(Boolean.TRUE);
		bean.setMinute57(Boolean.TRUE);
		bean.setMinute58(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getMinute1());
		assertEquals(Boolean.TRUE, bean.getMinute58());
	}

	@Test
	@SuppressWarnings("static-method")
	void hourFieldsSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setAllHours("*");
		bean.setHour0(Boolean.TRUE);
		bean.setHour6(Boolean.TRUE);
		bean.setHour12(Boolean.TRUE);
		bean.setHour18(Boolean.TRUE);
		bean.setHour23(Boolean.TRUE);
		assertEquals("*", bean.getAllHours());
		assertEquals(Boolean.TRUE, bean.getHour0());
		assertEquals(Boolean.TRUE, bean.getHour12());
		assertEquals(Boolean.TRUE, bean.getHour23());
	}

	@Test
	@SuppressWarnings("static-method")
	void allHourFieldsSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setHour1(Boolean.TRUE);
		bean.setHour2(Boolean.TRUE);
		bean.setHour3(Boolean.TRUE);
		bean.setHour4(Boolean.TRUE);
		bean.setHour5(Boolean.TRUE);
		bean.setHour7(Boolean.TRUE);
		bean.setHour8(Boolean.TRUE);
		bean.setHour9(Boolean.TRUE);
		bean.setHour10(Boolean.TRUE);
		bean.setHour11(Boolean.TRUE);
		bean.setHour13(Boolean.TRUE);
		bean.setHour14(Boolean.TRUE);
		bean.setHour15(Boolean.TRUE);
		bean.setHour16(Boolean.TRUE);
		bean.setHour17(Boolean.TRUE);
		bean.setHour19(Boolean.TRUE);
		bean.setHour20(Boolean.TRUE);
		bean.setHour21(Boolean.TRUE);
		bean.setHour22(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getHour1());
		assertEquals(Boolean.TRUE, bean.getHour22());
	}

	@Test
	@SuppressWarnings("static-method")
	void dayFieldsSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setAllDays("*");
		bean.setDay1(Boolean.TRUE);
		bean.setDay15(Boolean.TRUE);
		bean.setDay31(Boolean.TRUE);
		assertEquals("*", bean.getAllDays());
		assertEquals(Boolean.TRUE, bean.getDay1());
		assertEquals(Boolean.TRUE, bean.getDay15());
		assertEquals(Boolean.TRUE, bean.getDay31());
	}

	@Test
	@SuppressWarnings("static-method")
	void allDayFieldsSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setDay2(Boolean.TRUE);
		bean.setDay3(Boolean.TRUE);
		bean.setDay4(Boolean.TRUE);
		bean.setDay5(Boolean.TRUE);
		bean.setDay6(Boolean.TRUE);
		bean.setDay7(Boolean.TRUE);
		bean.setDay8(Boolean.TRUE);
		bean.setDay9(Boolean.TRUE);
		bean.setDay10(Boolean.TRUE);
		bean.setDay11(Boolean.TRUE);
		bean.setDay12(Boolean.TRUE);
		bean.setDay13(Boolean.TRUE);
		bean.setDay14(Boolean.TRUE);
		bean.setDay16(Boolean.TRUE);
		bean.setDay17(Boolean.TRUE);
		bean.setDay18(Boolean.TRUE);
		bean.setDay19(Boolean.TRUE);
		bean.setDay20(Boolean.TRUE);
		bean.setDay21(Boolean.TRUE);
		bean.setDay22(Boolean.TRUE);
		bean.setDay23(Boolean.TRUE);
		bean.setDay24(Boolean.TRUE);
		bean.setDay25(Boolean.TRUE);
		bean.setDay26(Boolean.TRUE);
		bean.setDay27(Boolean.TRUE);
		bean.setDay28(Boolean.TRUE);
		bean.setDay29(Boolean.TRUE);
		bean.setDay30(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getDay2());
		assertEquals(Boolean.TRUE, bean.getDay30());
	}

	@Test
	@SuppressWarnings("static-method")
	void monthFieldsSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setAllMonths("*");
		bean.setMonth1(Boolean.TRUE);
		bean.setMonth6(Boolean.TRUE);
		bean.setMonth12(Boolean.TRUE);
		assertEquals("*", bean.getAllMonths());
		assertEquals(Boolean.TRUE, bean.getMonth1());
		assertEquals(Boolean.TRUE, bean.getMonth6());
		assertEquals(Boolean.TRUE, bean.getMonth12());
	}

	@Test
	@SuppressWarnings("static-method")
	void allMonthFieldsSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setMonth2(Boolean.TRUE);
		bean.setMonth3(Boolean.TRUE);
		bean.setMonth4(Boolean.TRUE);
		bean.setMonth5(Boolean.TRUE);
		bean.setMonth7(Boolean.TRUE);
		bean.setMonth8(Boolean.TRUE);
		bean.setMonth9(Boolean.TRUE);
		bean.setMonth10(Boolean.TRUE);
		bean.setMonth11(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getMonth2());
		assertEquals(Boolean.TRUE, bean.getMonth11());
	}

	@Test
	@SuppressWarnings("static-method")
	void weekdayFieldsSetAndGet() {
		JobSchedule bean = JobSchedule.newInstance();
		bean.setAllWeekdays("*");
		bean.setWeekday1(Boolean.TRUE);
		bean.setWeekday2(Boolean.TRUE);
		bean.setWeekday3(Boolean.TRUE);
		bean.setWeekday4(Boolean.TRUE);
		bean.setWeekday5(Boolean.TRUE);
		bean.setWeekday6(Boolean.TRUE);
		bean.setWeekday7(Boolean.TRUE);
		assertEquals("*", bean.getAllWeekdays());
		assertEquals(Boolean.TRUE, bean.getWeekday1());
		assertEquals(Boolean.TRUE, bean.getWeekday7());
	}
}
