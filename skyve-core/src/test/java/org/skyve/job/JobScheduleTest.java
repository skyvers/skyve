package org.skyve.job;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Date;

import org.junit.jupiter.api.Test;

public class JobScheduleTest {

	@Test
	@SuppressWarnings("static-method")
	public void defaultConstructorHasNullFields() {
		JobSchedule schedule = new JobSchedule();
		assertNull(schedule.getUuid());
		assertNull(schedule.getJobName());
		assertNull(schedule.getStartTime());
		assertNull(schedule.getEndTime());
		assertNull(schedule.getCronExpression());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setUuidRoundtrip() {
		JobSchedule schedule = new JobSchedule();
		schedule.setUuid("abc-123");
		assertThat(schedule.getUuid(), is("abc-123"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setJobNameRoundtrip() {
		JobSchedule schedule = new JobSchedule();
		schedule.setJobName("myJob");
		assertThat(schedule.getJobName(), is("myJob"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setStartTimeRoundtrip() {
		JobSchedule schedule = new JobSchedule();
		Date now = new Date();
		schedule.setStartTime(now);
		assertThat(schedule.getStartTime(), is(now));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setEndTimeRoundtrip() {
		JobSchedule schedule = new JobSchedule();
		Date now = new Date();
		schedule.setEndTime(now);
		assertThat(schedule.getEndTime(), is(now));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setCronExpressionRoundtrip() {
		JobSchedule schedule = new JobSchedule();
		schedule.setCronExpression("0 0 * * *");
		assertThat(schedule.getCronExpression(), is("0 0 * * *"));
	}
}
