package modules.admin.JobSchedule;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.domain.JobSchedule;
import util.AbstractH2Test;

/**
 * H2-backed tests for JobScheduleBizlet covering postLoad and getVariantDomainValues.
 */
public class JobScheduleBizletH2Test extends AbstractH2Test {

	private static final JobScheduleBizlet bizlet = new JobScheduleBizlet();

	// ---- postLoad: allMinutes="*" when cron has wildcard minute ----

	@Test
	void postLoadWithAllMinutesCronSetsAllMinutesWildcard() throws Exception {
		// "0 * * * * ?" - every minute, every hour, every day
		JobScheduleExtension bean = new JobScheduleExtension();
		bean.setCronExpression("0 * * * * ?");

		bizlet.postLoad(bean);

		assertThat(bean.getAllMinutes(), is("*"));
		assertThat(bean.getAllHours(), is("*"));
	}

	@Test
	void postLoadWithSelectedMinutesCronSetsSelectedCode() throws Exception {
		// "0 30 14 * * ?" - at 2:30pm every day
		JobScheduleExtension bean = new JobScheduleExtension();
		bean.setCronExpression("0 30 14 * * ?");

		bizlet.postLoad(bean);

		assertThat(bean.getAllMinutes(), is("X"));
		assertThat(bean.getAllHours(), is("X"));
	}

	@Test
	void postLoadWithAllDaysCronSetsAllDays() throws Exception {
		// "0 0 12 * * ?" - noon every day, all days, all months
		JobScheduleExtension bean = new JobScheduleExtension();
		bean.setCronExpression("0 0 12 * * ?");

		bizlet.postLoad(bean);

		assertThat(bean.getAllDays(), is("*"));
		assertThat(bean.getAllMonths(), is("*"));
	}

	@Test
	void postLoadWithSelectedDaysCronSetsSelectedDays() throws Exception {
		// "0 0 0 15 * ?" - at midnight on the 15th of every month
		JobScheduleExtension bean = new JobScheduleExtension();
		bean.setCronExpression("0 0 0 15 * ?");

		bizlet.postLoad(bean);

		assertThat(bean.getAllDays(), is("X"));
	}

	@Test
	void postLoadWithLastDayCronSetsLastDay() throws Exception {
		// "0 0 0 L * ?" - last day of month
		JobScheduleExtension bean = new JobScheduleExtension();
		bean.setCronExpression("0 0 0 L * ?");

		bizlet.postLoad(bean);

		assertThat(bean.getAllDays(), is("L"));
	}

	@Test
	void postLoadWithLastWeekDayCronSetsLastWeekDay() throws Exception {
		// "0 0 0 LW * ?" - last weekday of month
		JobScheduleExtension bean = new JobScheduleExtension();
		bean.setCronExpression("0 0 0 LW * ?");

		bizlet.postLoad(bean);

		assertThat(bean.getAllDays(), is("LW"));
	}

	@Test
	void postLoadWithSelectedMonthsCronSetsSelectedMonths() throws Exception {
		// "0 0 12 * 6 ?" - noon every day only in June
		JobScheduleExtension bean = new JobScheduleExtension();
		bean.setCronExpression("0 0 12 * 6 ?");

		bizlet.postLoad(bean);

		assertThat(bean.getAllMonths(), is("X"));
	}

	@Test
	void postLoadWithSelectedWeekdaysCronSetsSelectedWeekdays() throws Exception {
		// "0 0 12 ? * 2" - noon every Monday
		JobScheduleExtension bean = new JobScheduleExtension();
		bean.setCronExpression("0 0 12 ? * 2");

		bizlet.postLoad(bean);

		assertThat(bean.getAllWeekdays(), is("X"));
	}

	// ---- getVariantDomainValues ----

	@Test
	void getVariantDomainValuesForJobNameReturnsNonEmptyList() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues(JobSchedule.jobNamePropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least one job in the admin module");
	}

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsEmptyList() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues("unknownAttribute");
		assertThat(result, is(notNullValue()));
		assertThat(result.isEmpty(), is(true));
	}
}
