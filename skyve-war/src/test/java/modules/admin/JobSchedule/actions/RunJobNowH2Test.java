package modules.admin.JobSchedule.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.JobSchedule.JobScheduleExtension;
import modules.admin.domain.JobSchedule;
import util.AbstractH2Test;

/**
 * H2-backed tests for RunJobNow action covering validation and execution paths.
 */
public class RunJobNowH2Test extends AbstractH2Test {

	private JobSchedule bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		bean = new JobScheduleExtension();
		webContext = new MockWebContext();
	}

	// ---- Validation: null jobName ----

	@Test
	void executeWithNullJobNameThrowsValidationException() {
		bean.setJobName(null);

		RunJobNow action = new RunJobNow();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}

	// ---- Validation: malformed jobName (no dot) ----

	@Test
	void executeWithJobNameMissingDotThrowsValidationException() {
		bean.setJobName("invalidJobName");

		RunJobNow action = new RunJobNow();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}

	// ---- Valid jobName: admin.jAdhocBackup ----

	@Test
	void executeWithValidJobNameStartsJob() throws Exception {
		bean.setJobName("admin.jAdhocBackup");

		RunJobNow action = new RunJobNow();
		ServerSideActionResult<JobSchedule> result = action.execute(bean, webContext);
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));
		assertThat(result.getBean().getJobScheduledImmediately(), is(Boolean.TRUE));
	}
}
