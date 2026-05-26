package modules.admin.JobSchedule.actions;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.JobSchedule.JobScheduleExtension;

@SuppressWarnings("static-method")
public class RunJobNowTest {

	@Test
	void executeWithNullJobNameThrowsValidationException() {
		RunJobNow action = new RunJobNow();
		JobScheduleExtension bean = new JobScheduleExtension();
		// jobName is null → throws before calling CORE
		assertThrows(ValidationException.class, () -> action.execute(bean, null));
	}
}
