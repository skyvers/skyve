package modules.admin.Job;

import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class JobExtensionTest {

	@Test
	void rerunnableWithNullStatusReturnsFalse() {
		JobExtension job = new JobExtension();
		// getStatus() is null → returns false immediately
		assertFalse(job.rerunnable());
	}
}
