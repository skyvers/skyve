package modules.admin.Job;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class JobExtensionH2Test extends AbstractH2Test {
	@Test
	void rerunnableReturnsTrueForUniqueKnownCompletedJobDisplayName() {
		JobExtension job = new JobExtension();
		job.setStatus("Complete");
		job.setDisplayName("Truncate Audit Log");

		assertTrue(job.rerunnable());
	}

	@Test
	void rerunnableReturnsFalseWhenCompletedJobDisplayNameDoesNotMatchMetadata() {
		JobExtension job = new JobExtension();
		job.setStatus("Complete");
		job.setDisplayName("Not a configured job");

		assertFalse(job.rerunnable());
	}
}
