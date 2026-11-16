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

	/**
	 * Generate Test Data is started with the Control Panel, which a rerun cannot give back: the rerun failed with a
	 * ClassCastException, as the job was given the Job instead.
	 */
	@Test
	void rerunnableReturnsFalseWhenTheJobWasStartedWithABean() {
		JobExtension job = new JobExtension();
		job.setStatus("Complete");
		job.setDisplayName("Generate Test Data");
		job.setBeanBizId("01234567-89ab-cdef-0123-456789abcdef");
		job.setBeanModuleName("admin");
		job.setBeanDocumentName("ControlPanel");

		assertFalse(job.rerunnable());
	}

	@Test
	void rerunnableReturnsFalseWhenCompletedJobDisplayNameDoesNotMatchMetadata() {
		JobExtension job = new JobExtension();
		job.setStatus("Complete");
		job.setDisplayName("Not a configured job");

		assertFalse(job.rerunnable());
	}
}
