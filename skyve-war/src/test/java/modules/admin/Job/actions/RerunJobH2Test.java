package modules.admin.Job.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.job.JobSchedulerStaticSingleton;
import org.skyve.job.JobScheduler;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import modules.admin.domain.Job;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class RerunJobH2Test extends AbstractH2Test {
	private JobScheduler scheduler;

	@BeforeEach
	void mockTheJobScheduler() {
		// Replaces the no-op scheduler that AbstractH2Test's beforeBase() (which runs first) installs, to see what is run
		scheduler = mock(JobScheduler.class);
		JobSchedulerStaticSingleton.set(scheduler);
	}

	@Test
	void executeWarnsWhenDisplayNameDoesNotMatchMetadataJob() {
		Job job = Job.newInstance();
		job.setDisplayName("No such job");
		WebContext webContext = mock(WebContext.class);

		ServerSideActionResult<Job> result = new RerunJob().execute(job, webContext);

		assertThat(result.getBean(), is(job));
		verify(webContext).growl(MessageSeverity.warn, "Unable to find this job to re-run");
	}

	/**
	 * A job started without a bean (a scheduled job) is rerun without one, as it first ran, not with the Job.
	 */
	@Test
	void executeRerunsAJobStartedWithoutABeanWithoutABean() {
		Job job = Job.newInstance();
		job.setStatus("Complete");
		job.setDisplayName("Truncate Audit Log");
		WebContext webContext = mock(WebContext.class);

		new RerunJob().execute(job, webContext);

		verify(scheduler).runOneShotJob(argThat((JobMetaData j) -> "Truncate Audit Log".equals(j.getDisplayName())),
											isNull(),
											any(User.class));
		verify(webContext).growl(MessageSeverity.info, "Started Truncate Audit Log");
	}

	/**
	 * Generate Test Data is started with the Control Panel, which the job record does not keep: the rerun gave the job
	 * the Job instead, and it failed with a ClassCastException. It is refused instead.
	 */
	@Test
	void executeRefusesAJobStartedWithABean() {
		Job job = Job.newInstance();
		job.setStatus("Complete");
		job.setDisplayName("Generate Test Data");
		job.setBeanBizId("01234567-89ab-cdef-0123-456789abcdef");
		job.setBeanModuleName("admin");
		job.setBeanDocumentName("ControlPanel");
		WebContext webContext = mock(WebContext.class);

		ServerSideActionResult<Job> result = new RerunJob().execute(job, webContext);

		assertThat(result.getBean(), is(job));
		verify(webContext).growl(MessageSeverity.warn,
									"This job can't be re-run, as the values it was started with are not kept. Start it again where it was started.");
		verifyNoInteractions(scheduler);
	}
}
