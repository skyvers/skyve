package modules.admin.Job.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.Job;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class RerunJobH2Test extends AbstractH2Test {
	@Test
	void executeWarnsWhenDisplayNameDoesNotMatchMetadataJob() {
		Job job = Job.newInstance();
		job.setDisplayName("No such job");
		WebContext webContext = mock(WebContext.class);

		ServerSideActionResult<Job> result = new RerunJob().execute(job, webContext);

		assertThat(result.getBean(), is(job));
		verify(webContext).growl(MessageSeverity.warn, "Unable to find this job to re-run");
	}
}
