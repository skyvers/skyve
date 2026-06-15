package org.skyve.job;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

import org.junit.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
public class JobDescriptionTest {
	@Test
	public void gettersAndSettersRoundTripAllProperties() {
		JobDescription description = new JobDescription();
		Timestamp start = new Timestamp();
		Timestamp end = new Timestamp();
		User user = mock(User.class);

		description.setName("job-name");
		description.setPercentComplete(73);
		description.setLogging("log-text");
		description.setStatus(JobStatus.cancelled);
		description.setStartTime(start);
		description.setEndTime(end);
		description.setUser(user);
		description.setInstanceId("instance-123");

		assertThat(description.getName(), is("job-name"));
		assertEquals(73, description.getPercentComplete());
		assertThat(description.getLogging(), is("log-text"));
		assertThat(description.getStatus(), is(JobStatus.cancelled));
		assertThat(description.getStartTime(), is(sameInstance(start)));
		assertThat(description.getEndTime(), is(sameInstance(end)));
		assertThat(description.getUser(), is(sameInstance(user)));
		assertThat(description.getInstanceId(), is("instance-123"));
	}

	@Test
	public void jobStatusExposesExpectedValues() {
		assertThat(JobStatus.valueOf("complete"), is(JobStatus.complete));
		assertThat(JobStatus.valueOf("failed"), is(JobStatus.failed));
		assertThat(JobStatus.valueOf("cancelled"), is(JobStatus.cancelled));
		assertEquals(3, JobStatus.values().length);
	}
}
