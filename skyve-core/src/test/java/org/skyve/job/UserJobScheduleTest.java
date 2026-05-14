package org.skyve.job;

import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.metadata.user.User;

@ExtendWith(MockitoExtension.class)
public class UserJobScheduleTest {

	@Mock
	private JobSchedule jobSchedule;

	@Mock
	private User user;

	@Test
	public void constructorStoresJobSchedule() {
		UserJobSchedule ujs = new UserJobSchedule(jobSchedule, user);
		assertSame(jobSchedule, ujs.getJobSchedule());
	}

	@Test
	public void constructorStoresUser() {
		UserJobSchedule ujs = new UserJobSchedule(jobSchedule, user);
		assertSame(user, ujs.getUser());
	}
}
