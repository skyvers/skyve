package org.skyve.impl.generate.sail;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;
import org.skyve.metadata.user.User;

@SuppressWarnings({"static-method", "boxing"})
class GeneratorTest {
	@Test
	void addLoginAndOutCreatesBeforeAndAfterProcedures() throws Exception {
		User user = mock(User.class);
		when(user.getName()).thenReturn("admin");
		Automation automation = new Automation();

		invokeAddLoginAndOut(automation, user, "demo", "secret");

		assertThat(automation.getBefore().getSteps().size(), is(1));
		assertThat(automation.getAfter().getSteps().size(), is(1));
		Step before = automation.getBefore().getSteps().get(0);
		assertThat(before, instanceOf(Login.class));
		Login login = (Login) before;
		assertThat(login.getCustomer(), is("demo"));
		assertThat(login.getUser(), is("admin"));
		assertThat(login.getPassword(), is("secret"));
		assertThat(automation.getAfter().getSteps().get(0), instanceOf(Logout.class));
	}

	@Test
	void addLoginAndOutPrependsLoginToExistingBeforeAndAppendsLogoutToExistingAfter() throws Exception {
		User user = mock(User.class);
		when(user.getName()).thenReturn("admin");
		Automation automation = new Automation();
		Procedure before = new Procedure();
		Comment existingBefore = new Comment();
		before.getSteps().add(existingBefore);
		automation.setBefore(before);
		Procedure after = new Procedure();
		Comment existingAfter = new Comment();
		after.getSteps().add(existingAfter);
		automation.setAfter(after);

		invokeAddLoginAndOut(automation, user, null, "secret");

		assertThat(automation.getBefore().getSteps().size(), is(2));
		assertThat(automation.getBefore().getSteps().get(0), instanceOf(Login.class));
		assertThat(automation.getBefore().getSteps().get(1), is(existingBefore));
		assertThat(automation.getAfter().getSteps().size(), is(2));
		assertThat(automation.getAfter().getSteps().get(0), is(existingAfter));
		assertThat(automation.getAfter().getSteps().get(1), instanceOf(Logout.class));
	}

	private static void invokeAddLoginAndOut(Automation automation, User user, String customer, String password) throws Exception {
		Method method = Generator.class.getDeclaredMethod("addLoginAndOut", Automation.class, User.class, String.class, String.class);
		method.setAccessible(true);
		method.invoke(null, automation, user, customer, password);
	}
}
