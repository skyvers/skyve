package org.skyve.metadata.sail.language.step.interaction.navigation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class TestMenuTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultsAreNull() {
		TestMenu step = new TestMenu();
		assertThat(step.getModuleName(), is(nullValue()));
		assertThat(step.getItemName(), is(nullValue()));
		assertThat(step.getPresent(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void settersRoundtripAndIdentifier() {
		TestMenu step = new TestMenu();
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		step.setModuleName(" admin ");
		assertThat(step.getIdentifier(ctx), is("admin"));
		step.setItemName(" Users ");
		step.setPresent(Boolean.FALSE);
		assertThat(step.getModuleName(), is("admin"));
		assertThat(step.getItemName(), is("Users"));
		assertThat(step.getPresent(), is(Boolean.FALSE));
		assertThat(step.getIdentifier(ctx), is("admin/Users"));
	}

	@Test
	@SuppressWarnings("static-method")
	void executeCallsExecutor() {
		TestMenu step = new TestMenu();
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeTestMenu(step);
	}
}
