package org.skyve.metadata.sail.language.step.interaction.navigation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class NavigateMenuTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultsAreNull() {
		NavigateMenu step = new NavigateMenu();
		assertThat(step.getModuleName(), is(nullValue()));
		assertThat(step.getItemName(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void settersRoundtripAndIdentifier() {
		NavigateMenu step = new NavigateMenu();
		step.setModuleName(" admin ");
		step.setItemName(" Security Admin/Users ");
		assertThat(step.getModuleName(), is("admin"));
		assertThat(step.getItemName(), is("Security Admin/Users"));
		assertThat(step.getIdentifier(mock(AutomationContext.class)), is("admin/Security Admin/Users"));
	}

	@Test
	@SuppressWarnings("static-method")
	void executeCallsExecutor() {
		NavigateMenu step = new NavigateMenu();
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeNavigateMenu(step);
	}
}
