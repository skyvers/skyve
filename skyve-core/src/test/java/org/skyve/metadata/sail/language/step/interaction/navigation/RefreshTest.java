package org.skyve.metadata.sail.language.step.interaction.navigation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

@SuppressWarnings("static-method")
class RefreshTest {

	@Test
	void executeCallsExecutor() {
		Refresh step = new Refresh();
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeRefresh(step);
	}

	@Test
	void getIdentifierIsTheStepName() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(new Refresh().getIdentifier(ctx), is("Refresh"));
	}
}
