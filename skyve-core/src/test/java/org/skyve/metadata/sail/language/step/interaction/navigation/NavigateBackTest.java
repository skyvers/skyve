package org.skyve.metadata.sail.language.step.interaction.navigation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

@SuppressWarnings("static-method")
class NavigateBackTest {

	@Test
	void executeCallsExecutor() {
		NavigateBack step = new NavigateBack();
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeNavigateBack(step);
	}

	@Test
	void getIdentifierIsTheStepName() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(new NavigateBack().getIdentifier(ctx), is("NavigateBack"));
	}
}
