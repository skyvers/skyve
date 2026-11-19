package org.skyve.metadata.sail.language.step.interaction.navigation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

@SuppressWarnings("static-method")
class CloseWindowTest {

	@Test
	void executeCallsExecutor() {
		CloseWindow step = new CloseWindow();
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeCloseWindow(step);
	}

	@Test
	void getIdentifierIsTheStepName() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(new CloseWindow().getIdentifier(ctx), is("CloseWindow"));
	}
}
