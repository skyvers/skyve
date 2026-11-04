package org.skyve.metadata.sail.language.step.interaction.session;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class SwitchModeTest {

	@Test
	@SuppressWarnings("static-method")
	void executeCallsExecutor() {
		SwitchMode step = new SwitchMode();
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeSwitchMode(step);
	}

	@Test
	@SuppressWarnings("static-method")
	void getIdentifierReturnsSwitchMode() {
		assertThat(new SwitchMode().getIdentifier(mock(AutomationContext.class)), is("SwitchMode"));
	}
}
