package org.skyve.metadata.sail.language.step.interaction.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.execution.Executor;

class SaveTest {

	private Save step;

	@BeforeEach
	void setUp() {
		step = new Save();
	}

	@Test
	void defaultCreateViewIsNull() {
		assertThat(step.getCreateView(), is(nullValue()));
	}

	@Test
	void setCreateViewRoundtrip() {
		step.setCreateView(Boolean.TRUE);
		assertThat(step.getCreateView(), is(Boolean.TRUE));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeSave(step);
	}

	@Test
	void getIdentifierReturnsSave() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is(ImplicitActionName.Save.toString()));
	}

	@Test
	void testSuccessDefaultIsNull() {
		assertThat(step.getTestSuccess(), is(nullValue()));
	}
}
