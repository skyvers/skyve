package org.skyve.metadata.sail.language.step.context;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class ClearContextTest {

	private ClearContext step;

	@BeforeEach
	void setUp() {
		step = new ClearContext();
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeClearContext(step);
	}

	@Test
	void getIdentifierReturnsNull() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is(nullValue()));
	}
}
