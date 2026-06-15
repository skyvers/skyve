package org.skyve.metadata.sail.language.step;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class PauseTest {

	private Pause step;

	@BeforeEach
	void setUp() {
		step = new Pause();
	}

	@Test
	void defaultMillisIsZero() {
		assertEquals(0L, step.getMillis());
	}

	@Test
	void setMillisRoundtrip() {
		step.setMillis(500L);
		assertEquals(500L, step.getMillis());
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executePause(step);
	}

	@Test
	void getIdentifierReturnsNull() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is(nullValue()));
	}
}
