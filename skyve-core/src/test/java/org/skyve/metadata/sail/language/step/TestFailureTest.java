package org.skyve.metadata.sail.language.step;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class TestFailureTest {

	private TestFailure step;

	@BeforeEach
	void setUp() {
		step = new TestFailure();
	}

	@Test
	void defaultMessageIsNull() {
		assertThat(step.getMessage(), is(nullValue()));
	}

	@Test
	void setMessageRoundtrip() {
		step.setMessage("expected error");
		assertThat(step.getMessage(), is("expected error"));
	}

	@Test
	void setMessageTrimsWhitespace() {
		step.setMessage("  error  ");
		assertThat(step.getMessage(), is("error"));
	}

	@Test
	void setMessageBlankBecomesNull() {
		step.setMessage("  ");
		assertThat(step.getMessage(), is(nullValue()));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeTestFailure(step);
	}

	@Test
	void getIdentifierReturnsFailure() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("Failure"));
	}
}
