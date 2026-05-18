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

class ExecuteTest {

	private Execute step;

	@BeforeEach
	void setUp() {
		step = new Execute();
	}

	@Test
	void defaultScriptIsNull() {
		assertThat(step.getScript(), is(nullValue()));
	}

	@Test
	void setScriptRoundtrip() {
		step.setScript("someScript()");
		assertThat(step.getScript(), is("someScript()"));
	}

	@Test
	void setScriptTrimsWhitespace() {
		step.setScript("  script  ");
		assertThat(step.getScript(), is("script"));
	}

	@Test
	void setScriptBlankBecomesNull() {
		step.setScript("  ");
		assertThat(step.getScript(), is(nullValue()));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeExecute(step);
	}

	@Test
	void getIdentifierReturnsNull() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is(nullValue()));
	}
}
