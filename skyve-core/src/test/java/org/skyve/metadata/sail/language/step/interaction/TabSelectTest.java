package org.skyve.metadata.sail.language.step.interaction;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class TabSelectTest {

	private TabSelect step;

	@BeforeEach
	void setUp() {
		step = new TabSelect();
	}

	@Test
	void defaultTabPathIsNull() {
		assertThat(step.getTabPath(), is(nullValue()));
	}

	@Test
	void setTabPathRoundtrip() {
		step.setTabPath("Details");
		assertThat(step.getTabPath(), is("Details"));
	}

	@Test
	void setTabPathTrimsWhitespace() {
		step.setTabPath("  Details  ");
		assertThat(step.getTabPath(), is("Details"));
	}

	@Test
	void setTabPathBlankBecomesNull() {
		step.setTabPath("   ");
		assertThat(step.getTabPath(), is(nullValue()));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeTabSelect(step);
	}

	@Test
	void getIdentifierReturnsTabPathWithSuffix() {
		step.setTabPath("Details");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("Details Tab"));
	}
}
