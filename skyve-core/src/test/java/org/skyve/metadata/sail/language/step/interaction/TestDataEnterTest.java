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

class TestDataEnterTest {

	private TestDataEnter step;

	@BeforeEach
	void setUp() {
		step = new TestDataEnter();
	}

	@Test
	void defaultFixtureIsNull() {
		assertThat(step.getFixture(), is(nullValue()));
	}

	@Test
	void setFixtureRoundtrip() {
		step.setFixture("myFixture");
		assertThat(step.getFixture(), is("myFixture"));
	}

	@Test
	void setFixtureTrimsWhitespace() {
		step.setFixture("  fixture1  ");
		assertThat(step.getFixture(), is("fixture1"));
	}

	@Test
	void setFixtureBlankBecomesNull() {
		step.setFixture("   ");
		assertThat(step.getFixture(), is(nullValue()));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeTestDataEnter(step);
	}

	@Test
	void getIdentifierReturnsNull() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is(nullValue()));
	}
}
