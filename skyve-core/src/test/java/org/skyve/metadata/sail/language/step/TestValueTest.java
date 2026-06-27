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

class TestValueTest {

	private TestValue step;

	@BeforeEach
	void setUp() {
		step = new TestValue();
	}

	@Test
	void defaultBindingIsNull() {
		assertThat(step.getBinding(), is(nullValue()));
	}

	@Test
	void defaultValueIsNull() {
		assertThat(step.getValue(), is(nullValue()));
	}

	@Test
	void setBindingRoundtrip() {
		step.setBinding("firstName");
		assertThat(step.getBinding(), is("firstName"));
	}

	@Test
	void setBindingTrimsWhitespace() {
		step.setBinding("  binding  ");
		assertThat(step.getBinding(), is("binding"));
	}

	@Test
	void setBindingBlankBecomesNull() {
		step.setBinding("   ");
		assertThat(step.getBinding(), is(nullValue()));
	}

	@Test
	void setValueRoundtrip() {
		step.setValue("expectedValue");
		assertThat(step.getValue(), is("expectedValue"));
	}

	@Test
	void setValueBlankBecomesNull() {
		step.setValue("  ");
		assertThat(step.getValue(), is(nullValue()));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeTestValue(step);
	}

	@Test
	void getIdentifierReturnsBinding() {
		step.setBinding("lastName");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("lastName"));
	}
}
