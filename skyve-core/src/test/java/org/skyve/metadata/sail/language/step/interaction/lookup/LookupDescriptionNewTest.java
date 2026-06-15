package org.skyve.metadata.sail.language.step.interaction.lookup;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class LookupDescriptionNewTest {

	private LookupDescriptionNew step;

	@BeforeEach
	void setUp() {
		step = new LookupDescriptionNew();
	}

	@Test
	void defaultBindingIsNull() {
		assertThat(step.getBinding(), is(nullValue()));
	}

	@Test
	void setBindingRoundtrip() {
		step.setBinding("customer");
		assertThat(step.getBinding(), is("customer"));
	}

	@Test
	void setBindingTrimsWhitespace() {
		step.setBinding("  customer  ");
		assertThat(step.getBinding(), is("customer"));
	}

	@Test
	void setBindingBlankBecomesNull() {
		step.setBinding("  ");
		assertThat(step.getBinding(), is(nullValue()));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeLookupDescriptionNew(step);
	}

	@Test
	void getIdentifierReturnsDotNew() {
		step.setBinding("customer");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("customer.new"));
	}
}
