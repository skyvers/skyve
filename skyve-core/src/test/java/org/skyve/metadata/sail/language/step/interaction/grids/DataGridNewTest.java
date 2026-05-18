package org.skyve.metadata.sail.language.step.interaction.grids;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class DataGridNewTest {

	private DataGridNew step;

	@BeforeEach
	void setUp() {
		step = new DataGridNew();
	}

	@Test
	void defaultBindingIsNull() {
		assertThat(step.getBinding(), is(nullValue()));
	}

	@Test
	void setBindingRoundtrip() {
		step.setBinding("contacts");
		assertThat(step.getBinding(), is("contacts"));
	}

	@Test
	void setBindingTrimsWhitespace() {
		step.setBinding("  contacts  ");
		assertThat(step.getBinding(), is("contacts"));
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
		verify(executor).executeDataGridNew(step);
	}

	@Test
	void getIdentifierReturnsDotNew() {
		step.setBinding("contacts");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("contacts.new"));
	}
}
