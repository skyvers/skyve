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

class DataGridSelectTest {

	private DataGridSelect step;

	@BeforeEach
	void setUp() {
		step = new DataGridSelect();
	}

	@Test
	void defaultBindingIsNull() {
		assertThat(step.getBinding(), is(nullValue()));
	}

	@Test
	void defaultRowIsNull() {
		assertThat(step.getRow(), is(nullValue()));
	}

	@Test
	void setBindingRoundtrip() {
		step.setBinding("contacts");
		assertThat(step.getBinding(), is("contacts"));
	}

	@Test
	void setRowRoundtrip() {
		step.setRow(Integer.valueOf(3));
		assertThat(step.getRow(), is(Integer.valueOf(3)));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeDataGridSelect(step);
	}

	@Test
	void getIdentifierReturnsDotSelect() {
		step.setBinding("contacts");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("contacts.select"));
	}
}
