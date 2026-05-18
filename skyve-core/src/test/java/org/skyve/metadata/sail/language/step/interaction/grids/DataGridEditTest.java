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

class DataGridEditTest {

	private DataGridEdit step;

	@BeforeEach
	void setUp() {
		step = new DataGridEdit();
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
		step.setRow(Integer.valueOf(1));
		assertThat(step.getRow(), is(Integer.valueOf(1)));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeDataGridEdit(step);
	}

	@Test
	void getIdentifierReturnsDotEdit() {
		step.setBinding("contacts");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("contacts.edit"));
	}
}
