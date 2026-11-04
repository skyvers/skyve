package org.skyve.metadata.sail.language.step.interaction.grids;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.MetaDataException;
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
		step.setRow(Integer.valueOf(0));
		step.execute(executor);
		verify(executor).executeDataGridSelect(step);
	}

	@Test
	void getIdentifierReturnsDotSelect() {
		step.setBinding("contacts");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("contacts.select"));
	}

	// ---- rowContaining ----

	@Test
	void defaultRowContainingIsNull() {
		assertThat(step.getRowContaining(), is(nullValue()));
	}

	@Test
	void setRowContainingRoundtripTrims() {
		step.setRowContaining("  Jane {run} ");
		assertThat(step.getRowContaining(), is("Jane {run}"));
	}

	@Test
	void setRowNullAllowed() {
		step.setRow(null);
		assertThat(step.getRow(), is(nullValue()));
	}

	@Test
	void executeWithRowContainingCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setRowContaining("Jane");
		step.execute(executor);
		verify(executor).executeDataGridSelect(step);
	}

	@Test
	void executeWithoutRowOrRowContainingThrows() {
		Executor executor = mock(Executor.class);
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeWithRowAndRowContainingThrows() {
		Executor executor = mock(Executor.class);
		step.setRow(Integer.valueOf(0));
		step.setRowContaining("Jane");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}
}
