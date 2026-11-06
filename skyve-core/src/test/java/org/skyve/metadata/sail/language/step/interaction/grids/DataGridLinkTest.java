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

class DataGridLinkTest {

	private DataGridLink step;

	@BeforeEach
	void setUp() {
		step = new DataGridLink();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getBinding(), is(nullValue()));
		assertThat(step.getRow(), is(nullValue()));
		assertThat(step.getRowContaining(), is(nullValue()));
		assertThat(step.getText(), is(nullValue()));
		assertThat(step.getConfirm(), is(nullValue()));
		assertThat(step.getTestSuccess(), is(nullValue()));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setBinding(" rows ");
		step.setRowContaining(" SAIL {run} ");
		step.setText(" Revoke Access ");
		step.setConfirm(Boolean.FALSE);
		step.setTestSuccess(Boolean.FALSE);
		assertThat(step.getBinding(), is("rows"));
		assertThat(step.getRowContaining(), is("SAIL {run}"));
		assertThat(step.getText(), is("Revoke Access"));
		assertThat(step.getConfirm(), is(Boolean.FALSE));
		assertThat(step.getTestSuccess(), is(Boolean.FALSE));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setBinding("rows");
		step.setRow(Integer.valueOf(2));
		step.setText("Run From Row");
		step.execute(executor);
		verify(executor).executeDataGridLink(step);
	}

	@Test
	void executeNeedsExactlyOneOfRowAndRowContaining() {
		Executor executor = mock(Executor.class);
		step.setBinding("rows");
		step.setText("Run From Row");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setRow(Integer.valueOf(0));
		step.setRowContaining("x");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeNeedsAText() {
		Executor executor = mock(Executor.class);
		step.setBinding("rows");
		step.setRow(Integer.valueOf(0));
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierReturnsTheBinding() {
		step.setBinding("rows");
		assertThat(step.getIdentifier(mock(AutomationContext.class)), is("rows"));
	}
}
