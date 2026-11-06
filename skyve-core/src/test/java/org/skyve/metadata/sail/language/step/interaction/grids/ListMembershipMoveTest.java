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

class ListMembershipMoveTest {

	private ListMembershipMove step;

	@BeforeEach
	void setUp() {
		step = new ListMembershipMove();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getBinding(), is(nullValue()));
		assertThat(step.getRow(), is(nullValue()));
		assertThat(step.getRowContaining(), is(nullValue()));
		assertThat(step.getTo(), is(nullValue()));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setBinding(" members ");
		step.setRowContaining(" Third {run} ");
		step.setTo(Integer.valueOf(0));
		assertThat(step.getBinding(), is("members"));
		assertThat(step.getRowContaining(), is("Third {run}"));
		assertThat(step.getTo(), is(Integer.valueOf(0)));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setBinding("members");
		step.setRow(Integer.valueOf(2));
		step.setTo(Integer.valueOf(0));
		step.execute(executor);
		verify(executor).executeListMembershipMove(step);
	}

	@Test
	void executeNeedsExactlyOneOfRowAndRowContaining() {
		Executor executor = mock(Executor.class);
		step.setBinding("members");
		step.setTo(Integer.valueOf(0));
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setRow(Integer.valueOf(0));
		step.setRowContaining("x");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeNeedsAPositionOfZeroOrMore() {
		Executor executor = mock(Executor.class);
		step.setBinding("members");
		step.setRow(Integer.valueOf(0));
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setTo(Integer.valueOf(-1));
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierReturnsTheBinding() {
		step.setBinding("members");
		assertThat(step.getIdentifier(mock(AutomationContext.class)), is("members"));
	}
}
