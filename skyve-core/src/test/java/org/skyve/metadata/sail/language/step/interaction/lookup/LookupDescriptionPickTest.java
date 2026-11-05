package org.skyve.metadata.sail.language.step.interaction.lookup;

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

class LookupDescriptionPickTest {

	private LookupDescriptionPick step;

	@BeforeEach
	void setUp() {
		step = new LookupDescriptionPick();
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
		step.setBinding("contactLookup");
		assertThat(step.getBinding(), is("contactLookup"));
	}

	@Test
	void setBindingTrimsWhitespace() {
		step.setBinding("  lookup  ");
		assertThat(step.getBinding(), is("lookup"));
	}

	@Test
	void setBindingBlankBecomesNull() {
		step.setBinding("   ");
		assertThat(step.getBinding(), is(nullValue()));
	}

	@Test
	void setRowRoundtrip() {
		step.setRow(Integer.valueOf(0));
		assertThat(step.getRow(), is(Integer.valueOf(0)));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setRow(Integer.valueOf(0));
		step.execute(executor);
		verify(executor).executeLookupDescriptionPick(step);
	}

	@Test
	void defaultRowContainingIsNull() {
		assertThat(step.getRowContaining(), is(nullValue()));
	}

	@Test
	void setRowContainingRoundtripTrims() {
		step.setRowContaining("  SAIL lookup {run}  ");
		assertThat(step.getRowContaining(), is("SAIL lookup {run}"));
	}

	@Test
	void executeWithRowContainingCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setRowContaining("SAIL lookup");
		step.execute(executor);
		verify(executor).executeLookupDescriptionPick(step);
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
		step.setRowContaining("SAIL lookup");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierReturnsBindingWithSuffix() {
		step.setBinding("contact");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("contact.pick"));
	}
}
