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
		step.execute(executor);
		verify(executor).executeLookupDescriptionPick(step);
	}

	@Test
	void getIdentifierReturnsBindingWithSuffix() {
		step.setBinding("contact");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("contact.pick"));
	}
}
