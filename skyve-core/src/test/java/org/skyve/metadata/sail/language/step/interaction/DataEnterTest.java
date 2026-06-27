package org.skyve.metadata.sail.language.step.interaction;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class DataEnterTest {

	private DataEnter step;

	@BeforeEach
	void setUp() {
		step = new DataEnter();
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
	void defaultNavigateToWidgetIsNull() {
		assertThat(step.getNavigateToWidget(), is(nullValue()));
	}

	@Test
	void setBindingRoundtrip() {
		step.setBinding("contact.firstName");
		assertThat(step.getBinding(), is("contact.firstName"));
	}

	@Test
	void setBindingTrimsWhitespace() {
		step.setBinding("  myBinding  ");
		assertThat(step.getBinding(), is("myBinding"));
	}

	@Test
	void setBindingBlankBecomesNull() {
		step.setBinding("   ");
		assertThat(step.getBinding(), is(nullValue()));
	}

	@Test
	void setValueRoundtrip() {
		step.setValue("John");
		assertThat(step.getValue(), is("John"));
	}

	@Test
	void setValueBlankBecomesNull() {
		step.setValue("  ");
		assertThat(step.getValue(), is(nullValue()));
	}

	@Test
	void setNavigateToWidgetRoundtrip() {
		step.setNavigateToWidget(Boolean.TRUE);
		assertThat(step.getNavigateToWidget(), is(Boolean.TRUE));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeDataEnter(step);
	}

	@Test
	void getIdentifierReturnsBinding() {
		step.setBinding("myField");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("myField"));
	}
}
