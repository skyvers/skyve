package org.skyve.metadata.sail.language.step.interaction.grids;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.sail.execution.GenerateEditContext;
import org.skyve.impl.sail.execution.GenerateListContext;
import org.skyve.metadata.sail.execution.Executor;

class ListGridClearFilterTest {

	private ListGridClearFilter step;

	@BeforeEach
	void setUp() {
		step = new ListGridClearFilter();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getModuleName(), is(nullValue()));
		assertThat(step.getDocumentName(), is(nullValue()));
		assertThat(step.getQueryName(), is(nullValue()));
		assertThat(step.getModelName(), is(nullValue()));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setModuleName(" admin ");
		step.setDocumentName(" Contact ");
		step.setQueryName(" qContacts ");
		step.setModelName(" model ");
		assertThat(step.getModuleName(), is("admin"));
		assertThat(step.getDocumentName(), is("Contact"));
		assertThat(step.getQueryName(), is("qContacts"));
		assertThat(step.getModelName(), is("model"));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeListGridClearFilter(step);
	}

	@Test
	void getIdentifierReturnsListGridIdentifier() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = mock(AutomationContext.class);
		when(ctx.getModuleName()).thenReturn("admin");
		when(ctx.getDocumentName()).thenReturn("Contact");
		step.setModelName("model");
		assertThat(step.getIdentifier(ctx), is("admin.Contact.model"));
	}
}
