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

class ListGridSortTest {

	private ListGridSort step;

	@BeforeEach
	void setUp() {
		step = new ListGridSort();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getModuleName(), is(nullValue()));
		assertThat(step.getDocumentName(), is(nullValue()));
		assertThat(step.getQueryName(), is(nullValue()));
		assertThat(step.getModelName(), is(nullValue()));
		assertThat(step.getColumn(), is(nullValue()));
		assertThat(step.getDescending(), is(nullValue()));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setModuleName(" admin ");
		step.setDocumentName(" Contact ");
		step.setQueryName(" qContacts ");
		step.setModelName(" model ");
		step.setColumn(" name ");
		step.setDescending(Boolean.TRUE);
		assertThat(step.getModuleName(), is("admin"));
		assertThat(step.getDocumentName(), is("Contact"));
		assertThat(step.getQueryName(), is("qContacts"));
		assertThat(step.getModelName(), is("model"));
		assertThat(step.getColumn(), is("name"));
		assertThat(step.getDescending(), is(Boolean.TRUE));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setColumn("name");
		step.execute(executor);
		verify(executor).executeListGridSort(step);
	}

	@Test
	void getIdentifierReturnsListGridIdentifier() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = mock(AutomationContext.class);
		when(ctx.getModuleName()).thenReturn("admin");
		step.setQueryName("qContacts");
		assertThat(step.getIdentifier(ctx), is("admin.qContacts"));
	}
}
