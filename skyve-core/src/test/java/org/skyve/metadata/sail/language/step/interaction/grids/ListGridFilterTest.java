package org.skyve.metadata.sail.language.step.interaction.grids;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.sail.execution.GenerateEditContext;
import org.skyve.impl.sail.execution.GenerateListContext;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridFilter.FilterOperator;

class ListGridFilterTest {

	private ListGridFilter step;

	@BeforeEach
	void setUp() {
		step = new ListGridFilter();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getModuleName(), is(nullValue()));
		assertThat(step.getDocumentName(), is(nullValue()));
		assertThat(step.getQueryName(), is(nullValue()));
		assertThat(step.getModelName(), is(nullValue()));
		assertThat(step.getColumn(), is(nullValue()));
		assertThat(step.getValue(), is(nullValue()));
		assertThat(step.getOperator(), is(nullValue()));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setModuleName(" admin ");
		step.setDocumentName(" Contact ");
		step.setQueryName(" qContacts ");
		step.setModelName(" model ");
		step.setColumn(" name ");
		step.setValue(" Smith{run} ");
		step.setOperator(FilterOperator.startsWith);
		assertThat(step.getModuleName(), is("admin"));
		assertThat(step.getDocumentName(), is("Contact"));
		assertThat(step.getQueryName(), is("qContacts"));
		assertThat(step.getModelName(), is("model"));
		assertThat(step.getColumn(), is("name"));
		assertThat(step.getValue(), is("Smith{run}"));
		assertThat(step.getOperator(), is(FilterOperator.startsWith));
	}

	@Test
	void blankValueBecomesNull() {
		step.setValue("   ");
		assertThat(step.getValue(), is(nullValue()));
	}

	@Test
	void effectiveOperatorDefaultsToContains() {
		assertThat(step.getEffectiveOperator(), is(FilterOperator.contains));
		step.setOperator(FilterOperator.greaterThan);
		assertThat(step.getEffectiveOperator(), is(FilterOperator.greaterThan));
	}

	@Test
	void executeWithValueCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setColumn("name");
		step.setValue("Smith");
		step.setOperator(FilterOperator.equals);
		step.execute(executor);
		verify(executor).executeListGridFilter(step);
	}

	@Test
	void executeWithoutValueClearsTheColumn() {
		Executor executor = mock(Executor.class);
		step.setColumn("name");
		step.execute(executor);
		verify(executor).executeListGridFilter(step);
	}

	@Test
	void executeOperatorWithoutValueThrows() {
		Executor executor = mock(Executor.class);
		step.setColumn("name");
		step.setOperator(FilterOperator.equals);
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierReturnsListGridIdentifier() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = mock(AutomationContext.class);
		when(ctx.getModuleName()).thenReturn("admin");
		when(ctx.getDocumentName()).thenReturn("Contact");
		assertThat(step.getIdentifier(ctx), is("admin.Contact"));
		step.setQueryName("qContacts");
		assertThat(step.getIdentifier(ctx), is("admin.qContacts"));
	}
}
