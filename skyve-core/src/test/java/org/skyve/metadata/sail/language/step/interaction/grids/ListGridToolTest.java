package org.skyve.metadata.sail.language.step.interaction.grids;

import static org.hamcrest.CoreMatchers.containsString;
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
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridTool.Tool;

class ListGridToolTest {

	private ListGridTool step;

	@BeforeEach
	void setUp() {
		step = new ListGridTool();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getModuleName(), is(nullValue()));
		assertThat(step.getDocumentName(), is(nullValue()));
		assertThat(step.getQueryName(), is(nullValue()));
		assertThat(step.getModelName(), is(nullValue()));
		assertThat(step.getTool(), is(nullValue()));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setModuleName(" admin ");
		step.setDocumentName(" Contact ");
		step.setQueryName(" qContacts ");
		step.setModelName(" model ");
		step.setTool(Tool.clearSelection);
		assertThat(step.getModuleName(), is("admin"));
		assertThat(step.getDocumentName(), is("Contact"));
		assertThat(step.getQueryName(), is("qContacts"));
		assertThat(step.getModelName(), is("model"));
		assertThat(step.getTool(), is(Tool.clearSelection));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setTool(Tool.refresh);
		step.execute(executor);
		verify(executor).executeListGridTool(step);
	}

	@Test
	void executeWithoutAToolThrowsNamingTheTools() {
		Executor executor = mock(Executor.class);
		MetaDataException e = assertThrows(MetaDataException.class, () -> step.execute(executor));
		assertThat(e.getMessage(), containsString("requires [tool], one of [refresh, clearSelection]"));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierReturnsListGridIdentifier() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = mock(AutomationContext.class);
		when(ctx.getModuleName()).thenReturn("admin");
		step.setQueryName("qContacts");
		assertThat(step.getIdentifier(ctx), is("admin.qContacts"));
	}
}
