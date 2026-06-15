package org.skyve.metadata.sail.language.step.interaction.grids;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.sail.execution.GenerateEditContext;
import org.skyve.impl.sail.execution.GenerateListContext;
import org.skyve.metadata.sail.execution.Executor;

class ListGridSelectTest {

	private ListGridSelect select;

	@BeforeEach
	void setUp() {
		select = new ListGridSelect();
	}

	// ---- default state ----

	@Test
	void defaultModuleNameIsNull() {
		assertThat(select.getModuleName(), is(nullValue()));
	}

	@Test
	void defaultDocumentNameIsNull() {
		assertThat(select.getDocumentName(), is(nullValue()));
	}

	@Test
	void defaultQueryNameIsNull() {
		assertThat(select.getQueryName(), is(nullValue()));
	}

	@Test
	void defaultModelNameIsNull() {
		assertThat(select.getModelName(), is(nullValue()));
	}

	@Test
	void defaultRowIsNull() {
		assertThat(select.getRow(), is(nullValue()));
	}

	// ---- setters / getters ----

	@Test
	void setModuleNameRoundtrip() {
		select.setModuleName("admin");
		assertThat(select.getModuleName(), is("admin"));
	}

	@Test
	void setModuleNameTrimsWhitespace() {
		select.setModuleName("  admin  ");
		assertThat(select.getModuleName(), is("admin"));
	}

	@Test
	void setModuleNameBlankBecomesNull() {
		select.setModuleName("   ");
		assertThat(select.getModuleName(), is(nullValue()));
	}

	@Test
	void setDocumentNameRoundtrip() {
		select.setDocumentName("Contact");
		assertThat(select.getDocumentName(), is("Contact"));
	}

	@Test
	void setQueryNameRoundtrip() {
		select.setQueryName("qContacts");
		assertThat(select.getQueryName(), is("qContacts"));
	}

	@Test
	void setModelNameRoundtrip() {
		select.setModelName("myModel");
		assertThat(select.getModelName(), is("myModel"));
	}

	@Test
	void setRowRoundtrip() {
		select.setRow(Integer.valueOf(2));
		assertThat(select.getRow(), is(Integer.valueOf(2)));
	}

	// ---- execute ----

	@Test
	void executeCallsExecutorListGridSelect() {
		Executor executor = mock(Executor.class);
		select.execute(executor);
		verify(executor).executeListGridSelect(select);
	}

	// ---- getIdentifier with queryName ----

	@Test
	void getIdentifierWithQueryName() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		select.setModuleName("admin");
		select.setQueryName("qContacts");
		String id = select.getIdentifier(ctx);
		assertThat(id, is("admin.qContacts.select"));
	}

	// ---- getIdentifier with modelName ----

	@Test
	void getIdentifierWithModelName() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		select.setModuleName("admin");
		select.setDocumentName("Contact");
		select.setModelName("myModel");
		String id = select.getIdentifier(ctx);
		assertThat(id, is("admin.Contact.myModel.select"));
	}

	// ---- getIdentifier fallback to document ----

	@Test
	void getIdentifierFallsBackToDocument() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		select.setModuleName("admin");
		select.setDocumentName("Contact");
		String id = select.getIdentifier(ctx);
		assertThat(id, is("admin.Contact.select"));
	}

	// ---- getIdentifier uses context when module/doc null ----

	@Test
	void getIdentifierUsesContextModuleAndDocumentWhenNull() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("security", "User");
		String id = select.getIdentifier(ctx);
		assertThat(id, is("security.User.select"));
	}

	// Helper to create a minimal AutomationContext
	private static AutomationContext<GenerateListContext, GenerateEditContext> newContext(String moduleName, String documentName) {
		return new AutomationContext<>() {
			{
				setModuleName(moduleName);
				setDocumentName(documentName);
			}

			@Override
			public void generate(GenerateListContext listContext) {
				// no-op
			}

			@Override
			public void generate(GenerateEditContext editContext) {
				// no-op
			}
		};
	}
}
