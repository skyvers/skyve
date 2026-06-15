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

class ListGridZoomTest {

	private ListGridZoom zoom;

	@BeforeEach
	void setUp() {
		zoom = new ListGridZoom();
	}

	// ---- default state ----

	@Test
	void defaultModuleNameIsNull() {
		assertThat(zoom.getModuleName(), is(nullValue()));
	}

	@Test
	void defaultDocumentNameIsNull() {
		assertThat(zoom.getDocumentName(), is(nullValue()));
	}

	@Test
	void defaultQueryNameIsNull() {
		assertThat(zoom.getQueryName(), is(nullValue()));
	}

	@Test
	void defaultModelNameIsNull() {
		assertThat(zoom.getModelName(), is(nullValue()));
	}

	@Test
	void defaultRowIsNull() {
		assertThat(zoom.getRow(), is(nullValue()));
	}

	// ---- setters / getters ----

	@Test
	void setModuleNameRoundtrip() {
		zoom.setModuleName("admin");
		assertThat(zoom.getModuleName(), is("admin"));
	}

	@Test
	void setModuleNameTrimsWhitespace() {
		zoom.setModuleName("  admin  ");
		assertThat(zoom.getModuleName(), is("admin"));
	}

	@Test
	void setModuleNameBlankBecomesNull() {
		zoom.setModuleName("   ");
		assertThat(zoom.getModuleName(), is(nullValue()));
	}

	@Test
	void setDocumentNameRoundtrip() {
		zoom.setDocumentName("Contact");
		assertThat(zoom.getDocumentName(), is("Contact"));
	}

	@Test
	void setQueryNameRoundtrip() {
		zoom.setQueryName("qContacts");
		assertThat(zoom.getQueryName(), is("qContacts"));
	}

	@Test
	void setModelNameRoundtrip() {
		zoom.setModelName("myModel");
		assertThat(zoom.getModelName(), is("myModel"));
	}

	@Test
	void setRowRoundtrip() {
		zoom.setRow(Integer.valueOf(3));
		assertThat(zoom.getRow(), is(Integer.valueOf(3)));
	}

	// ---- execute ----

	@Test
	void executeCallsExecutorListGridZoom() {
		Executor executor = mock(Executor.class);
		zoom.execute(executor);
		verify(executor).executeListGridZoom(zoom);
	}

	// ---- getIdentifier with queryName ----

	@Test
	void getIdentifierWithQueryName() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		zoom.setModuleName("admin");
		zoom.setQueryName("qContacts");
		String id = zoom.getIdentifier(ctx);
		assertThat(id, is("admin.qContacts.zoom"));
	}

	// ---- getIdentifier with modelName ----

	@Test
	void getIdentifierWithModelName() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		zoom.setModuleName("admin");
		zoom.setDocumentName("Contact");
		zoom.setModelName("myModel");
		String id = zoom.getIdentifier(ctx);
		assertThat(id, is("admin.Contact.myModel.zoom"));
	}

	// ---- getIdentifier fallback to document ----

	@Test
	void getIdentifierFallsBackToDocument() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		zoom.setModuleName("admin");
		zoom.setDocumentName("Contact");
		String id = zoom.getIdentifier(ctx);
		assertThat(id, is("admin.Contact.zoom"));
	}

	// ---- getIdentifier uses context when module/doc null ----

	@Test
	void getIdentifierUsesContextModuleAndDocumentWhenNull() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("security", "User");
		String id = zoom.getIdentifier(ctx);
		assertThat(id, is("security.User.zoom"));
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
