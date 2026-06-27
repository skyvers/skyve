package org.skyve.metadata.sail.language.step.interaction.navigation;

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

class NavigateListTest {

	private NavigateList step;

	@BeforeEach
	void setUp() {
		step = new NavigateList();
	}

	@Test
	void defaultModuleNameIsNull() {
		assertThat(step.getModuleName(), is(nullValue()));
	}

	@Test
	void defaultDocumentNameIsNull() {
		assertThat(step.getDocumentName(), is(nullValue()));
	}

	@Test
	void defaultQueryNameIsNull() {
		assertThat(step.getQueryName(), is(nullValue()));
	}

	@Test
	void defaultModelNameIsNull() {
		assertThat(step.getModelName(), is(nullValue()));
	}

	@Test
	void setModuleNameRoundtrip() {
		step.setModuleName("admin");
		assertThat(step.getModuleName(), is("admin"));
	}

	@Test
	void setModuleNameTrimsWhitespace() {
		step.setModuleName("  admin  ");
		assertThat(step.getModuleName(), is("admin"));
	}

	@Test
	void setDocumentNameRoundtrip() {
		step.setDocumentName("Contact");
		assertThat(step.getDocumentName(), is("Contact"));
	}

	@Test
	void setQueryNameRoundtrip() {
		step.setQueryName("qContacts");
		assertThat(step.getQueryName(), is("qContacts"));
	}

	@Test
	void setModelNameRoundtrip() {
		step.setModelName("myModel");
		assertThat(step.getModelName(), is("myModel"));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeNavigateList(step);
	}

	@Test
	void getIdentifierWithQueryName() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		step.setModuleName("admin");
		step.setQueryName("qContacts");
		assertThat(step.getIdentifier(ctx), is("admin.qContacts"));
	}

	@Test
	void getIdentifierWithModelName() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		step.setModuleName("admin");
		step.setDocumentName("Contact");
		step.setModelName("myModel");
		assertThat(step.getIdentifier(ctx), is("admin.Contact.myModel"));
	}

	@Test
	void getIdentifierWithDocumentFallback() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		step.setModuleName("admin");
		step.setDocumentName("Contact");
		assertThat(step.getIdentifier(ctx), is("admin.Contact"));
	}

	@Test
	void getIdentifierFallsBackToContext() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		// no module/document set — falls back to context
		assertThat(step.getIdentifier(ctx), is("admin.Contact"));
	}

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
