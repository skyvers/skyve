package org.skyve.metadata.sail.language.step.context;

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
import org.skyve.web.UserAgentType;

class PushListContextTest {

	private PushListContext step;

	@BeforeEach
	void setUp() {
		step = new PushListContext();
	}

	@Test
	void defaultUxuiIsNull() {
		assertThat(step.getUxui(), is(nullValue()));
	}

	@Test
	void defaultUserAgentTypeIsNull() {
		assertThat(step.getUserAgentType(), is(nullValue()));
	}

	@Test
	void setUxuiRoundtrip() {
		step.setUxui("desktop");
		assertThat(step.getUxui(), is("desktop"));
	}

	@Test
	void setUserAgentTypeRoundtrip() {
		step.setUserAgentType(UserAgentType.desktop);
		assertThat(step.getUserAgentType(), is(UserAgentType.desktop));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executePushListContext(step);
	}

	@Test
	void getIdentifierWithQueryName() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		step.setModuleName("admin");
		step.setQueryName("qContacts");
		assertThat(step.getIdentifier(ctx), is("admin.qContacts"));
	}

	@Test
	void getIdentifierWithDocumentFallback() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext("admin", "Contact");
		step.setModuleName("admin");
		step.setDocumentName("Contact");
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
