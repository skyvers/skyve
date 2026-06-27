package org.skyve.metadata.sail.language.step.interaction.navigation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class NavigateEditTest {

	private NavigateEdit step;

	@BeforeEach
	void setUp() {
		step = new NavigateEdit();
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
	void defaultBizIdIsNull() {
		assertThat(step.getBizId(), is(nullValue()));
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
	void setModuleNameBlankBecomesNull() {
		step.setModuleName("   ");
		assertThat(step.getModuleName(), is(nullValue()));
	}

	@Test
	void setDocumentNameRoundtrip() {
		step.setDocumentName("Contact");
		assertThat(step.getDocumentName(), is("Contact"));
	}

	@Test
	void setBizIdRoundtrip() {
		step.setBizId("abc-123");
		assertThat(step.getBizId(), is("abc-123"));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeNavigateEdit(step);
	}

	@Test
	void getIdentifierReturnsModuleDotDocument() {
		step.setModuleName("admin");
		step.setDocumentName("Contact");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("admin.Contact"));
	}
}
