package org.skyve.metadata.sail.language.step.context;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.web.UserAgentType;

class PushEditContextTest {

	private PushEditContext step;

	@BeforeEach
	void setUp() {
		step = new PushEditContext();
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
	void defaultCreateViewIsNull() {
		assertThat(step.getCreateView(), is(nullValue()));
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
	void setCreateViewRoundtrip() {
		step.setCreateView(Boolean.TRUE);
		assertThat(step.getCreateView(), is(Boolean.TRUE));
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
		verify(executor).executePushEditContext(step);
	}

	@Test
	void getIdentifierReturnsDotSeparated() {
		step.setModuleName("admin");
		step.setDocumentName("Contact");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("admin.Contact"));
	}
}
