package org.skyve.metadata.sail.language.step.interaction.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class ActionTest {

	private Action step;

	@BeforeEach
	void setUp() {
		step = new Action();
	}

	// ---- AbstractAction testSuccess ----

	@Test
	void defaultTestSuccessIsNull() {
		assertThat(step.getTestSuccess(), is(nullValue()));
	}

	@Test
	void setTestSuccessRoundtrip() {
		step.setTestSuccess(Boolean.FALSE);
		assertThat(step.getTestSuccess(), is(Boolean.FALSE));
	}

	// ---- Action fields ----

	@Test
	void defaultActionNameIsNull() {
		assertThat(step.getActionName(), is(nullValue()));
	}

	@Test
	void defaultConfirmIsNull() {
		assertThat(step.getConfirm(), is(nullValue()));
	}

	@Test
	void setActionNameRoundtrip() {
		step.setActionName("BizImport");
		assertThat(step.getActionName(), is("BizImport"));
	}

	@Test
	void setActionNameTrimsWhitespace() {
		step.setActionName("  BizExport  ");
		assertThat(step.getActionName(), is("BizExport"));
	}

	@Test
	void setActionNameBlankBecomesNull() {
		step.setActionName("   ");
		assertThat(step.getActionName(), is(nullValue()));
	}

	@Test
	void setConfirmRoundtrip() {
		step.setConfirm(Boolean.TRUE);
		assertThat(step.getConfirm(), is(Boolean.TRUE));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeAction(step);
	}

	@Test
	void getIdentifierReturnsActionName() {
		step.setActionName("BizReport");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("BizReport"));
	}
}
