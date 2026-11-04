package org.skyve.metadata.sail.language.step;

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

class TestGridRowsTest {

	private TestGridRows step;

	@BeforeEach
	void setUp() {
		step = new TestGridRows();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getModuleName(), is(nullValue()));
		assertThat(step.getDocumentName(), is(nullValue()));
		assertThat(step.getQueryName(), is(nullValue()));
		assertThat(step.getModelName(), is(nullValue()));
		assertThat(step.getBinding(), is(nullValue()));
		assertThat(step.getRowContaining(), is(nullValue()));
		assertThat(step.getMinRows(), is(nullValue()));
		assertThat(step.getMaxRows(), is(nullValue()));
		assertThat(step.getTimeoutMillis(), is(nullValue()));
	}

	@Test
	void settersRoundtrip() {
		step.setModuleName(" admin ");
		step.setDocumentName(" User ");
		step.setQueryName(" qUsers ");
		step.setModelName(" model ");
		step.setBinding(" roles ");
		step.setRowContaining(" jane{run} ");
		step.setMinRows(Integer.valueOf(1));
		step.setMaxRows(Integer.valueOf(2));
		step.setTimeoutMillis(Integer.valueOf(3000));
		assertThat(step.getModuleName(), is("admin"));
		assertThat(step.getDocumentName(), is("User"));
		assertThat(step.getQueryName(), is("qUsers"));
		assertThat(step.getModelName(), is("model"));
		assertThat(step.getBinding(), is("roles"));
		assertThat(step.getRowContaining(), is("jane{run}"));
		assertThat(step.getMinRows(), is(Integer.valueOf(1)));
		assertThat(step.getMaxRows(), is(Integer.valueOf(2)));
		assertThat(step.getTimeoutMillis(), is(Integer.valueOf(3000)));
	}

	@Test
	void effectiveMinRowsDefaultsToOneWithoutBounds() {
		assertThat(Integer.valueOf(step.getEffectiveMinRows()), is(Integer.valueOf(1)));
	}

	@Test
	void effectiveMinRowsDefaultsToZeroWithMaxRows() {
		step.setMaxRows(Integer.valueOf(0));
		assertThat(Integer.valueOf(step.getEffectiveMinRows()), is(Integer.valueOf(0)));
	}

	@Test
	void effectiveMinRowsUsesMinRows() {
		step.setMinRows(Integer.valueOf(3));
		assertThat(Integer.valueOf(step.getEffectiveMinRows()), is(Integer.valueOf(3)));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setQueryName("qUsers");
		step.setMinRows(Integer.valueOf(1));
		step.setMaxRows(Integer.valueOf(1));
		step.execute(executor);
		verify(executor).executeTestGridRows(step);
	}

	@Test
	void executeBindingWithListAttributeThrows() {
		Executor executor = mock(Executor.class);
		step.setBinding("roles");
		step.setDocumentName("User");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeNegativeCountThrows() {
		Executor executor = mock(Executor.class);
		step.setMaxRows(Integer.valueOf(-1));
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeNegativeTimeoutThrows() {
		Executor executor = mock(Executor.class);
		step.setTimeoutMillis(Integer.valueOf(-1));
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeMinAboveMaxThrows() {
		Executor executor = mock(Executor.class);
		step.setMinRows(Integer.valueOf(2));
		step.setMaxRows(Integer.valueOf(1));
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierReturnsBinding() {
		step.setBinding("roles");
		assertThat(step.getIdentifier(mock(AutomationContext.class)), is("roles"));
	}

	@Test
	@SuppressWarnings("unchecked")
	void getIdentifierReturnsListGridIdentifier() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = mock(AutomationContext.class);
		when(ctx.getModuleName()).thenReturn("admin");
		when(ctx.getDocumentName()).thenReturn("User");
		assertThat(step.getIdentifier(ctx), is("admin.User"));
		step.setQueryName("qUsers");
		assertThat(step.getIdentifier(ctx), is("admin.qUsers"));
	}
}
