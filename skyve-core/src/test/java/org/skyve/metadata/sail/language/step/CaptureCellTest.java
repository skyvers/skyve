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
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;

class CaptureCellTest {

	private CaptureCell step;

	@BeforeEach
	void setUp() {
		step = new CaptureCell();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getBinding(), is(nullValue()));
		assertThat(step.getModuleName(), is(nullValue()));
		assertThat(step.getDocumentName(), is(nullValue()));
		assertThat(step.getQueryName(), is(nullValue()));
		assertThat(step.getModelName(), is(nullValue()));
		assertThat(step.getRow(), is(nullValue()));
		assertThat(step.getRowContaining(), is(nullValue()));
		assertThat(step.getColumn(), is(nullValue()));
		assertThat(step.getVariable(), is(nullValue()));
		assertThat(step.getPattern(), is(nullValue()));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setModuleName(" todo ");
		step.setDocumentName(" ToDo ");
		step.setQueryName(" qToDos ");
		step.setModelName(" m ");
		step.setRowContaining(" SAIL {run} ");
		step.setColumn(" reference ");
		step.setVariable(" ref ");
		step.setPattern(" \\d+ ");
		assertThat(step.getModuleName(), is("todo"));
		assertThat(step.getDocumentName(), is("ToDo"));
		assertThat(step.getQueryName(), is("qToDos"));
		assertThat(step.getModelName(), is("m"));
		assertThat(step.getRowContaining(), is("SAIL {run}"));
		assertThat(step.getColumn(), is("reference"));
		assertThat(step.getVariable(), is("ref"));
		assertThat(step.getPattern(), is("\\d+"));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setQueryName("qToDos");
		step.setRow(Integer.valueOf(0));
		step.setColumn("reference");
		step.setVariable("ref");
		step.execute(executor);
		verify(executor).executeCaptureCell(step);
	}

	@Test
	void executeNeedsExactlyOneOfRowAndRowContaining() {
		Executor executor = mock(Executor.class);
		step.setBinding("rows");
		step.setColumn("name");
		step.setVariable("v");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setRow(Integer.valueOf(1));
		step.setRowContaining("x");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeRejectsABindingWithListAttributes() {
		Executor executor = mock(Executor.class);
		step.setBinding("rows");
		step.setQueryName("q");
		step.setRow(Integer.valueOf(0));
		step.setColumn("name");
		step.setVariable("v");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeRejectsABadVariableOrPattern() {
		Executor executor = mock(Executor.class);
		step.setBinding("rows");
		step.setRow(Integer.valueOf(0));
		step.setColumn("name");
		step.setVariable("a-b");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setVariable("ab");
		step.setPattern("(");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierIsTheBindingOrTheListGrid() {
		AutomationContext<?, ?> context = mock(AutomationContext.class);
		when(context.getModuleName()).thenReturn("todo");
		when(context.getDocumentName()).thenReturn("ToDo");
		step.setBinding("rows");
		assertThat(step.getIdentifier(context), is("rows"));
		step.setBinding(null);
		step.setQueryName("qToDos");
		assertThat(step.getIdentifier(context), is("todo.qToDos"));
	}
}
