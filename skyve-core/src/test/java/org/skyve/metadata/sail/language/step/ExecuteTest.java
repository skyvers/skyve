package org.skyve.metadata.sail.language.step;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;

class ExecuteTest {

	private Execute step;

	@BeforeEach
	void setUp() {
		step = new Execute();
	}

	@Test
	void defaultScriptIsNull() {
		assertThat(step.getScript(), is(nullValue()));
	}

	@Test
	void setScriptRoundtrip() {
		step.setScript("someScript()");
		assertThat(step.getScript(), is("someScript()"));
	}

	@Test
	void setScriptTrimsWhitespace() {
		step.setScript("  script  ");
		assertThat(step.getScript(), is("script"));
	}

	@Test
	void setScriptBlankBecomesNull() {
		step.setScript("  ");
		assertThat(step.getScript(), is(nullValue()));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setScript("someScript()");
		step.execute(executor);
		verify(executor).executeExecute(step);
	}

	@Test
	void defaultVariableIsNullAndSetVariableTrims() {
		assertThat(step.getVariable(), is(nullValue()));
		step.setVariable(" title ");
		assertThat(step.getVariable(), is("title"));
	}

	@Test
	void executeWithAVariableCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setScript("return document.title;");
		step.setVariable("title_2");
		step.execute(executor);
		verify(executor).executeExecute(step);
	}

	@Test
	void executeRejectsAnEmptyScriptOrABadVariable() {
		Executor executor = mock(Executor.class);
		MetaDataException empty = assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setScript("return 1;");
		step.setVariable("2nd");
		MetaDataException badVariable = assertThrows(MetaDataException.class, () -> step.execute(executor));
		assertAll(() -> assertThat(empty.getMessage(), is("<execute /> has no script")),
					() -> assertThat(badVariable.getMessage(), is("<execute /> [variable] 2nd is not a variable name: use a letter, then letters, digits or underscores")));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierReturnsNull() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is(nullValue()));
	}
}
