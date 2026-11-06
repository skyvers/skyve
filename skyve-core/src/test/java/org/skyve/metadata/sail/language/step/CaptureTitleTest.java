package org.skyve.metadata.sail.language.step;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;

class CaptureTitleTest {

	private CaptureTitle step;

	@BeforeEach
	void setUp() {
		step = new CaptureTitle();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getVariable(), is(nullValue()));
		assertThat(step.getPattern(), is(nullValue()));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setVariable(" request ");
		step.setPattern(" Request (\\d+) ");
		assertThat(step.getVariable(), is("request"));
		assertThat(step.getPattern(), is("Request (\\d+)"));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setVariable("request");
		step.execute(executor);
		verify(executor).executeCaptureTitle(step);
	}

	@Test
	void executeRejectsABadVariableOrPattern() {
		Executor executor = mock(Executor.class);
		step.setVariable("2nd");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setVariable("ok");
		step.setPattern("[");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierReturnsTheVariable() {
		step.setVariable("request");
		assertThat(step.getIdentifier(mock(AutomationContext.class)), is("request"));
	}
}
