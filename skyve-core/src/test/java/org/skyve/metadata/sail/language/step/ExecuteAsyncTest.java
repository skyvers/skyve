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

class ExecuteAsyncTest {

	private ExecuteAsync step;

	@BeforeEach
	void setUp() {
		step = new ExecuteAsync();
	}

	@Test
	void defaultsAreNull() {
		assertAll(() -> assertThat(step.getScript(), is(nullValue())),
					() -> assertThat(step.getTimeoutMillis(), is(nullValue())),
					() -> assertThat(step.getVariable(), is(nullValue())));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setScript("  await null;\n  return 1;  ");
		step.setTimeoutMillis(Integer.valueOf(250));
		step.setVariable(" status ");
		assertAll(() -> assertThat(step.getScript(), is("await null;\n  return 1;")),
					() -> assertThat(step.getTimeoutMillis(), is(Integer.valueOf(250))),
					() -> assertThat(step.getVariable(), is("status")));
	}

	@Test
	void aBlankScriptBecomesNull() {
		step.setScript(" \n ");
		assertThat(step.getScript(), is(nullValue()));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setScript("return true;");
		step.setTimeoutMillis(Integer.valueOf(0));
		step.execute(executor);
		verify(executor).executeExecuteAsync(step);
	}

	@Test
	void executeRejectsAnEmptyScriptOrANegativeTimeoutOrABadVariable() {
		Executor executor = mock(Executor.class);
		MetaDataException empty = assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setScript("return true;");
		step.setTimeoutMillis(Integer.valueOf(-1));
		MetaDataException negative = assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setTimeoutMillis(null);
		step.setVariable("a-b");
		MetaDataException badVariable = assertThrows(MetaDataException.class, () -> step.execute(executor));
		assertAll(() -> assertThat(empty.getMessage(), is("<executeAsync /> has no script")),
					() -> assertThat(negative.getMessage(), is("<executeAsync /> [timeoutMillis] cannot be negative")),
					() -> assertThat(badVariable.getMessage(), is("<executeAsync /> [variable] a-b is not a variable name: use a letter, then letters, digits or underscores")));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierIsTheElementAndTheScriptsFirstLine() {
		AutomationContext<?, ?> context = mock(AutomationContext.class);
		String empty = step.getIdentifier(context);
		step.setScript("return document.readyState === 'complete';\nreturn false;");
		String firstLine = step.getIdentifier(context);
		step.setScript("return 'a very long first line that goes on for more than sixty characters';\nx");
		String truncated = step.getIdentifier(context);
		assertAll(() -> assertThat(empty, is("executeAsync ")),
					() -> assertThat(firstLine, is("executeAsync return document.readyState === 'complete';")),
					() -> assertThat(truncated, is("executeAsync return 'a very long first line that goes on for more than si")));
	}
}
