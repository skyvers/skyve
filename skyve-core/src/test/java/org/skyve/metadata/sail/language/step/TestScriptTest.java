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

class TestScriptTest {

	private TestScript step;

	@BeforeEach
	void setUp() {
		step = new TestScript();
	}

	@Test
	void defaultsAreNull() {
		assertAll(() -> assertThat(step.getScript(), is(nullValue())),
					() -> assertThat(step.getTimeoutMillis(), is(nullValue())),
					() -> assertThat(step.getMessage(), is(nullValue())));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setScript("  await null;\n  return 1;  ");
		step.setTimeoutMillis(Integer.valueOf(250));
		step.setMessage(" made {run} ");
		assertAll(() -> assertThat(step.getScript(), is("await null;\n  return 1;")),
					() -> assertThat(step.getTimeoutMillis(), is(Integer.valueOf(250))),
					() -> assertThat(step.getMessage(), is("made {run}")));
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
		verify(executor).executeTestScript(step);
	}

	@Test
	void executeRejectsAnEmptyScriptOrANegativeTimeout() {
		Executor executor = mock(Executor.class);
		MetaDataException empty = assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setScript("return true;");
		step.setTimeoutMillis(Integer.valueOf(-1));
		MetaDataException negative = assertThrows(MetaDataException.class, () -> step.execute(executor));
		assertAll(() -> assertThat(empty.getMessage(), is("<testScript /> has no script")),
					() -> assertThat(negative.getMessage(), is("<testScript /> [timeoutMillis] cannot be negative")));
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
		assertAll(() -> assertThat(empty, is("testScript ")),
					() -> assertThat(firstLine, is("testScript return document.readyState === 'complete';")),
					() -> assertThat(truncated, is("testScript return 'a very long first line that goes on for more than si")));
	}
}
