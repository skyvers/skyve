package org.skyve.metadata.sail.language.step;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;

class CallTest {

	private Call step;

	@BeforeEach
	void setUp() {
		step = new Call();
	}

	@Test
	void defaultsAreNullOrEmpty() {
		assertAll(() -> assertThat(step.getClassName(), is(nullValue())),
					() -> assertThat(step.getVariable(), is(nullValue())),
					() -> assertTrue(step.getParameters().isEmpty()));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setClassName(" sail.kitchenSink.EchoCall ");
		step.setVariable(" echo ");
		CallParameter parameter = parameter(" text ", " made {run} ");
		step.getParameters().add(parameter);
		assertAll(() -> assertThat(step.getClassName(), is("sail.kitchenSink.EchoCall")),
					() -> assertThat(step.getVariable(), is("echo")),
					() -> assertThat(step.getParameters().get(0), is(parameter)),
					() -> assertThat(parameter.getName(), is("text")),
					() -> assertThat(parameter.getValue(), is("made {run}")));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setClassName("sail.kitchenSink.EchoCall");
		step.setVariable("echo");
		step.getParameters().add(parameter("text", "a"));
		step.getParameters().add(parameter("fail", "assert"));
		step.execute(executor);
		verify(executor).executeCall(step);
	}

	@Test
	void executeRejectsAMissingClassNameABadParameterOrABadVariable() {
		Executor executor = mock(Executor.class);
		MetaDataException noClass = assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setClassName("C");
		step.getParameters().add(parameter(" ", "v"));
		MetaDataException noName = assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.getParameters().set(0, parameter("n", ""));
		MetaDataException noValue = assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.getParameters().set(0, parameter("n", "1"));
		step.getParameters().add(parameter("n", "2"));
		MetaDataException twice = assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.getParameters().remove(1);
		step.setVariable("1st");
		MetaDataException badVariable = assertThrows(MetaDataException.class, () -> step.execute(executor));
		assertAll(() -> assertThat(noClass.getMessage(), is("<call /> has no [className]")),
					() -> assertThat(noName.getMessage(), is("<call className=\"C\" /> has a <parameter /> without a [name] or [value]")),
					() -> assertThat(noValue.getMessage(), is("<call className=\"C\" /> has a <parameter /> without a [name] or [value]")),
					() -> assertThat(twice.getMessage(), is("<call className=\"C\" /> has more than one <parameter name=\"n\" />")),
					() -> assertThat(badVariable.getMessage(), is("<call /> [variable] 1st is not a variable name: use a letter, then letters, digits or underscores")));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierIsTheElementAndTheClassName() {
		step.setClassName("sail.kitchenSink.EchoCall");
		assertThat(step.getIdentifier(mock(AutomationContext.class)), is("call sail.kitchenSink.EchoCall"));
	}

	private static CallParameter parameter(String name, String value) {
		CallParameter result = new CallParameter();
		result.setName(name);
		result.setValue(value);
		return result;
	}
}
