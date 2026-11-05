package org.skyve.metadata.sail.language.step;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;

class CaptureValueTest {

	private CaptureValue step;

	@BeforeEach
	void setUp() {
		step = new CaptureValue();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getBinding(), is(nullValue()));
		assertThat(step.getVariable(), is(nullValue()));
	}

	@Test
	void settersRoundtripAndTrim() {
		step.setBinding(" invoiceNumber ");
		step.setVariable(" invoice ");
		assertThat(step.getBinding(), is("invoiceNumber"));
		assertThat(step.getVariable(), is("invoice"));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setBinding("invoiceNumber");
		step.setVariable("invoice_2");
		step.execute(executor);
		verify(executor).executeCaptureValue(step);
	}

	@Test
	@SuppressWarnings("static-method")
	void executeRejectsVariableNamesThatAreNotIdentifiers() {
		assertAll(Stream.of("2nd", "my-var", "a.b", "a b", "{run}", "_x").map(name -> () -> {
			CaptureValue capture = new CaptureValue();
			capture.setBinding("invoiceNumber");
			capture.setVariable(name);
			Executor executor = mock(Executor.class);
			assertThrows(MetaDataException.class, () -> capture.execute(executor), name);
			verifyNoInteractions(executor);
		}));
	}

	@Test
	void executeWithoutVariableThrows() {
		Executor executor = mock(Executor.class);
		step.setBinding("invoiceNumber");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierReturnsBinding() {
		step.setBinding("invoiceNumber");
		assertThat(step.getIdentifier(mock(AutomationContext.class)), is("invoiceNumber"));
	}
}
