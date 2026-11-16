package org.skyve.metadata.sail.language.step;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.step.TestMessage.MessageType;

class TestMessageTest {

	private TestMessage step;

	@BeforeEach
	void setUp() {
		step = new TestMessage();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getContains(), is(nullValue()));
		assertThat(step.getType(), is(nullValue()));
	}

	@Test
	void settersRoundtrip() {
		step.setContains(" Backup started ");
		step.setType(MessageType.warning);
		assertThat(step.getContains(), is("Backup started"));
		assertThat(step.getType(), is(MessageType.warning));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeTestMessage(step);
	}

	@Test
	void getIdentifierReturnsMessage() {
		assertThat(step.getIdentifier(mock(AutomationContext.class)), is("Message"));
	}
}
