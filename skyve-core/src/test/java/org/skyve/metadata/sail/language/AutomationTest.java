package org.skyve.metadata.sail.language;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.web.UserAgentType;

class AutomationTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultInteractionsListIsNotNull() {
		assertThat(new Automation().getInteractions(), is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetUxui() {
		Automation a = new Automation();
		a.setUxui("desktop");
		assertThat(a.getUxui(), is("desktop"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetUserAgentType() {
		Automation a = new Automation();
		a.setUserAgentType(UserAgentType.desktop);
		assertThat(a.getUserAgentType(), is(UserAgentType.desktop));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetTestStrategy() {
		Automation a = new Automation();
		a.setTestStrategy(TestStrategy.Assert);
		assertThat(a.getTestStrategy(), is(TestStrategy.Assert));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetBefore() {
		Automation a = new Automation();
		Procedure before = new Procedure();
		a.setBefore(before);
		assertThat(a.getBefore(), is(before));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultBeforeIsNull() {
		assertThat(new Automation().getBefore(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetAfter() {
		Automation a = new Automation();
		Procedure after = new Procedure();
		a.setAfter(after);
		assertThat(a.getAfter(), is(after));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultAfterIsNull() {
		assertThat(new Automation().getAfter(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void executeCallsExecuteAutomation() {
		Automation a = new Automation();
		Executor executor = mock(Executor.class);
		a.execute(executor);
		verify(executor).executeAutomation(a);
	}
}
