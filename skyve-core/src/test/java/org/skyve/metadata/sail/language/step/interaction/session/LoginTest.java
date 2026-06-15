package org.skyve.metadata.sail.language.step.interaction.session;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.sail.execution.Executor;

class LoginTest {

	private Login step;

	@BeforeEach
	void setUp() {
		step = new Login();
	}

	@Test
	void defaultCustomerIsNull() {
		assertThat(step.getCustomer(), is(nullValue()));
	}

	@Test
	void defaultUserIsNull() {
		assertThat(step.getUser(), is(nullValue()));
	}

	@Test
	void defaultPasswordIsNull() {
		assertThat(step.getPassword(), is(nullValue()));
	}

	@Test
	void setCustomerRoundtrip() {
		step.setCustomer("defaultCustomer");
		assertThat(step.getCustomer(), is("defaultCustomer"));
	}

	@Test
	void setCustomerTrimsWhitespace() {
		step.setCustomer("  acme  ");
		assertThat(step.getCustomer(), is("acme"));
	}

	@Test
	void setCustomerBlankBecomesNull() {
		step.setCustomer("   ");
		assertThat(step.getCustomer(), is(nullValue()));
	}

	@Test
	void setUserRoundtrip() {
		step.setUser("admin");
		assertThat(step.getUser(), is("admin"));
	}

	@Test
	void setPasswordRoundtrip() {
		step.setPassword("secret");
		assertThat(step.getPassword(), is("secret"));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeLogin(step);
	}

	@Test
	void getIdentifierReturnsNull() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is(nullValue()));
	}
}
