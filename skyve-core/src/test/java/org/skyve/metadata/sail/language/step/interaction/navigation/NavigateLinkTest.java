package org.skyve.metadata.sail.language.step.interaction.navigation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;

class NavigateLinkTest {

	private NavigateLink step;

	@BeforeEach
	void setUp() {
		step = new NavigateLink();
	}

	@Test
	void defaultHrefIsNull() {
		assertThat(step.getHref(), is(nullValue()));
	}

	@Test
	void setHrefRoundtrip() {
		step.setHref("someHref");
		assertThat(step.getHref(), is("someHref"));
	}

	@Test
	void setHrefTrimsWhitespace() {
		step.setHref("  someHref  ");
		assertThat(step.getHref(), is("someHref"));
	}

	@Test
	void setHrefBlankBecomesNull() {
		step.setHref("   ");
		assertThat(step.getHref(), is(nullValue()));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setHref("someHref");
		step.execute(executor);
		verify(executor).executeNavigateLink(step);
	}

	@Test
	void executeByTextCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setText("Open");
		step.execute(executor);
		verify(executor).executeNavigateLink(step);
	}

	@Test
	void executeNeedsExactlyOneOfHrefAndText() {
		Executor executor = mock(Executor.class);
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		step.setHref("someHref");
		step.setText("Open");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void setTextTrimsAndBlankBecomesNull() {
		step.setText("  Open  ");
		assertThat(step.getText(), is("Open"));
		step.setText("   ");
		assertThat(step.getText(), is(nullValue()));
	}

	@Test
	void getIdentifierReturnsTextWithoutHref() {
		step.setText("Open");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("Open"));
	}

	@Test
	void getIdentifierReturnsHref() {
		step.setHref("someHref");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(step.getIdentifier(ctx), is("someHref"));
	}
}
