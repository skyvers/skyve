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

class TestWidgetTest {

	private TestWidget step;

	@BeforeEach
	void setUp() {
		step = new TestWidget();
	}

	@Test
	void defaultsAreNullAndVisibleIsExpected() {
		assertThat(step.getBinding(), is(nullValue()));
		assertThat(step.getActionName(), is(nullValue()));
		assertThat(step.getTabPath(), is(nullValue()));
		assertThat(step.getVisible(), is(nullValue()));
		assertThat(step.getEnabled(), is(nullValue()));
		assertThat(Boolean.valueOf(step.isExpectedVisible()), is(Boolean.TRUE));
	}

	@Test
	void visibleFalseIsNotExpectedVisible() {
		step.setVisible(Boolean.FALSE);
		assertThat(Boolean.valueOf(step.isExpectedVisible()), is(Boolean.FALSE));
	}

	@Test
	void settersRoundtrip() {
		step.setBinding(" name ");
		step.setActionName(" Restore ");
		step.setTabPath(" Backup/Restore ");
		step.setVisible(Boolean.TRUE);
		step.setEnabled(Boolean.FALSE);
		assertThat(step.getBinding(), is("name"));
		assertThat(step.getActionName(), is("Restore"));
		assertThat(step.getTabPath(), is("Backup/Restore"));
		assertThat(step.getVisible(), is(Boolean.TRUE));
		assertThat(step.getEnabled(), is(Boolean.FALSE));
	}

	@Test
	void executeWithOneTargetCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setTabPath("Backup");
		step.setEnabled(Boolean.TRUE);
		step.execute(executor);
		verify(executor).executeTestWidget(step);
	}

	@Test
	void executeWithoutTargetThrows() {
		Executor executor = mock(Executor.class);
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeWithTwoTargetsThrows() {
		Executor executor = mock(Executor.class);
		step.setBinding("name");
		step.setActionName("Restore");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeInvisibleAndEnabledThrows() {
		Executor executor = mock(Executor.class);
		step.setBinding("name");
		step.setVisible(Boolean.FALSE);
		step.setEnabled(Boolean.FALSE);
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierMatchesTheOtherSteps() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		step.setTabPath("Backup");
		assertThat(step.getIdentifier(ctx), is("Backup Tab"));
		step.setActionName("Restore");
		assertThat(step.getIdentifier(ctx), is("Restore"));
		step.setBinding("name");
		assertThat(step.getIdentifier(ctx), is("name"));
	}
}
