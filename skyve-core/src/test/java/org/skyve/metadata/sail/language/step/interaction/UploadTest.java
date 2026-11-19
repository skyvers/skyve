package org.skyve.metadata.sail.language.step.interaction;

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

class UploadTest {

	private Upload step;

	@BeforeEach
	void setUp() {
		step = new Upload();
	}

	@Test
	void defaultsAreNull() {
		assertThat(step.getBinding(), is(nullValue()));
		assertThat(step.getActionName(), is(nullValue()));
		assertThat(step.getFile(), is(nullValue()));
	}

	@Test
	void settersTrimAndRoundtrip() {
		step.setBinding(" content ");
		step.setActionName(" ImportAction ");
		step.setFile(" sail/files/logo.png ");
		assertThat(step.getBinding(), is("content"));
		assertThat(step.getActionName(), is("ImportAction"));
		assertThat(step.getFile(), is("sail/files/logo.png"));
	}

	@Test
	void executeWithBindingCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setBinding("content");
		step.execute(executor);
		verify(executor).executeUpload(step);
	}

	@Test
	void executeWithActionCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.setActionName("ImportAction");
		step.execute(executor);
		verify(executor).executeUpload(step);
	}

	@Test
	void executeWithoutTargetThrows() {
		Executor executor = mock(Executor.class);
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void executeWithBothTargetsThrows() {
		Executor executor = mock(Executor.class);
		step.setBinding("content");
		step.setActionName("ImportAction");
		assertThrows(MetaDataException.class, () -> step.execute(executor));
		verifyNoInteractions(executor);
	}

	@Test
	void getIdentifierReturnsBindingOrAction() {
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		step.setActionName("ImportAction");
		assertThat(step.getIdentifier(ctx), is("ImportAction"));
		step.setBinding("content");
		assertThat(step.getIdentifier(ctx), is("content"));
	}

	@Test
	void testSuccessDefaultsToNullAndRoundtrips() {
		assertThat(step.getTestSuccess(), is(nullValue()));
		step.setTestSuccess(Boolean.FALSE);
		assertThat(step.getTestSuccess(), is(Boolean.FALSE));
	}
}
