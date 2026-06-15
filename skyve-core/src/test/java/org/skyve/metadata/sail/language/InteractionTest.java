package org.skyve.metadata.sail.language;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.sail.execution.Executor;

class InteractionTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultStepsListIsNotNull() {
		assertThat(new Interaction().getSteps(), is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetName() {
		Interaction i = new Interaction();
		i.setName("myInteraction");
		assertThat(i.getName(), is("myInteraction"));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultBeforeIsNull() {
		assertThat(new Interaction().getBefore(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetBefore() {
		Interaction i = new Interaction();
		Procedure before = new Procedure();
		i.setBefore(before);
		assertThat(i.getBefore(), is(before));
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultAfterIsNull() {
		assertThat(new Interaction().getAfter(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetAfter() {
		Interaction i = new Interaction();
		Procedure after = new Procedure();
		i.setAfter(after);
		assertThat(i.getAfter(), is(after));
	}

	@Test
	@SuppressWarnings("static-method")
	void executeCallsExecuteInteraction() {
		Interaction i = new Interaction();
		Executor executor = mock(Executor.class);
		i.execute(executor);
		verify(executor).executeInteraction(i);
	}
}
