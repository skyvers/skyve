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

class CommentTest {

	private Comment step;

	@BeforeEach
	void setUp() {
		step = new Comment();
	}

	@Test
	void defaultCommentIsNull() {
		assertThat(step.getComment(), is(nullValue()));
	}

	@Test
	void setCommentRoundtrip() {
		step.setComment("This is a comment");
		assertThat(step.getComment(), is("This is a comment"));
	}

	@Test
	void setCommentTrimsWhitespace() {
		step.setComment("  hello  ");
		assertThat(step.getComment(), is("hello"));
	}

	@Test
	void setCommentBlankBecomesNull() {
		step.setComment("   ");
		assertThat(step.getComment(), is(nullValue()));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeComment(step);
	}

	@Test
	@SuppressWarnings("static-method")
	void getIdentifierReturnsCommentText() {
		Comment s = new Comment();
		s.setComment("note");
		AutomationContext<?, ?> ctx = mock(AutomationContext.class);
		assertThat(s.getIdentifier(ctx), is("Comment note"));
	}
}
