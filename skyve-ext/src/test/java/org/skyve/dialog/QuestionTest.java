package org.skyve.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.dialog.Question.Button;
import org.skyve.domain.messages.MessageException;

/**
 * Tests the question presets, default buttons, aborting buttons and the question exception.
 */
@SuppressWarnings("static-method")
class QuestionTest {
	@Test
	void okCancelDefaultsToCancelThatAnswers() {
		Question question = Question.okCancel("key", "title", "message");

		assertEquals("key", question.getKey());
		assertEquals("title", question.getTitle());
		assertEquals("message", question.getMessage());
		assertEquals(List.of(Button.OK, Button.CANCEL), question.getButtons());
		assertEquals(Button.CANCEL, question.getDefaultButton());
		assertFalse(question.aborts(Button.OK));
		assertFalse(question.aborts(Button.CANCEL));
	}

	@Test
	void yesNoDefaultsToNoThatAnswers() {
		Question question = Question.yesNo("key", "title", "message");

		assertEquals(List.of(Button.YES, Button.NO), question.getButtons());
		assertEquals(Button.NO, question.getDefaultButton());
		assertFalse(question.aborts(Button.NO));
	}

	@Test
	void theDefaultButtonCanBeThePositiveButton() {
		Question question = Question.okCancel("key", "title", "message");

		assertSame(question, question.defaultButton(Button.OK));
		assertEquals(Button.OK, question.getDefaultButton());
	}

	@Test
	void theDefaultButtonCanBeTheNegativeButton() {
		Question question = Question.yesNo("key", "title", "message").defaultButton(Button.YES);

		assertSame(question, question.defaultButton(Button.NO));
		assertEquals(Button.NO, question.getDefaultButton());
	}

	@Test
	void theDefaultButtonMustBeOneOfTheQuestionsButtons() {
		Question question = Question.okCancel("key", "title", "message");

		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> question.defaultButton(Button.YES));
		assertTrue(e.getMessage().contains("key"), e.getMessage());
		assertEquals(Button.CANCEL, question.getDefaultButton());
	}

	@Test
	void cancelAbortsMakesOnlyTheNegativeButtonAbort() {
		Question okCancel = Question.okCancel("key", "title", "message");
		Question yesNo = Question.yesNo("key", "title", "message").cancelAborts();

		assertSame(okCancel, okCancel.cancelAborts());
		assertTrue(okCancel.aborts(Button.CANCEL));
		assertFalse(okCancel.aborts(Button.OK));
		assertTrue(yesNo.aborts(Button.NO));
		assertFalse(yesNo.aborts(Button.YES));
		assertFalse(yesNo.aborts(Button.CANCEL));
	}

	@Test
	void theExceptionCarriesTheQuestionAndItsIdAndIsNotAMessageException() {
		Question question = Question.okCancel("key", "title", "message");

		QuestionException e = new QuestionException(question, "id");

		assertSame(question, e.getQuestion());
		assertEquals("id", e.getId());
		assertTrue(e.getMessage().contains("key"), e.getMessage());
		assertFalse(MessageException.class.isInstance(e));
	}
}
