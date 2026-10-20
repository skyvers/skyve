package modules.kitchensink.DialogsFixture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.dialog.Dialogs;
import org.skyve.dialog.Question;
import org.skyve.dialog.QuestionException;
import org.skyve.impl.dialog.DialogsProtocol;
import org.skyve.impl.dialog.DialogsStaticSingleton;
import org.skyve.impl.web.WebContainer;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.web.WebContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import modules.kitchensink.DialogsFixture.actions.AskOnce;
import modules.kitchensink.DialogsFixture.actions.AskTwice;
import modules.kitchensink.DialogsFixtureRow.DialogsFixtureRowBizlet;
import modules.kitchensink.domain.DialogsFixture;
import modules.kitchensink.domain.DialogsFixtureRow;
import modules.test.AbstractSkyveTest;
import util.TestDialogs;

/**
 * Asks the dialogs fixture's questions without a renderer: from a job, from persistence and through the test answers.
 */
class DialogsFixtureH2Test extends AbstractSkyveTest {
	@AfterEach
	@SuppressWarnings("static-method")
	void restore() {
		WebContainer.clear();
	}

	@Test
	@SuppressWarnings("static-method")
	void aJobCannotAsk() {
		DialogsStaticSingleton.setDefault();
		WebContainer.clear();

		assertFalse(EXT.getDialogs().canAsk());
		Dialogs dialogs = EXT.getDialogs();
		Question question = Question.okCancel("job", "Job", "Can a job ask?");
		IllegalStateException e = assertThrows(IllegalStateException.class, () -> dialogs.ask(question));
		assertTrue(e.getMessage().contains("job"), e.getMessage());
	}

	@Test
	void aQuestionFromPreSaveComesOutOfPersistenceUnwrapped() {
		DialogsStaticSingleton.setDefault();
		MockHttpServletRequest request = new MockHttpServletRequest();
		request.setAttribute(DialogsProtocol.ASKABLE_ATTRIBUTE_NAME, Boolean.TRUE);
		WebContainer.setHttpServletRequestResponse(request, new MockHttpServletResponse());
		DialogsFixture fixture = DialogsFixture.newInstance();
		fixture.setAskOnSave(Boolean.TRUE);

		QuestionException e = assertThrows(QuestionException.class, () -> p.save(fixture));
		assertEquals("DialogsFixture.preSave", e.getQuestion().getKey());
	}

	@Test
	@SuppressWarnings("static-method")
	void testsAnswerWithThePositiveButton() throws Exception {
		DialogsFixture fixture = DialogsFixture.newInstance();

		DialogsFixture result = new AskTwice().execute(fixture, null).getBean();

		assertEquals("Ask twice answered YES then OK", result.getOutcome());
	}

	@Test
	@SuppressWarnings("static-method")
	void aTestCanAnswerByKey() throws Exception {
		DialogsStaticSingleton.set(new TestDialogs(Map.of("DialogsFixture.AskTwice#1", "NO")));
		DialogsFixture fixture = DialogsFixture.newInstance();

		DialogsFixture result = new AskTwice().execute(fixture, null).getBean();

		assertEquals("Ask twice answered NO then OK", result.getOutcome());
	}

	@Test
	@SuppressWarnings("static-method")
	void aTestCannotAnswerWithAButtonThatAborts() {
		DialogsStaticSingleton.set(new TestDialogs(Map.of("DialogsFixture.AskOnce", "CANCEL")));
		DialogsFixture fixture = DialogsFixture.newInstance();
		AskOnce action = new AskOnce();

		IllegalArgumentException e = assertThrows(IllegalArgumentException.class, () -> action.execute(fixture, null));
		assertTrue(e.getMessage().contains("DialogsFixture.AskOnce"), e.getMessage());
	}

	@Test
	@SuppressWarnings("static-method")
	void aRerenderFromAnotherFieldDoesNotAsk() throws Exception {
		DialogsStaticSingleton.setDefault();
		DialogsFixture fixture = DialogsFixture.newInstance();

		new DialogsFixtureBizlet().preRerender(DialogsFixture.actionTextPropertyName, fixture, mockWebContext());

		assertNull(fixture.getOutcome());
	}

	@Test
	@SuppressWarnings("static-method")
	void aRowAsksOnlyBeforeZoomOutAndRemove() throws Exception {
		DialogsStaticSingleton.setDefault();
		DialogsFixtureRow row = DialogsFixtureRow.newInstance();
		DialogsFixtureRowBizlet bizlet = new DialogsFixtureRowBizlet();
		WebContext webContext = mockWebContext();

		assertSame(row, bizlet.preExecute(ImplicitActionName.Edit, row, null, webContext));
		assertThrows(IllegalStateException.class, () -> bizlet.preExecute(ImplicitActionName.Remove, row, null, webContext));
	}
}
