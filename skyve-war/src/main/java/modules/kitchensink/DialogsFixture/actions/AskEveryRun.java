package modules.kitchensink.DialogsFixture.actions;

import java.util.concurrent.atomic.AtomicInteger;

import org.skyve.EXT;
import org.skyve.dialog.Question;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.kitchensink.domain.DialogsFixture;

/**
 * Asks a question whose text includes a counter that changes on every run, so that no answer ever matches it
 * and the request fails once it carries the maximum number of answers.
 *
 * <p>The counter is static state outside the transaction, which is exactly what a question must not depend on.
 */
public class AskEveryRun implements ServerSideAction<DialogsFixture> {
	/**
	 * Counts the runs of this action in this JVM. Thread-safe.
	 */
	private static final AtomicInteger RUNS = new AtomicInteger();

	/**
	 * Asks a question that is different on every run.
	 *
	 * @param bean	The fixture.
	 * @param webContext	The current web context.
	 * @return	The fixture, which is never reached while the counter changes.
	 * @throws org.skyve.dialog.QuestionException	on every run until the answer limit is reached.
	 * @throws IllegalStateException	once the request carries the maximum number of answers.
	 */
	@Override
	public ServerSideActionResult<DialogsFixture> execute(DialogsFixture bean, WebContext webContext) throws Exception {
		EXT.getDialogs().ask(Question.okCancel("DialogsFixture.AskEveryRun",
												"Ask every run",
												"This question changes on every run. This is run " + RUNS.incrementAndGet() + ".")
										.cancelAborts());
		bean.setOutcome("Ask every run answered OK");
		return new ServerSideActionResult<>(bean);
	}
}
