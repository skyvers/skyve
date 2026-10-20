package org.skyve.dialog;

import org.skyve.dialog.Question.Button;

import jakarta.annotation.Nonnull;

/**
 * Asks the user questions from server code.
 *
 * <p>A question is asked by rolling the request back and rerunning it. When the current request has
 * no answer to a question, {@link #ask(Question)} throws {@link QuestionException}. The renderer
 * rolls the request back, does not keep the conversation, and shows the question. The client then
 * sends the same request again with the answers given so far, and on that rerun {@code ask} returns
 * the answer and the code carries on past the question. A request can ask several questions; each
 * rerun stops at the next unanswered one.
 *
 * <p>Obtain the service through {@link org.skyve.EXT#getDialogs()}. It needs no
 * {@link org.skyve.web.WebContext}, so Bizlet methods such as {@code preSave} and {@code preDelete},
 * interceptors and helper code can ask too.
 *
 * <p>Questions can be asked from custom server-side actions; OK, Save, ZoomOut and Delete, including
 * their Bizlet and interceptor callbacks; rerenders; and, in PrimeFaces, Remove. They cannot be asked
 * when a form loads, while the response is rendered (from getters, conditions or the title), from {@code postRender}
 * (which runs after the response is sent), from list grids, downloads, uploads, REST, jobs, background tasks or push.
 *
 * <p>Threading: implementations are <em>thread-safe</em>; the default implementation holds no state
 * and keeps answers on the current request.
 *
 * @see org.skyve.EXT#getDialogs()
 * @since 10.0
 */
public interface Dialogs {
	/**
	 * Asks a question and returns the code of the button the user chose.
	 *
	 * <p>The answer is matched to the question by an id computed from its key, title, message and
	 * buttons, so an answer to a question whose text has since changed is not used and the question is
	 * asked again. The same question asked twice in one run gets the same answer. This method never
	 * returns the code of a button that aborts (see {@link Question#cancelAborts()}): choosing one ends
	 * the user's gesture on the client without another request.
	 *
	 * <p>Because a question reruns the request, these limits apply. They are documented rather than enforced.
	 * <ul>
	 * <li>Don't ask after a commit in the same request, including
	 * {@code webContext.cacheConversationAndCycleTransaction()}, {@code background()} and an explicit
	 * {@code persistence.commit}. The committed work is not rolled back, and it runs again on the rerun.</li>
	 * <li>Don't ask after a side effect outside the transaction, such as a content or attachment write,
	 * {@code EXT.push}, email, a job, an external HTTP call or a change to static state. Each rerun repeats it.</li>
	 * <li>PrimeFaces: don't ask after changing the conversation's current bean. The view records the new
	 * bean's web id even when the request fails, and the next request cannot find it in the old conversation.</li>
	 * <li>PrimeFaces: scripts queued with {@code executeScript} before the question still run.</li>
	 * <li>SmartClient: a click queued behind a rerender triggered by leaving a field waits while that
	 * rerender asks. If the user answers, the click runs once the rerender completes. If the user aborts,
	 * the click is dropped and the user clicks again.</li>
	 * </ul>
	 *
	 * @param question	The question to ask.
	 * @return	The {@link Button#name() code} of the button the user chose; never the code of a button that aborts.
	 * @throws QuestionException	if the current request has no usable answer to the question. Let it propagate.
	 * @throws IllegalStateException	if questions cannot be asked here (see {@link #canAsk()}), or if the request
	 * 			already carries the maximum number of answers without one for this question. That happens when a
	 * 			question's text changes on every run, so its answers never match. The renderer reports it as an
	 * 			unexpected error.
	 */
	@Nonnull String ask(@Nonnull Question question);

	/**
	 * Asks a question and reports whether the user chose the positive button.
	 *
	 * <p>Behaves as {@link #ask(Question)}, including its exceptions and limits.
	 *
	 * @param question	The question to ask.
	 * @return	{@code true} for OK or Yes; {@code false} for Cancel or No when they do not abort.
	 * @throws QuestionException	if the current request has no usable answer to the question.
	 * @throws IllegalStateException	as for {@link #ask(Question)}.
	 */
	default boolean confirm(@Nonnull Question question) {
		String code = ask(question);
		return Button.OK.name().equals(code) || Button.YES.name().equals(code);
	}

	/**
	 * Indicates whether a question can be asked on the current thread.
	 *
	 * <p>The default implementation answers {@code true} only during an HTTP request on a path that can
	 * ask; it answers {@code false} in a job, a background task, outside any request, and on a request
	 * that cannot ask, such as loading a form.
	 *
	 * @return	{@code true} if {@link #ask(Question)} can ask the user.
	 */
	boolean canAsk();
}
