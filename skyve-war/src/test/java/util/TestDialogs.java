package util;

import java.util.Map;

import org.skyve.dialog.Dialogs;
import org.skyve.dialog.Question;
import org.skyve.dialog.Question.Button;

import jakarta.annotation.Nonnull;

/**
 * Answers server-side questions in tests, which run without an HTTP request.
 *
 * <p>A question is answered with the code mapped to its key, or otherwise with its positive button (OK or Yes),
 * so that code under test runs past its questions. {@link InternalBaseH2Test} installs one with no mapped
 * answers before each test. A test that needs other answers installs its own with
 * {@code DialogsStaticSingleton.set(new TestDialogs(Map.of("key", "NO")))}; one that tests asking through
 * a request installs the request-based implementation with {@code DialogsStaticSingleton.setDefault()}.
 *
 * <p>Threading: <em>thread-safe</em>; the answers are not changed after construction.
 */
public class TestDialogs implements Dialogs {
	private final Map<String, String> codesByKey;

	/**
	 * Creates dialogs that answer every question with its positive button.
	 */
	public TestDialogs() {
		this(Map.of());
	}

	/**
	 * Creates dialogs that answer questions from a map, and other questions with their positive button.
	 *
	 * @param codesByKey	The button code to answer, by question key. It is kept, not copied.
	 */
	public TestDialogs(@Nonnull Map<String, String> codesByKey) {
		this.codesByKey = codesByKey;
	}

	/**
	 * Returns the code mapped to the question's key, or the code of its positive button.
	 *
	 * @throws IllegalArgumentException	if the mapped code is not one of the question's buttons, or is a button that aborts,
	 * 			which a request could never answer.
	 */
	@Override
	public @Nonnull String ask(@Nonnull Question question) {
		String code = codesByKey.get(question.getKey());
		if (code == null) {
			return question.getButtons().get(0).name();
		}
		for (Button button : question.getButtons()) {
			if (button.name().equals(code) && (! question.aborts(button))) {
				return code;
			}
		}
		throw new IllegalArgumentException("Question " + question.getKey() + " cannot be answered " + code);
	}

	/**
	 * Always answers {@code true}: these dialogs can answer anywhere.
	 */
	@Override
	public boolean canAsk() {
		return true;
	}
}
