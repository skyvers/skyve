package util.sail.browser;

import java.time.Instant;
import java.util.List;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The JavaScript dialogs the browser's pages open: {@code alert}, {@code confirm}, {@code prompt} and
 * {@code beforeunload}. A driver may answer a {@code beforeunload} dialog itself at once (ChromeDriver does), so the
 * record of dialogs seen is the only way a test sees one.
 * <p>
 * The capability {@link Capability#DIALOGS}.
 */
public interface Dialogs {
	/**
	 * Returns the dialogs the pages have opened since the control began watching (at the latest, the first call to
	 * this interface) or the last {@link #clearDialogs()}.
	 *
	 * @return the dialogs, oldest first; a copy the caller owns
	 */
	@Nonnull List<Dialog> dialogs();

	/**
	 * Forgets the dialogs seen so far.
	 */
	void clearDialogs();

	/**
	 * Returns when the record of dialogs began: when the control began watching the browser, which is the first time a
	 * step asked for the browser's control, not when the browser started. A dialog opened before then is not recorded,
	 * so a check that the record began in its own step can't tell what happened before it.
	 *
	 * @return the time the record began
	 */
	@Nonnull Instant watchingSince();

	/**
	 * Answers the dialog open now in the page in front, as a user would.
	 *
	 * @param accept	true for OK (or leave, for {@code beforeunload}), false for Cancel (or stay)
	 * @param promptText	the text to answer a {@code prompt} with, or null to leave its default
	 * @throws IllegalStateException	if no dialog is open
	 */
	void answerDialog(boolean accept, @Nullable String promptText);
}
