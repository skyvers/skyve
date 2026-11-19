package util.sail.browser;

import jakarta.annotation.Nonnull;

/**
 * A JavaScript dialog a page opened, as {@link Dialogs} saw it.
 *
 * @param type	what kind of dialog it is
 * @param message	the dialog's message (empty for {@link Type#BEFOREUNLOAD})
 * @param url	the page that opened it
 */
public record Dialog(@Nonnull Type type, @Nonnull String message, @Nonnull String url) {
	/** The kinds of JavaScript dialog. */
	public enum Type {
		/** {@code alert()}: a message and OK. */
		ALERT,
		/** {@code confirm()}: OK or Cancel. */
		CONFIRM,
		/** {@code prompt()}: a text answer, or Cancel. */
		PROMPT,
		/** Leave the page or stay, asked by a {@code beforeunload} handler. */
		BEFOREUNLOAD
	}
}
