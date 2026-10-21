package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;

import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;

/**
 * Thrown when a request names an action the user could not have triggered from the view as last
 * rendered, or asks for a download that was not prepared.
 *
 * <p>A client may request an action that is hidden or disabled on the current screen, either
 * because the screen is stale (the conditions controlling the action changed after it was rendered)
 * or because the request was crafted by hand. The server refuses the request with this exception
 * rather than executing it.
 *
 * <p>{@link #getMessage()} is an untranslated developer message naming the user, the document and
 * the action, intended for logs. {@link #getMessages()} holds a single localised message for the
 * user with no bindings, so clients show it as a global message rather than against a field.
 *
 * @since 10.0
 * @see OptimisticLockException
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class ActionUnavailableException extends DomainException implements MessageException {
	private static final long serialVersionUID = -4386052287384713096L;

	private static final String MESSAGE_KEY = "exception.actionUnavailable";

	private final List<Message> messages;

	/**
	 * Creates the exception for a refused action.
	 *
	 * <p>The user message is localised by {@link Message#Message(String)} through
	 * {@link org.skyve.util.Util#nullSafeI18n(String, String...)}, which uses the locale of the user on
	 * the current thread's persistence, or English when there is none.
	 *
	 * @param user the user who requested the action; must not be {@code null}
	 * @param documentName the name of the document the action was requested on; must not be {@code null}
	 * @param actionName the name of the requested action, as the request gave it; must not be {@code null}
	 */
	public ActionUnavailableException(@Nonnull User user, @Nonnull String documentName, @Nonnull String actionName) {
		super("Action " + actionName + " is not available to user " + user.getName() + " on document " + documentName, false);
		messages = Collections.singletonList(new Message(MESSAGE_KEY));
	}

	/**
	 * Returns the single localised user message, which has no bindings.
	 *
	 * @return an immutable list of one message; never {@code null}
	 */
	@Override
	public @Nonnull List<Message> getMessages() {
		return messages;
	}
}
