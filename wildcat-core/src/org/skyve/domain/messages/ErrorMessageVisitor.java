package org.skyve.domain.messages;

import java.util.List;

/**
 * 
 */
public abstract class ErrorMessageVisitor {
	/**
	 * 
	 * @param message
	 */
	public final void visit(ErrorMessage message) {
		accept(message.getErrorMessage(), message.getBindings());
		List<ErrorMessage> subordinates = message.getSubordinates();
		for (ErrorMessage subordinate : subordinates) {
			visit(subordinate);
		}
	}

	/**
	 * 
	 * @param message
	 * @param bindings
	 */
	protected abstract void accept(String message, Iterable<String> bindings);
}
