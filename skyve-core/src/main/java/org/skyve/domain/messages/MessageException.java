package org.skyve.domain.messages;

import java.util.List;

import jakarta.annotation.Nonnull;

/**
 * 
 */
public interface MessageException {
	public @Nonnull List<Message> getMessages();
}
