package org.skyve.impl.domain.number;

import org.skyve.CORE;
import org.skyve.persistence.Persistence;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Standard {@link AbstractDocumentNumberGenerator} that retrieves the current-thread
 * persistence instance from {@link org.skyve.CORE#getPersistence()} to increment
 * and return the next sequence value.
 *
 * <p>This is the default implementation registered via
 * {@link NumberGeneratorStaticSingleton#setDefault()}.
 *
 * <p>Threading: thread-safe; see {@link AbstractDocumentNumberGenerator}. The number is taken
 * in the caller's transaction, so a transaction that rolls back gives its number back (no
 * gaps), and the sequence's row stays locked until the caller's transaction ends: concurrent
 * transactions numbering from one sequence wait for each other.
 *
 * @see AbstractDocumentNumberGenerator
 * @see NumberGeneratorStaticSingleton
 */
public class DocumentNumberGenerator extends AbstractDocumentNumberGenerator {
	@Override
	public @Nonnull String next(@Nullable String prefix, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String fieldName, int minimumLength) {
		Persistence pers = CORE.getPersistence();
		return getNextNumber(pers, prefix, moduleName, documentName, fieldName, minimumLength);
	}
}
