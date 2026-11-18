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
 * <p>Precondition: READ COMMITTED isolation (as in Skyve's shipped MySQL and PostgreSQL
 * datasource templates). At a snapshot isolation level (a hand-written MySQL datasource at
 * REPEATABLE READ, or {@code SQLServer2016HibernateContentPersistence}, which sets SNAPSHOT)
 * concurrent first uses of a sequence can silently produce duplicate rows and numbers. Waits
 * are bounded by the database's lock timeout and Skyve's OLTP query timeout.
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
