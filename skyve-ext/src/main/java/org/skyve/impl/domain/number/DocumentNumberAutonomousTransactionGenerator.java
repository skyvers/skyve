package org.skyve.impl.domain.number;

import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Generates document sequence numbers in an autonomous (inner) database
 * transaction, so that sequence allocation is committed immediately and
 * not rolled back if the outer transaction fails.
 */
public class DocumentNumberAutonomousTransactionGenerator extends AbstractDocumentNumberGenerator {
	@Override
	public @Nonnull String next(@Nullable String prefix, @Nonnull String moduleName, @Nonnull String documentName, @Nonnull String fieldName, int minimumLength) {
		AbstractHibernatePersistence pers = (AbstractHibernatePersistence) CORE.getPersistence();
		User user = pers.getUser();
		try {
			pers = (AbstractHibernatePersistence) AbstractPersistence.newInstance();
		}
		catch (Exception e) {
			throw new DomainException("Could not instantiate Autonomous Transaction Persistence", e);
		}
		try {
			pers.setUser(user);
			pers.begin();
			return getNextNumber(pers, prefix, moduleName, documentName, fieldName, minimumLength);
		}
		finally {
			// Can't call pers.commit(true) here as it would remove the current thread's Persistence as well
			try {
				pers.commit(false);
			}
			finally {
				pers.close();
			}
		}
	}
}
