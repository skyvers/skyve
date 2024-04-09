package util;

import org.junit.jupiter.api.AfterEach;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.backup.Truncate;
import org.skyve.persistence.Persistence;

/**
 * This is the base test class to use whenever a rollback is not appropriate
 * to clean up at the end of running a test.
 * 
 * Instead of performing a rollback at the end of each test, it will truncate
 * (delete all rows from all tables) and begin a new transaction.
 * 
 * An example of when to extend this base class is if the code under test is
 * performing its own commits, which will not be able to be rolled back.
 */
public abstract class AbstractH2TestTruncate extends AbstractH2Test {

	private static final String SCHEMA = "PUBLIC";

	@Override
	@AfterEach
	public void afterBase() {
		super.afterBase(); // rollback and evict
		
		Persistence p = CORE.getPersistence();
		try {
			p.begin();
			Truncate.truncate(SCHEMA, true, true);
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
		finally {
			p.commit(false);
		}
	}
}
