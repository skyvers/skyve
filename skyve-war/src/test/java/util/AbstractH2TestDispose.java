package util;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.persistence.Persistence;

/**
 * This is the base test class to use whenever a rollback is not appropriate
 * to clean up at the end of running a test.
 * 
 * Instead of performing a rollback at the end of each test, it will dispose
 * of the connection which causes H2 to delete the in-memory database and create
 * a new one.
 * 
 * An example of when to extend this base class is if the code under test is
 * performing its own commits, which will not be able to be rolled back.
 */
public abstract class AbstractH2TestDispose extends AbstractH2Test {

	@BeforeEach
	@SuppressWarnings("static-method")
	public void before() throws Exception {
		EXT.getCaching().startup();
	}

	@Override
	@AfterEach
	public void afterBase() {
		super.afterBase(); // rollback and evict

		// The call to commit and disposeAllPersistenceInstances will close and dispose the current connection.
		// For H2 by default, closing the last connection to a database closes the database.
		// For an in-memory database, this means the content is lost.
		// See http://www.h2database.com/html/features.html (In-Memory Databases)
		Persistence p = CORE.getPersistence();
		((AbstractPersistence) p).disposeAllPersistenceInstances();
	}
}
