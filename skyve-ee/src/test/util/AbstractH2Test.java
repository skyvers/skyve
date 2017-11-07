package util;

import org.apache.deltaspike.core.api.provider.BeanProvider;
import org.jboss.weld.environment.se.Weld;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.skyve.CORE;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.Persistence;

public abstract class AbstractH2Test {
	protected static final String USER = "TestUser";
	protected static final String CUSTOMER = "bizhub";

	private static final String DB_DIALECT = "org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect";
	private static final String DB_DRIVER = "org.h2.Driver";
	private static final String DB_URL = "jdbc:h2:mem:test";
	private static final String DB_UNAME = "user";
	private static final String DB_PWD = "password";
	private static final String CONTENT_DIRECTORY = "content/";

	// Add common mocks here
	// @Mock
	// protected WebContext webContext;

	protected Persistence p;

	private static Weld weld;

	public AbstractH2Test() {
		// support injected classes in unit tests
		BeanProvider.injectFields(this);
	}

	@BeforeClass
	public static void beforeClass() throws Exception {
		weld = new Weld();
		weld.addPackage(true, CORE.class);
		weld.initialize();
	}
	
	@AfterClass
	public static void afterClass() throws Exception {
		if (weld != null) {
			weld.shutdown();
		}
	}

	@Before
	public void beforeBase() {
		AbstractPersistence.IMPLEMENTATION_CLASS = HibernateContentPersistence.class;
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		UtilImpl.DATA_STORE = new DataStore(DB_DRIVER, DB_URL, DB_UNAME, DB_PWD, DB_DIALECT);
		UtilImpl.DATA_STORES.put("test", UtilImpl.DATA_STORE);
		UtilImpl.DDL_SYNC = true;
		UtilImpl.SQL_TRACE = false;
		UtilImpl.QUERY_TRACE = false;
		UtilImpl.CONTENT_DIRECTORY = CONTENT_DIRECTORY;

		AbstractRepository.set(new LocalDesignRepository());

		final SuperUser user = new SuperUser();
		user.setCustomerName(CUSTOMER);
		user.setName(USER);
		user.setId(USER);
		final AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user);
		persistence.begin();

		p = CORE.getPersistence();
	}

	@After
	public void afterBase() {
		// The call to commit and disposeAllPersistenceInstances will close and dispose the current connection.
		// For H2 by default, closing the last connection to a database closes the database.
		// For an in-memory database, this means the content is lost.
		// See http://www.h2database.com/html/features.html (In-Memory Databases)
		p.commit(true);
		((AbstractPersistence) p).disposeAllPersistenceInstances();
	}
}
