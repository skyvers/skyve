package modules.test;

import modules.test.domain.AllAttributesInverseOneToOnePersistent;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;
import modules.test.domain.Hierarchical;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

import org.junit.After;
import org.junit.Before;
//import org.mockito.Mock;
//import org.powermock.api.mockito.PowerMockito;
//import org.powermock.core.classloader.annotations.PowerMockIgnore;
//import org.powermock.core.classloader.annotations.PrepareForTest;
//import org.powermock.modules.junit4.PowerMockRunner;
import org.skyve.CORE;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.metadata.repository.LocalDesignRepository;
import org.skyve.wildcat.metadata.user.SuperUser;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.hibernate.HibernateNoContentPersistence;
import org.skyve.wildcat.util.UtilImpl;

/**
 * Invoke the JUnit test with PowerMockRunner. 
 * This will allow us to use PowerMock to mock static/final methods if required.
 * 
 * @see https://github.com/jayway/powermock/wiki
 */
//@RunWith(PowerMockRunner.class)
//@PrepareForTest({SetupBizlet.class, HealthAuditInterceptor.class})
//@PowerMockIgnore("org.apache.log4j.*") 
public abstract class AbstractH2Test {
	protected static final String USER = "TestUser";
	protected static final String CUSTOMER = "bizhub";

	private static final String DB_DIALECT = "org.skyve.wildcat.persistence.hibernate.dialect.H2SpatialDialect";
	private static final String DB_DRIVER = "org.h2.Driver";
	private static final String DB_URL = "jdbc:h2:mem:test";
	//private static final String DB_URL = "jdbc:h2:mem:test;DB_CLOSE_ON_EXIT=FALSE";
	//private static final String DB_URL = "jdbc:h2:file:./target/test/db/H2;AUTO_SERVER=TRUE;DB_CLOSE_ON_EXIT=FALSE";
	private static final String DB_UNAME = "user";
	private static final String DB_PWD = "password";

	// Add common mocks here
//	@Mock
//	protected WebContext webContext;
	
	protected Persistence p;
	protected User u;
	protected Customer c;
	protected Module m;
	protected Document aapd;
	protected Document aai121pd;
	protected Document aarpd;
	protected Document hd;
	protected Document mbd;
	protected Document mejsd;
	protected Document messd;
	protected Document msjsd;
	protected Document msssd;
	
	@Before
	public void beforeBase() throws MetaDataException {
		AbstractPersistence.IMPLEMENTATION_CLASS = HibernateNoContentPersistence.class;
		UtilImpl.DIALECT = DB_DIALECT;
		UtilImpl.STANDALONE_DATABASE_JDBC_DRIVER = DB_DRIVER;
		UtilImpl.STANDALONE_DATABASE_CONNECTION_URL = DB_URL;
		UtilImpl.STANDALONE_DATABASE_USERNAME = DB_UNAME;
		UtilImpl.STANDALONE_DATABASE_PASSWORD = DB_PWD;
		UtilImpl.DDL_SYNC = true;
		UtilImpl.SQL_TRACE = false;
		UtilImpl.QUERY_TRACE = false;

		AbstractRepository.set(new LocalDesignRepository());

		final SuperUser user = new SuperUser();
		user.setCustomerName(CUSTOMER);
		user.setName(USER);
		user.setId(USER);
		final AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user);
		persistence.begin();
		
		p = CORE.getPersistence();
		u = p.getUser();
		c = u.getCustomer();
		m = c.getModule(AllAttributesPersistent.MODULE_NAME);
		aapd = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
		aai121pd = m.getDocument(c, AllAttributesInverseOneToOnePersistent.DOCUMENT_NAME);
		aarpd = m.getDocument(c, AllAttributesRequiredPersistent.DOCUMENT_NAME);
		hd = m.getDocument(c,  Hierarchical.DOCUMENT_NAME);
		mbd = m.getDocument(c, MappedBase.DOCUMENT_NAME);
		mejsd = m.getDocument(c, MappedExtensionJoinedStrategy.DOCUMENT_NAME);
		messd = m.getDocument(c, MappedExtensionSingleStrategy.DOCUMENT_NAME);
		msjsd = m.getDocument(c, MappedSubclassedJoinedStrategy.DOCUMENT_NAME);
		msssd = m.getDocument(c, MappedSubclassedSingleStrategy.DOCUMENT_NAME);
	}
	
	@After
	public void afterBase() throws MetaDataException {
		// The call to commit and disposeAllPersistenceInstances will close and dispose the current connection.
		// For H2 by default, closing the last connection to a database closes the database. 
		// For an in-memory database, this means the content is lost.
		// See http://www.h2database.com/html/features.html (In-Memory Databases)
		p.commit(true);
		((AbstractPersistence) p).disposeAllPersistenceInstances();
	}
	
/*
	@AfterClass
	public static void shutDown() throws Exception {
		Util.LOGGER.fine("SHUT DOWN");
	}
*/
}
