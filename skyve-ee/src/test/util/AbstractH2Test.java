package util;

import org.junit.After;
import org.junit.Before;
import org.skyve.CORE;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DataStore;
import org.skyve.persistence.Persistence;

import modules.test.domain.AllAttributesInverseOneToOnePersistent;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;
import modules.test.domain.AnyDerived1;
import modules.test.domain.AnyDerived2;
import modules.test.domain.ArcOneToMany;
import modules.test.domain.ArcOneToOne;
import modules.test.domain.Hierarchical;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

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
	protected User u;
	protected Customer c;
	protected Module m;
	protected Document aapd;
	protected Document aai121pd;
	protected Document aarpd;
	protected Document ad1;
	protected Document ad2;
	protected Document ao2m;
	protected Document ao2o;
	protected Document hd;
	protected Document mbd;
	protected Document mejsd;
	protected Document messd;
	protected Document msjsd;
	protected Document msssd;

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
		u = p.getUser();
		c = u.getCustomer();
		m = c.getModule(AllAttributesPersistent.MODULE_NAME);
		aapd = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
		aai121pd = m.getDocument(c, AllAttributesInverseOneToOnePersistent.DOCUMENT_NAME);
		aarpd = m.getDocument(c, AllAttributesRequiredPersistent.DOCUMENT_NAME);
		ad1 = m.getDocument(c, AnyDerived1.DOCUMENT_NAME);
		ad2 = m.getDocument(c, AnyDerived2.DOCUMENT_NAME);
		ao2m = m.getDocument(c, ArcOneToMany.DOCUMENT_NAME);
		ao2o = m.getDocument(c, ArcOneToOne.DOCUMENT_NAME);
		hd = m.getDocument(c, Hierarchical.DOCUMENT_NAME);
		mbd = m.getDocument(c, MappedBase.DOCUMENT_NAME);
		mejsd = m.getDocument(c, MappedExtensionJoinedStrategy.DOCUMENT_NAME);
		messd = m.getDocument(c, MappedExtensionSingleStrategy.DOCUMENT_NAME);
		msjsd = m.getDocument(c, MappedSubclassedJoinedStrategy.DOCUMENT_NAME);
		msssd = m.getDocument(c, MappedSubclassedSingleStrategy.DOCUMENT_NAME);
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

	/*
		@AfterClass
		public static void shutDown() throws Exception {
			Util.LOGGER.fine("SHUT DOWN");
		}
	*/
}
