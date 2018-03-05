package org.skyve.impl.sail.interpret;

import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.sail.execution.PrimeFacesInlineSeleneseExecutor;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.persistence.DataStore;

public class Interpreter {
	protected static final String USER = "TestUser";
	protected static final String CUSTOMER = "bizhub";

	private static final String DB_DIALECT = "org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect";
	private static final String DB_DRIVER = "org.h2.Driver";
	private static final String DB_URL = "jdbc:h2:mem:test";
	private static final String DB_UNAME = "user";
	private static final String DB_PWD = "password";
	private static final String CONTENT_DIRECTORY = "content/";

	public static void main(String[] args) throws Exception {
		AbstractPersistence.IMPLEMENTATION_CLASS = HibernateContentPersistence.class;
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		UtilImpl.DATA_STORE = new DataStore(DB_DRIVER, DB_URL, DB_UNAME, DB_PWD, DB_DIALECT);
		UtilImpl.DATA_STORES.put("test", UtilImpl.DATA_STORE);
		UtilImpl.DDL_SYNC = true;
		UtilImpl.SQL_TRACE = false;
		UtilImpl.QUERY_TRACE = false;
		UtilImpl.CONTENT_DIRECTORY = CONTENT_DIRECTORY;

//generator should create Automation object
//		String xml = Generator.visitModules("bizhub");
//System.out.println(xml);
//		Automation automation = XMLUtil.unmarshalSAIL(new StringReader(xml));
		AbstractRepository.set(new LocalDesignRepository());

		final SuperUser user = new SuperUser();
		user.setCustomerName(CUSTOMER);
		user.setName(USER);
		user.setId(USER);

		final AbstractPersistence persistence = AbstractPersistence.get();
		try {
			persistence.setUser(user);
			persistence.begin();

			Automation automation = XMLMetaData.unmarshalSAIL("/Users/mike/dtf/skyve/skyve-tools/test.xml");
			PrimeFacesInlineSeleneseExecutor executor = new PrimeFacesInlineSeleneseExecutor(new SkyveComponentBuilderChain());
			automation.execute(executor);
			System.out.println(executor);
		}
		finally {
			// The call to commit and disposeAllPersistenceInstances will close and dispose the current connection.
			// For H2 by default, closing the last connection to a database closes the database.
			// For an in-memory database, this means the content is lost.
			// See http://www.h2database.com/html/features.html (In-Memory Databases)
			persistence.commit(true);
			persistence.disposeAllPersistenceInstances();
		}
	}
}
