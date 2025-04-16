package sail.admin;

import java.util.TreeMap;

import org.jboss.weld.environment.se.Weld;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cdi.SkyveCDIProducer;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.domain.number.NumberGeneratorStaticSingleton;
import org.skyve.impl.geoip.GeoIPServiceStaticSingleton;
import org.skyve.impl.job.JobSchedulerStaticSingleton;
import org.skyve.impl.job.MockJobScheduler;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.persistence.DataStore;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockServletContext;

import util.sail.BrowserConfiguration;
import util.sail.Devices;
import util.sail.PrimeFacesInterpretedWebDriverExecutor;
import util.sail.PrimeFacesTest;

class AdminSail extends PrimeFacesTest {
	private static final String DB_DIALECT = "org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect";
	private static final String DB_DRIVER = "org.h2.Driver";
	private static final String DB_URL = "jdbc:h2:mem:test";
	private static final String DB_UNAME = "user";
	private static final String DB_PWD = "password";

	private static Weld weld;
	
	@BeforeAll
	@SuppressWarnings("resource")
	static void setupClass() {
		// init injection
		weld = new Weld();
		weld.addPackage(true, SkyveCDIProducer.class); // skyve producer
		// For omnifaces to be injected
		weld.addBeanClass(MockHttpServletRequest.class);
		weld.addBeanClass(MockServletContext.class);

		weld.initialize();
	}
	
	@BeforeEach
	void setup() {
		AbstractPersistence.IMPLEMENTATION_CLASS = HibernateContentPersistence.class;
		AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = RDBMSDynamicPersistence.class;
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		NumberGeneratorStaticSingleton.setDefault();
		GeoIPServiceStaticSingleton.setDefault();
		CustomisationsStaticSingleton.setDefault();
		JobSchedulerStaticSingleton.set(new MockJobScheduler());
		UtilImpl.SAIL = true;
		UtilImpl.DATA_STORE = new DataStore(DB_DRIVER, DB_URL, DB_UNAME, DB_PWD, DB_DIALECT);
		UtilImpl.DATA_STORES.put("test", UtilImpl.DATA_STORE);
		UtilImpl.DDL_SYNC = true;
		UtilImpl.SQL_TRACE = false;
		UtilImpl.QUERY_TRACE = false;
		UtilImpl.JOB_SCHEDULER = false;
		UtilImpl.SMTP = "localhost";
		UtilImpl.SMTP_SENDER = "noreply@skyve.org";
		UtilImpl.SMTP_TEST_RECIPIENT = "noreceive@skyve.org";
		UtilImpl.SMTP_TEST_BOGUS_SEND = true;
		UtilImpl.CONFIGURATION = new TreeMap<>();

		ProvidedRepositoryFactory.set(new DefaultRepository());

		final SuperUser user = new SuperUser();
		user.setCustomerName("demo");
		user.setName("admin");
		user.setId("admin");

		final AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(user);
		persistence.begin();

		setupChrome(new BrowserConfiguration().baseUrl("http://localhost:8080/skyve/").userAgentString(Devices.ipad.userAgentString));
//		setupFirefox(new BrowserConfiguration().baseUrl("http://localhost:8080/skyve/").userAgentString(Devices.ipad.userAgentString));
	}

	@AfterEach
	void teardown() {
		tearDownBrowser();
	}
	
	@AfterAll
	static void teardownClass() {
		if (weld != null) {
			weld.shutdown();
		}
	}
	
	@Test
	void test() {
		Automation automation = XMLMetaData.unmarshalSAILFile("/Users/mike/_/skyve/skyve-war/src/test/java/sail/admin/admin-sail.xml");
		FacesUtil.setSailFacesContextIfNeeded();
		try {
			automation.execute(new PrimeFacesInterpretedWebDriverExecutor(this));
		}
		finally {
			FacesUtil.resetSailFacesContextIfNeeded();
		}
	}
}
