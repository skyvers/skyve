package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.web.UserAgentType;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.ControlPanel.SailTestStrategy;
import modules.admin.domain.ControlPanel.SailUserAgentType;
import router.UxUis;
import util.AbstractH2TestTruncate;

/**
 * Tests that the Control Panel's SAIL generation leaves no data behind (M16): generation runs application code (the
 * Bizlets' {@code newInstance}, view conditions and test data), and what that code writes is rolled back. The SAIL user
 * is saved and committed, so the data is truncated after each test.
 */
@SuppressWarnings("java:S120") // Module test packages mirror the generated document package.
class GenerateSAILRollbackH2Test extends AbstractH2TestTruncate {
	private ControlPanelExtension bean;
	private MockWebContext webContext;
	private Set<String> twoFactorCustomers;

	@BeforeEach
	void setup() {
		bean = ControlPanelSAILFixture.controlPanel();
		bean.setSailTestStrategy(SailTestStrategy.Assert);
		bean.setSailUxUi(UxUis.EXTERNAL.getName());
		bean.setSailUserAgentType(SailUserAgentType.desktop);
		webContext = new MockWebContext();
		twoFactorCustomers = UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS;
		// admin.Configuration's view reads it, and only a server's start-up sets it
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = Set.of();
	}

	@AfterEach
	void restore() {
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = twoFactorCustomers;
	}

	/**
	 * What application code writes while the Control Panel generates (here a Contact saved by the generation itself)
	 * is rolled back, and the persistence can still be used afterwards.
	 */
	@Test
	void whatGenerationWritesIsRolledBack() throws Exception {
		long contacts = contacts();
		GenerateSAIL writing = new GenerateMenuSAIL() {
			@Override
			public Automation single(User user,
										String loginCustomer,
										String loginPassword,
										String moduleName,
										String uxui,
										UserAgentType userAgentType,
										TestStrategy testStrategy)
			throws Exception {
				Contact contact = new DataBuilder().fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
				CORE.getPersistence().save(contact);
				return super.single(user, loginCustomer, loginPassword, moduleName, uxui, userAgentType, testStrategy);
			}
		};
		bean.setSailModuleName("kitchensink");

		writing.execute(bean, webContext);

		assertAll(() -> assertNotNull(bean.getUnescapedResults(), "generated"),
					() -> assertEquals(contacts, contacts(), "the Contact saved while generating is rolled back"));
	}

	/**
	 * Generating the admin and kitchen sink menus' SAIL, and the external Control Panel's Download of the admin menu's
	 * test, leaves every table's row count as it was.
	 */
	@Test
	void generatingMenusLeavesEveryTableAsItWas() throws Exception {
		Map<String, Long> before = rowCounts();

		bean.setSailModuleName("admin");
		new GenerateMenuSAIL().execute(bean, webContext);
		bean.setSailModuleName("kitchensink");
		new GenerateMenuSAIL().execute(bean, webContext);
		bean.setSailModuleName("admin");
		new DownloadClientSAIL().prepare(bean, webContext);

		assertEquals(before, rowCounts());
	}

	/**
	 * A persistent bean that generation's application code changes without saving is not written later in the request,
	 * even when a later save flushes the session: the session is cleared with the rollback.
	 */
	@Test
	void anUnsavedChangeDuringGenerationIsNotFlushedLater() throws Exception {
		Persistence p = CORE.getPersistence();
		Contact contact = new DataBuilder().fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		contact.setName("Before generation");
		contact = p.save(contact);
		p.commit(false);
		p.begin();
		String bizId = contact.getBizId();
		GenerateSAIL changing = new GenerateMenuSAIL() {
			@Override
			public Automation single(User user,
										String loginCustomer,
										String loginPassword,
										String moduleName,
										String uxui,
										UserAgentType userAgentType,
										TestStrategy testStrategy)
			throws Exception {
				Contact retrieved = CORE.getPersistence().retrieve(Contact.MODULE_NAME, Contact.DOCUMENT_NAME, bizId);
				if (retrieved != null) { // it was saved and committed above
					retrieved.setName("Changed while generating");
				}
				return super.single(user, loginCustomer, loginPassword, moduleName, uxui, userAgentType, testStrategy);
			}
		};
		bean.setSailModuleName("kitchensink");

		changing.execute(bean, webContext);
		// Skyve's session flushes only on a save, which flushes every change in the session: save something later in
		// the request, then commit as the request does
		p.save(new DataBuilder().fixture(FixtureType.crud).<Contact> build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME));
		p.commit(false);
		p.begin();
		p.evictAllCached();

		Contact reread = p.retrieve(Contact.MODULE_NAME, Contact.DOCUMENT_NAME, bizId);
		assertNotNull(reread);
		assertEquals("Before generation", reread.getName());
	}

	private static long contacts() {
		return CORE.getPersistence().newDocumentQuery(Contact.MODULE_NAME, Contact.DOCUMENT_NAME).beanResults().size();
	}

	/**
	 * Returns the number of rows in each table of the H2 database's public schema, by table name.
	 */
	private static Map<String, Long> rowCounts() {
		Persistence p = CORE.getPersistence();
		List<String> tables = p.newSQL("select table_name from information_schema.tables " +
										"where table_schema = 'PUBLIC' and table_type = 'BASE TABLE'")
								.scalarResults(String.class);
		Map<String, Long> result = new TreeMap<>();
		for (String table : tables) {
			Number count = p.newSQL("select count(1) from \"" + table + '"').scalarResult(Number.class);
			result.put(table, Long.valueOf((count == null) ? -1 : count.longValue()));
		}
		return result;
	}
}
