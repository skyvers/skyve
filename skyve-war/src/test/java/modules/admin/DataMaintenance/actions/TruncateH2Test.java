package modules.admin.DataMaintenance.actions;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.persistence.Persistence;

import jakarta.inject.Inject;
import modules.admin.User.UserService;
import modules.admin.domain.AdminDocument;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.DataMaintenance;
import util.AbstractH2TestDispose;

/**
 * Runs Data Maintenance's Truncate action, to check what it leaves in Hibernate's shared (second-level) cache. The
 * truncate commits, so each test disposes of the database.
 */
class TruncateH2Test extends AbstractH2TestDispose {
	@Inject
	private transient UserService userService;

	/**
	 * The truncate deletes the rows with SQL. When it left Hibernate's shared cache as it was, a cached bean (the
	 * admin Contact is cached) could still be retrieved after its row was deleted, and a save of it would fail.
	 */
	@Test
	void aCachedBeanIsGoneAfterATruncate() throws Exception {
		Persistence p = CORE.getPersistence();
		Contact contact = Contact.newInstance();
		contact.setName("Truncate cache");
		contact.setContactType(ContactType.person);
		contact = p.save(contact);
		p.commit(false);
		p.begin();
		p.evictAllCached();
		Contact cached = p.retrieve(AdminDocument.CONTACT, contact.getBizId());
		assertNotNull(cached);
		assertTrue(p.sharedCacheBean(cached), "the contact is in the shared cache before the truncate");

		DataMaintenance bean = DataMaintenance.newInstance();
		bean.setSchemaName("PUBLIC");
		bean.setConfirmPassword(PASSWORD);
		bean.setInjectBootstrapUser(Boolean.FALSE);
		Truncate action = new Truncate();
		Field field = Truncate.class.getDeclaredField("userService");
		field.setAccessible(true);
		field.set(action, userService);
		action.execute(bean, new MockWebContext());

		p.evictAllCached(); // a new request starts with an empty session
		assertFalse(p.sharedCacheBean(cached), "the truncate evicted the contact from the shared cache");
		assertNull(p.retrieve(AdminDocument.CONTACT, contact.getBizId()),
					"the truncate deleted the contact's row");
	}
}
