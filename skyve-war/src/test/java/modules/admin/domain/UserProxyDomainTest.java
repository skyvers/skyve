package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UserProxyDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		UserProxy bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		UserProxy bean = UserProxy.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserProxy", bean.getBizDocument());
	}

	@Test
	void userNameSetAndGet() {
		UserProxy bean = UserProxy.newInstance();
		bean.setUserName("proxyuser");
		assertEquals("proxyuser", bean.getUserName());
	}

	@Test
	void inactiveSetAndGet() {
		UserProxy bean = UserProxy.newInstance();
		bean.setInactive(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getInactive());
	}

        @Test
        void getBizKeyNotNull() {
                UserProxy bean = UserProxy.newInstance();
                assertNotNull(bean.getBizKey());
        }

        @Test
        void createdDateTimeSetAndGet() {
                UserProxy bean = UserProxy.newInstance();
                org.skyve.domain.types.DateTime now = new org.skyve.domain.types.DateTime();
                bean.setCreatedDateTime(now);
                assertEquals(now, bean.getCreatedDateTime());
        }

        @Test
        void contactSetAndGet() {
                modules.admin.UserProxy.UserProxyExtension bean = new modules.admin.UserProxy.UserProxyExtension();
                modules.admin.Contact.ContactExtension contact = new modules.admin.Contact.ContactExtension();
                bean.setContact(contact);
                assertEquals(contact, bean.getContact());
                // Setting same value should be a no-op
                bean.setContact(contact);
                assertEquals(contact, bean.getContact());
        }

        @Test
        void getBizKeyWithContactReturnsFormattedKey() {
                UserProxy bean = UserProxy.newInstance();
                bean.setUserName("testuser");
                modules.admin.Contact.ContactExtension contact = new modules.admin.Contact.ContactExtension();
                contact.setName("Test Contact");
                bean.setContact(contact);
                assertNotNull(bean.getBizKey());
        }
}
