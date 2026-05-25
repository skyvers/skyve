package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Contact.ContactType;
import util.AbstractH2Test;

public class ContactDomainTest extends AbstractH2Test {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesContact() throws Exception {
		Contact bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleAndDocumentNames() throws Exception {
		Contact bean = Contact.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Contact", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void nameSetAndGet() throws Exception {
		Contact bean = Contact.newInstance();
		bean.setName("Jane Smith");
		assertEquals("Jane Smith", bean.getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void contactTypePersonSetAndGet() throws Exception {
		Contact bean = Contact.newInstance();
		bean.setContactType(Contact.ContactType.person);
		assertEquals(Contact.ContactType.person, bean.getContactType());
	}

	@Test
	@SuppressWarnings("static-method")
	void contactTypeOrganisationSetAndGet() throws Exception {
		Contact bean = Contact.newInstance();
		bean.setContactType(Contact.ContactType.organisation);
		assertEquals(Contact.ContactType.organisation, bean.getContactType());
	}

	@Test
	@SuppressWarnings("static-method")
	void contactTypeEnumCodes() {
		assertEquals("Person", Contact.ContactType.person.toCode());
		assertEquals("Organisation", Contact.ContactType.organisation.toCode());
	}

	@Test
	@SuppressWarnings("static-method")
	void email1SetAndGet() throws Exception {
		Contact bean = Contact.newInstance();
		bean.setEmail1("jane@example.com");
		assertEquals("jane@example.com", bean.getEmail1());
	}

	@Test
	@SuppressWarnings("static-method")
	void mobileSetAndGet() throws Exception {
		Contact bean = Contact.newInstance();
		bean.setMobile("+61400000000");
		assertEquals("+61400000000", bean.getMobile());
	}

	@Test
	@SuppressWarnings("static-method")
	void imageSetAndGet() throws Exception {
		Contact bean = Contact.newInstance();
		bean.setImage("content-id-abc123");
		assertEquals("content-id-abc123", bean.getImage());
	}

        @Test
        @SuppressWarnings("static-method")
        void contactTypeFromCodeAndFromLocalisedDescription() {
                assertEquals(ContactType.person, ContactType.fromCode("Person"));
                assertNull(ContactType.fromCode("notexist"));
                assertNull(ContactType.fromLocalisedDescription("notexist"));
                assertNotNull(ContactType.fromLocalisedDescription(ContactType.person.toLocalisedDescription()));
        }

        @Test
        @SuppressWarnings("static-method")
        void contactTypeToDomainValues() {
                assertNotNull(ContactType.toDomainValues());
                assertEquals(2, ContactType.toDomainValues().size());
        }

        @Test
        @SuppressWarnings("static-method")
        void contactTypeToCodeAndToDomainValue() {
                assertEquals("Person", ContactType.person.toCode());
                assertNotNull(ContactType.person.toDomainValue());
                assertEquals("Person", ContactType.person.toDomainValue().getCode());
        }
}
