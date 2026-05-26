package modules.admin.Contact;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import modules.admin.domain.Contact.ContactType;

@SuppressWarnings("static-method")
public class ContactExtensionTest {

	@Test
	void bizKeyWithNullNameReturnsUnnamedContact() {
		ContactExtension contact = new ContactExtension();
		assertEquals("Unnamed Contact", contact.bizKey());
	}

	@Test
	void bizKeyWithNameReturnsName() {
		ContactExtension contact = new ContactExtension();
		contact.setName("Jane Doe");
		assertEquals("Jane Doe", contact.bizKey());
	}

	@Test
	void bizKeyWithNameAndContactTypeIncludesType() {
		ContactExtension contact = new ContactExtension();
		contact.setName("Jane Doe");
		contact.setContactType(ContactType.person);
		String result = contact.bizKey();
		assertTrue(result.startsWith("Jane Doe ("));
		assertTrue(result.contains("person"));
	}

	@Test
	void bizKeyWithNameAndMobileIncludesMobile() {
		ContactExtension contact = new ContactExtension();
		contact.setName("Jane Doe");
		contact.setMobile("0400000000");
		String result = contact.bizKey();
		assertTrue(result.contains("(m) 0400000000"));
	}
}
