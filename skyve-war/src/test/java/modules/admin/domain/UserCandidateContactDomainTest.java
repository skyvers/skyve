package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Contact.ContactExtension;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UserCandidateContactDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() {
		UserCandidateContact bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserCandidateContact.MODULE_NAME, UserCandidateContact.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() {
		UserCandidateContact bean = UserCandidateContact.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserCandidateContact", bean.getBizDocument());
	}

	@Test
	void matchScoreSetAndGet() {
		UserCandidateContact bean = UserCandidateContact.newInstance();
		bean.setMatchScore(Integer.valueOf(95));
		assertEquals(Integer.valueOf(95), bean.getMatchScore());
	}

	@Test
	void bizOrdinalSetAndGet() {
		UserCandidateContact bean = UserCandidateContact.newInstance();
		bean.setBizOrdinal(Integer.valueOf(1));
		assertEquals(Integer.valueOf(1), bean.getBizOrdinal());
	}

	@Test
	void contactSetAndGet() {
		UserCandidateContact bean = UserCandidateContact.newInstance();
		assertNull(bean.getContact());
		ContactExtension contact = Contact.newInstance();
		bean.setContact(contact);
		assertEquals(contact, bean.getContact());
	}

	@Test
	void getBizKeyWithNoContact() {
		UserCandidateContact bean = UserCandidateContact.newInstance();
		assertNotNull(bean.getBizKey());
	}
}
