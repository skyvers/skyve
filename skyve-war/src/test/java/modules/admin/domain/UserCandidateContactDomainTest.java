package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class UserCandidateContactDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		UserCandidateContact bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserCandidateContact.MODULE_NAME, UserCandidateContact.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		UserCandidateContact bean = UserCandidateContact.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserCandidateContact", bean.getBizDocument());
	}

	@Test
	void matchScoreSetAndGet() throws Exception {
		UserCandidateContact bean = UserCandidateContact.newInstance();
		bean.setMatchScore(Integer.valueOf(95));
		assertEquals(Integer.valueOf(95), bean.getMatchScore());
	}

	@Test
	void bizOrdinalSetAndGet() throws Exception {
		UserCandidateContact bean = UserCandidateContact.newInstance();
		bean.setBizOrdinal(Integer.valueOf(1));
		assertEquals(Integer.valueOf(1), bean.getBizOrdinal());
	}
}
