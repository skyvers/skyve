package modules.admin.User;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.User;
import util.AbstractH2Test;

class UserExtensionH2Test extends AbstractH2Test {
	private UserExtension bean;

	@BeforeEach
	void setup() {
		DataBuilder db = new DataBuilder().fixture(FixtureType.crud);
		bean = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		// create the test data
		bean = CORE.getPersistence().save(bean);
	}

	@Test
	void testToProxyCopiesExpectedAttributes() {
		// validate the test data
		assertThat(bean.getContact(), is(notNullValue()));

		// call the method under test
		UserProxyExtension result = bean.toUserProxy();

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBizId(), is(bean.getBizId()));
		assertThat(result.getBizUserId(), is(bean.getBizUserId()));
		assertThat(result.getBizCustomer(), is(bean.getBizCustomer()));

		assertThat(result.getUserName(), is(bean.getUserName()));
		assertThat(result.getInactive(), is(bean.getInactive()));
		assertThat(result.getContact(), is(bean.getContact()));
	}

	@Test
	@SuppressWarnings("boxing")
	void testToProxyDoesNotCreateDuplicates() {
		int initalSize = CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME).beanResults().size();

		// call the method under test
		UserProxyExtension result = bean.toUserProxy();
		result = CORE.getPersistence().save(result);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME).beanResults().size(),
					is(initalSize));
	}

	@Test
	void testGetPasswordLastChangedCountryNameWithValidCode() {
		bean.setPasswordLastChangedCountryCode("AU");
		String result = bean.getPasswordLastChangedCountryName();
		// Should return a non-null country name for a valid country code
		assertNotNull(result);
	}

	@Test
	void testOwningUserReturnsFalseWhenBizIdDoesNotMatchCurrentUser() {
		// bean is a different user from the test runner's admin user
		// owningUser() checks if CORE.getPersistence().getUser().getId() equals bean.getBizId()
		// Since bean is a saved User, not the currently-running test user, this returns false
		assertFalse(bean.owningUser());
	}

	@Test
	void testBizKeyReturnsUserName() {
		bean.setInactive(Boolean.FALSE);
		String key = bean.bizKey();
		// Should not start with INACTIVE
		assertFalse(key.startsWith("INACTIVE"));
	}

	@Test
	void testGetAssignedRolesReturnsListForPersistedUser() {
		// Just call the method to ensure it doesn't throw and returns a list
		assertNotNull(bean.getAssignedRoles());
	}
}
