package modules.admin.User;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.User;
import util.AbstractH2TestForJUnit5;

public class UserExtensionH2Test extends AbstractH2TestForJUnit5 {

	private UserExtension bean;

	@BeforeEach
	public void setup() throws Exception {
		DataBuilder db = new DataBuilder().fixture(FixtureType.crud);
		bean = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		// create the test data
		bean = CORE.getPersistence().save(bean);
	}

	@Test
	public void testToProxyCopiesExpectedAttributes() {
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
	public void testToProxyDoesNotCreateDuplicates() throws Exception {
		// validate the test data
		assertThat(CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME).beanResults()
				.size(), is(2));

		// call the method under test
		UserProxyExtension result = bean.toUserProxy();
		result = CORE.getPersistence().save(result);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME).beanResults()
				.size(), is(2));
	}
}
