package modules.admin.UserList;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.domain.UserList;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UserListBizletH2Test extends AbstractH2Test {
	@Test
	void getVariantDomainValuesForDefaultModuleReturnsCustomerModules() throws Exception {
		List<DomainValue> values = new UserListBizlet().getVariantDomainValues(UserList.defaultModuleNamePropertyName);

		assertThat(values, is(notNullValue()));
		assertTrue(values.stream().anyMatch(value -> "admin".equals(value.getCode())));
	}

	@Test
	void getVariantDomainValuesForInvitationGroupsReturnsList() throws Exception {
		List<DomainValue> values = new UserListBizlet().getVariantDomainValues(UserList.userInvitationGroupsPropertyName);

		assertThat(values, is(notNullValue()));
	}

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsNullOrEmpty() throws Exception {
		List<DomainValue> values = new UserListBizlet().getVariantDomainValues("unknown");

		assertTrue((values == null) || values.isEmpty());
	}
}
