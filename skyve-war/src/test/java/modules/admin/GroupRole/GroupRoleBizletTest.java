package modules.admin.GroupRole;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.mock.MockWebContext;

import modules.admin.DataMaintenance.DataMaintenanceExtension;
import modules.admin.domain.GroupRole;

@SuppressWarnings("static-method")
class GroupRoleBizletTest {

	private static final GroupRoleBizlet bizlet = new GroupRoleBizlet();

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getVariantDomainValues("unknownAttribute"));
	}

	@Test
	void resolveWithUnrelatedConversationBeanReturnsNull() throws Exception {
		// conversationBean is not GroupExtension or UserExtension, so returns null
		DataMaintenanceExtension unrelated = new DataMaintenanceExtension();
		GroupRole result = bizlet.resolve("some-biz-id", unrelated, new MockWebContext());
		assertNull(result);
	}
}
