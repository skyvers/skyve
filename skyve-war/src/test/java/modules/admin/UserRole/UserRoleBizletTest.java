package modules.admin.UserRole;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class UserRoleBizletTest {

	private static final UserRoleBizlet bizlet = new UserRoleBizlet();

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getVariantDomainValues("unknownAttribute"));
	}
}
