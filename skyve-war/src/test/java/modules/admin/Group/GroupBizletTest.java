package modules.admin.Group;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.User.UserService;
import modules.admin.domain.Group;

@SuppressWarnings("static-method")
public class GroupBizletTest {

	private static final GroupBizlet bizlet = new GroupBizlet();

	@Test
	void getDynamicDomainValuesForRolesWithNoCandidatesReturnsEmptyList() throws Exception {
		GroupExtension bean = new GroupExtension();
		UserService mockUserService = mock(UserService.class);
		when(mockUserService.getCustomerRoleValues(any())).thenReturn(Collections.emptyList());
		Field field = GroupExtension.class.getDeclaredField("userService");
		field.setAccessible(true);
		field.set(bean, mockUserService);

		List<DomainValue> result = bizlet.getDynamicDomainValues(Group.rolesPropertyName, bean);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void getDynamicDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		GroupExtension bean = new GroupExtension();
		List<DomainValue> result = bizlet.getDynamicDomainValues("unknownAttribute", bean);
		assertNull(result);
	}
}
