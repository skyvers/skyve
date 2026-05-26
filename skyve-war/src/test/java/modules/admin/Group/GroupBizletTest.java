package modules.admin.Group;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.domain.Group;

@SuppressWarnings("static-method")
public class GroupBizletTest {

	private static final GroupBizlet bizlet = new GroupBizlet();

	@Test
	void getDynamicDomainValuesForRolesWithNoCandidatesReturnsEmptyList() throws Exception {
		GroupExtension bean = new GroupExtension();
		// No candidate roles by default
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
