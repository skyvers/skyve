package modules.admin.UserRole;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;

import jakarta.inject.Inject;
import modules.admin.User.UserService;
import modules.admin.domain.UserRole;

public class UserRoleBizlet extends Bizlet<UserRole> {
	@Inject
	private transient UserService userService;
	
	@Override
	public List<DomainValue> getVariantDomainValues(String fieldName) 
	throws Exception {
		if (UserRole.roleNamePropertyName.equals(fieldName)) {
			return userService.getCustomerRoleValues(CORE.getUser());
		}

		return super.getVariantDomainValues(fieldName);
	}
}
