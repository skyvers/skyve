package modules.admin.UserRole;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;

import modules.admin.User.UserBizlet;
import modules.admin.domain.UserRole;

public class UserRoleBizlet extends Bizlet<UserRole> {
	@Override
	public List<DomainValue> getVariantDomainValues(String fieldName) 
	throws Exception {
		if (UserRole.roleNamePropertyName.equals(fieldName)) {
			return UserBizlet.getCustomerRoleValues(CORE.getUser());
		}

		return super.getVariantDomainValues(fieldName);
	}
}
