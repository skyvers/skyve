package modules.admin.GroupRole;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;

import modules.admin.User.UserBizlet;
import modules.admin.domain.GroupRole;

public class GroupRoleBizlet extends Bizlet<GroupRole> 
{
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = -3329006728591283654L;

	@Override
	public List<DomainValue> getVariantDomainValues(String fieldName)
	throws Exception 
	{
		if (fieldName.equals(GroupRole.roleNamePropertyName)) 
		{
			return UserBizlet.getCustomerRoleValues(CORE.getUser());
		}

		return super.getVariantDomainValues(fieldName);
	}
}
