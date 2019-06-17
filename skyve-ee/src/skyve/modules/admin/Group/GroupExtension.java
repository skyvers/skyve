package modules.admin.Group;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import modules.admin.User.UserBizlet;
import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;

/**
 * Synthesize Group Roles from metadata.
 * @author mike
 */
public class GroupExtension extends Group {
	private static final long serialVersionUID = 2678377209921744911L;

	private boolean determinedCandidateRoles = false;

	@Override
	public List<GroupRole> getCandidateRoles() {
		if (! determinedCandidateRoles) {
			List<GroupRole> candidates = super.getCandidateRoles();
			candidates.clear();
			candidates.addAll(getRoles());
			for (DomainValue value : UserBizlet.getCustomerRoleValues(CORE.getUser())) {
				if (! candidates.stream().anyMatch(c -> c.getRoleName().equals(value.getCode()))) {
					GroupRole role = GroupRole.newInstance();
					role.setRoleName(value.getCode());
					role.setParent(this);
					role.originalValues().clear();
					candidates.add(role);
				}
			}
			
			determinedCandidateRoles = true;
		}
		return super.getCandidateRoles();
	}
	
	void resetCandidateRoles() {
		determinedCandidateRoles = false;
	}
}
