package modules.admin.Group;

import java.util.List;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import jakarta.inject.Inject;
import modules.admin.User.UserService;
import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;

/**
 * Extends {@link Group} with lazily-expanded candidate role synthesis.
 * <p>
 * Candidate roles are derived from configured customer roles and merged with
 * currently assigned roles so UI pickers can offer both existing and available
 * role options.
 */
public class GroupExtension extends Group {
	private static final long serialVersionUID = 2678377209921744911L;

	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	private transient UserService userService;

	private boolean determinedCandidateRoles = false;

	/**
	 * Returns candidate roles, populating them on first access for this bean state.
	 *
	 * @return the candidate role list
	 */
	@Override
	public List<GroupRole> getCandidateRoles() {
		if (!determinedCandidateRoles) {
			List<GroupRole> candidates = super.getCandidateRoles();
			candidates.clear();
			candidates.addAll(getRoles());
			for (DomainValue value : userService.getCustomerRoleValues(CORE.getUser())) {
				if (!candidates.stream().anyMatch(c -> c.getRoleName().equals(value.getCode()))) {
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

	/**
	 * Clears the lazy-resolution flag so candidate roles can be recomputed.
	 */
	void resetCandidateRoles() {
		determinedCandidateRoles = false;
	}
}
