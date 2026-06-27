package modules.admin.Group;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Bizlet;

import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;

/**
 * Provides group-specific Bizlet behaviour for role selection and post-save cleanup.
 */
public class GroupBizlet extends Bizlet<GroupExtension> {
	/**
	 * Synthesises dynamic role domain values from candidate roles.
	 *
	 * @param attributeName
	 *        the attribute requesting dynamic values
	 * @param bean
	 *        the current group bean
	 * @return role-based values for {@link Group#rolesPropertyName}, otherwise superclass values
	 * @throws Exception
	 *         if value generation fails
	 */
	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, GroupExtension bean) throws Exception {
		if (Group.rolesPropertyName.equals(attributeName)) {
			List<GroupRole> candidates = bean.getCandidateRoles();
			List<DomainValue> result = new ArrayList<>(candidates.size());
			candidates.stream().forEach(role -> result.add(new DomainValue(role.getBizId(), role.getBizKey())));
			return result;
		}

		return super.getDynamicDomainValues(attributeName, bean);
	}
	
	/**
	 * Clears cached candidate roles after save.
	 *
	 * @param bean
	 *        the saved group bean
	 * @throws Exception
	 *         if post-save processing fails
	 */
	@Override
	public void postSave(GroupExtension bean) throws Exception {
		bean.resetCandidateRoles();
	}
}
