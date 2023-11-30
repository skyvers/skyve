package modules.admin.Group;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Bizlet;

import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;

public class GroupBizlet extends Bizlet<GroupExtension> {
	/**
	 * Synthesize DomainValues from the candidate roles collection. 
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
	 * Clear the candidate roles after group save.
	 */
	@Override
	public void postSave(GroupExtension bean) throws Exception {
		bean.resetCandidateRoles();
	}
}
