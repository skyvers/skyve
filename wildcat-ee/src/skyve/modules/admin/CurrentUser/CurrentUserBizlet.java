package modules.admin.CurrentUser;

import java.util.ArrayList;
import java.util.List;

import modules.admin.domain.CurrentUser;
import modules.admin.domain.Group;
import modules.admin.domain.User;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

public class CurrentUserBizlet extends Bizlet<CurrentUser> {
	private static final long serialVersionUID = -6841455574804123970L;

	@Override
	public CurrentUser newInstance(CurrentUser bean) throws Exception {
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		q.getFilter().addEquals(Binder.createCompoundBinding(User.contactPropertyName, Bean.DOCUMENT_ID), pers.getUser().getContactId());
		
		User user = q.beanResult();
		if (user != null) {
			bean.setCurrentUser(user);
			for(Group g: user.getGroups()){
				bean.getGroups().add(g);
			}
		}
			
		return bean;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String fieldName) throws Exception {
		Persistence persistence = CORE.getPersistence();
		
		if (User.groupsPropertyName.equals(fieldName)) {
			DocumentQuery query = persistence.newDocumentQuery(Group.MODULE_NAME, Group.DOCUMENT_NAME);
			query.addOrdering(Group.namePropertyName, SortDirection.ascending);
			List<Group> groups = query.beanResults();
			List<DomainValue> result = new ArrayList<>(groups.size());
			for (Group group : groups) {
				result.add(new DomainValue(group.getBizId(), group.getBizKey()));
			}

			return result;
		} 
		
		return super.getVariantDomainValues(fieldName);
	}

}
