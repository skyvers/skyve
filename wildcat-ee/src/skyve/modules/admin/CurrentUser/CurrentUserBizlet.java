package modules.admin.CurrentUser;

import modules.admin.domain.CurrentUser;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import modules.admin.domain.UserLoginRecord;
import modules.admin.domain.UserRole;

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
		DocumentQuery qUsers = pers.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		qUsers.getFilter().addEquals(Binder.createCompoundBinding(User.contactPropertyName, Bean.DOCUMENT_ID), pers.getUser().getContactId());
		
		User user = qUsers.beanResult();
		if (user != null) {
			bean.setCurrentUser(user);
			for(Group g: user.getGroups()){
				bean.getGroups().add(g);
			}
			for(UserRole r: user.getRoles()){
				bean.getRoles().add(r);
			}

			//get last login time
			DocumentQuery qLogins = pers.newDocumentQuery(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
			qLogins.getFilter().addEquals(UserLoginRecord.userNamePropertyName, user.getUserName());
			qLogins.addOrdering(UserLoginRecord.loginDateTimePropertyName, SortDirection.descending);
			
			//we only want the second login (not our current log in)
			qLogins.setFirstResult(1).setMaxResults(1);
			
			//TODO - nothing appears to be being retrieved
			UserLoginRecord lastLogin = qLogins.beanResult();
			if(lastLogin!=null){
				bean.setLastLogin(lastLogin.getLoginDateTime());
			}
		}
		
			
		return bean;
	}
}
