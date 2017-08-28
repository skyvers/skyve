package modules.admin.UserDashboard;

import java.util.List;

import modules.admin.domain.UserDashboard;
import modules.admin.domain.Group;
import modules.admin.domain.Job;
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

public class UserDashboardBizlet extends Bizlet<UserDashboard> {
	private static final long serialVersionUID = -6841455574804123970L;

	@Override
	public UserDashboard newInstance(UserDashboard bean) throws Exception {
		Persistence pers = CORE.getPersistence();
		DocumentQuery qUsers = pers.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		qUsers.getFilter().addEquals(Binder.createCompoundBinding(User.contactPropertyName, Bean.DOCUMENT_ID), pers.getUser().getContactId());
		
		User user = qUsers.beanResult();
		if (user != null) {
			bean.setCurrentUser(user);
			
			StringBuilder sb = new StringBuilder(64);
			for(Group g: user.getGroups()){
				bean.getGroups().add(g);
				if(sb.length()>0){
					sb.append(',');
				}
				sb.append(g.getName());
			}
			bean.setGroupMembershipList(sb.toString());
			
			for(UserRole r: user.getRoles()){
				bean.getRoles().add(r);
			}
			
			//find jobs
			//TODO - work out why I only get one job
			DocumentQuery qJobs = pers.newDocumentQuery(Job.MODULE_NAME, Job.DOCUMENT_NAME);
			qJobs.getFilter().addEquals(Bean.USER_ID, bean.getCurrentUser().getBizId());
			
			List<Job> jobs = qJobs.beanResults();
			for(Job job:jobs){
				bean.getJobs().add(job);
			}

			//get last login time
			DocumentQuery qLogins = pers.newDocumentQuery(UserLoginRecord.MODULE_NAME, UserLoginRecord.DOCUMENT_NAME);
			qLogins.getFilter().addEquals(UserLoginRecord.userNamePropertyName, user.getUserName());
			qLogins.addBoundOrdering(UserLoginRecord.loginDateTimePropertyName, SortDirection.descending);
			
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
