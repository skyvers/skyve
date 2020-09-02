package modules.admin.SelfRegistrationActivation;

import org.skyve.CORE;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import modules.admin.User.UserExtension;
import modules.admin.domain.SelfRegistrationActivation;
import modules.admin.domain.User;

public class SelfRegistrationActivationExtension extends SelfRegistrationActivation {

	/**
	 * 
	 */
	private static final long serialVersionUID = -852587779096146278L;

	public User activateUser(String activationCode) {
		Persistence p = CORE.getPersistence();
		DocumentQuery userQuery = p.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		userQuery.getFilter().addEquals(User.activationCodePropertyName, activationCode);

		UserExtension user = userQuery.beanResult();

		try {
			if (user == null) {
				Util.LOGGER.warning("No user exists for activation code=" + activationCode);
				setResult(Result.FAILURE);
			} else if (Boolean.TRUE.equals(user.getActivated())) {
				// User already activated, prompt them to login
				Util.LOGGER.warning("User=" + user.getUserName() + " already activated");
				setUser(user);
				setResult(Result.ALREADYACTIVATED);
			} else {
				user.setActivated(Boolean.TRUE);
				user = p.save(user);
				
				setUser(user);
				setResult(Result.SUCCESS);
			}
			return user;
		} catch (Exception e) {
			throw e;
		}
	}

	@Override
	public String getLoginUrl() {
		return Util.getSkyveContextUrl() + "/login";
	}

}
