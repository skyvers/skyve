package modules.admin.SelfRegistrationActivation;

import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.util.TimeUtil;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import modules.admin.User.UserExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.SelfRegistrationActivation;
import modules.admin.domain.User;

public class SelfRegistrationActivationExtension extends SelfRegistrationActivation {
	private static final long serialVersionUID = -852587779096146278L;

	public UserExtension activateUser(String activationCode) {
		Persistence p = CORE.getPersistence();
		DocumentQuery userQuery = p.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		userQuery.getFilter().addEquals(User.activationCodePropertyName, activationCode);

		// temporarily escalate access to query and save users
		p.setDocumentPermissionScopes(DocumentPermissionScope.customer);
		UserExtension user = null;
		try {
			user = userQuery.beanResult();

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

					boolean expired = false;
					// check for expiry of activation code
					Configuration configuration = Configuration.newInstance();
					if (configuration.getSelfRegistrationActivationExpiryHours() != null) {
						DateTime expiryDateTime = user.getActivationCodeCreationDateTime();
						DateTime now = new DateTime();
						if (expiryDateTime != null) {
							TimeUtil.addHours(expiryDateTime, configuration.getSelfRegistrationActivationExpiryHours().intValue());
							if (now.after(expiryDateTime)) {
								expired = true;
							}

						}
					}
					if (!expired) {
						user.setActivated(Boolean.TRUE);
						user = p.save(user);

						setUser(user);
						setResult(Result.SUCCESS);
					} else {
						setResult(Result.EXPIRED);
					}
				}
				return user;
			} catch (Exception e) {
				throw e;
			}
		} finally {
			p.resetDocumentPermissionScopes();
		}
	}

	@Override
	public String getLoginUrl() {
		return Util.getSkyveContextUrl() + "/login";
	}

	@Override
	public String getPleaseSignIn() {
		return Util.i18n("admin.selfRegistrationActivation.pleaseSignIn", this.getUser().getContact().getName(), this.getLoginUrl(),
				this.getUser().getContact().getEmail1());
	}

	@Override
	public String getSignInLink() {
		return Util.i18n("admin.selfRegistrationActivation.signInLink", this.getLoginUrl());
	}

	@Override
	public String getAlreadyActivated() {
		return Util.i18n("admin.selfRegistrationActivation.alreadyActivated", this.getUser().getContact().getName(),
				this.getLoginUrl());
	}

	@Override
	public String getNoLongerValid() {
		return Util.i18n("admin.selfRegistrationActivation.noLongerValid", this.getLoginUrl());
	}

	@Override
	public String getNotRecognised() {
		return Util.i18n("admin.selfRegistrationActivation.notRecognised", this.getLoginUrl());
	}
}
