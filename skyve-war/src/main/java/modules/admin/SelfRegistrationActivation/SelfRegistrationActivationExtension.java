package modules.admin.SelfRegistrationActivation;

import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.util.TimeUtil;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Util;

import modules.admin.User.UserExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.SelfRegistrationActivation;
import modules.admin.domain.User;

public class SelfRegistrationActivationExtension extends SelfRegistrationActivation {
	private static final long serialVersionUID = -852587779096146278L;

	public UserExtension activateUser(String activationCode) {
		// temporarily escalate access to query and save users
		return CORE.getPersistence().withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery userQuery = p.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
			userQuery.getFilter().addEquals(User.activationCodePropertyName, activationCode);

			UserExtension result = userQuery.beanResult();
			if (result == null) {
				Util.LOGGER.warning("No user exists for activation code=" + activationCode);
				setResult(Result.FAILURE);
			}
			else if (Boolean.TRUE.equals(result.getActivated())) {
				// User already activated, prompt them to login
				Util.LOGGER.warning("User=" + result.getUserName() + " already activated");
				setUser(result);
				setResult(Result.ALREADYACTIVATED);
			}
			else {
				boolean expired = false;
				// check for expiry of activation code
				Configuration configuration = Configuration.newInstance();
				if (configuration.getSelfRegistrationActivationExpiryHours() != null) {
					DateTime expiryDateTime = result.getActivationCodeCreationDateTime();
					DateTime now = new DateTime();
					if (expiryDateTime != null) {
						TimeUtil.addHours(expiryDateTime, configuration.getSelfRegistrationActivationExpiryHours().intValue());
						if (now.after(expiryDateTime)) {
							expired = true;
						}

					}
				}
				if (!expired) {
					result.setActivated(Boolean.TRUE);
					result = p.save(result);

					setUser(result);
					setResult(Result.SUCCESS);
				}
				else {
					setResult(Result.EXPIRED);
				}
			}
			
			return result;
		});
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
