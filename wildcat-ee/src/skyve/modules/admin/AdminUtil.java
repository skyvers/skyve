package modules.admin;

import modules.admin.domain.Configuration;
import modules.admin.domain.Configuration.PasswordComplexityModel;
import modules.admin.domain.DataMaintenance;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

public class AdminUtil {

	public static int MINIMUM_PASSWORD_LENGTH = 6;
	public static int MINIMUM_USERNAME_LENGTH = 4;

	public static String MINIMUM_PASSWORD_RULE = "Passwords must be 6 letters or more.";

	public static String MEDIUM_PASSWORD_RULE = "Passwords must be 6 letters or more and contain upper and lowercase and numeric characters.";

	public static String MAXIMUM_PASSWORD_RULE = "Passwords must be 8 letters or more, contain upper and lowercase, numeric characters and punctuation.";

	public static PasswordComplexityModel defaultPasswordComplexityModel = PasswordComplexityModel.mINIMUM;

	public static boolean validatePasswordComplexity(String candidatePassword, PasswordComplexityModel cm) {
		boolean result = false;

		if (candidatePassword != null && candidatePassword.length() >= MINIMUM_PASSWORD_LENGTH) {
			if (cm == PasswordComplexityModel.mINIMUM) {
				result = true;
			} else {
				if (candidatePassword.matches(".*[A-Z].*") && candidatePassword.matches(".*[a-z].*") && candidatePassword.matches(".*[0-9].*")) {
					// check for at least one Capital and one numeric
					if (cm == PasswordComplexityModel.mEDIUM) {
						result = true;
					} else {
						// check for inclusion of punctuation
						// PasswordComplexityModel.MAXIMUM
					}
				}
			}
		}

		return result;
	}

	public static String validatePasswordComplexityMessage(PasswordComplexityModel cm) {
		if (cm == PasswordComplexityModel.mINIMUM) {
			return MINIMUM_PASSWORD_RULE;
		} else if (cm == PasswordComplexityModel.mEDIUM) {
			return MEDIUM_PASSWORD_RULE;
		} else {
			return MAXIMUM_PASSWORD_RULE;
		}

	}

	public static Configuration getConfiguration() throws Exception {
		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Configuration.MODULE_NAME);
		Document document = module.getDocument(customer, Configuration.DOCUMENT_NAME);
		return document.newInstance(user);
	}

	public static DataMaintenance getDataMaintenance() throws Exception {
		User user = CORE.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(DataMaintenance.MODULE_NAME);
		Document document = module.getDocument(customer, DataMaintenance.DOCUMENT_NAME);

		DataMaintenance parm = document.newInstance(user);
		return parm;
	}	
}
