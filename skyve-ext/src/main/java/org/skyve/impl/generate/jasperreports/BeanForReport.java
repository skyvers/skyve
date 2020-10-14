package org.skyve.impl.generate.jasperreports;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

/**
 * Allows bean method calls from SQL type reports 
 * - allowing a consistent generation method for both bean and sql report templates
 * 
 * @author Robert
 *
 */
public class BeanForReport {

	public static Bean getBean(String moduleName, String documentName, String bizId) {
		Persistence pers = CORE.getPersistence();
		return pers.retrieve(moduleName, documentName, bizId);
	}

	public static User getUser() {
		return CORE.getPersistence().getUser();
	}

	public static String getMessage(String moduleName, String documentName, String bizId, String message) {
		Bean bean = getBean(moduleName, documentName, bizId);

		return getMessage(bean, message);
	}

	public static String getMessage(Bean bean, String message) {
		return Binder.formatMessage(message, bean);
	}

	public static boolean evaluateCondition(String moduleName, String documentName, String bizId, String conditionName) {
		Bean bean = getBean(moduleName, documentName, bizId);
		return bean.evaluateCondition(conditionName);
	}
}
