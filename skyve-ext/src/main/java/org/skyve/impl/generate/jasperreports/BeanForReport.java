package org.skyve.impl.generate.jasperreports;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

/**
 * Allows bean method calls from SQL type reports 
 * 
 * Exposes static helpers that Jasper report expressions can call to resolve
 * beans, users, formatted messages and condition checks.
 */
public class BeanForReport {

	/**
	 * Retrieves a bean instance for use by report expressions.
	 *
	 * @param moduleName The owning module name.
	 * @param documentName The document name within the module.
	 * @param bizId The business identifier of the row being rendered.
	 * @return The resolved bean, or {@code null} when no matching row exists.
	 */
	public static Bean getBean(String moduleName, String documentName, String bizId) {
		Persistence pers = CORE.getPersistence();
		return pers.retrieve(moduleName, documentName, bizId);
	}

	/**
	 * Returns the current report execution user.
	 *
	 * @return The current persistence user.
	 */
	public static User getUser() {
		return CORE.getPersistence().getUser();
	}

	/**
	 * Resolves a bean and formats a bound message expression against it.
	 *
	 * @param moduleName The owning module name.
	 * @param documentName The document name within the module.
	 * @param bizId The business identifier of the row being rendered.
	 * @param message The message pattern containing Skyve bindings.
	 * @return The formatted message text.
	 */
	public static String getMessage(String moduleName, String documentName, String bizId, String message) {
		Bean bean = getBean(moduleName, documentName, bizId);

		return getMessage(bean, message);
	}

	/**
	 * Formats a bound message expression against the supplied bean.
	 *
	 * @param bean The bean used as the message binding context.
	 * @param message The message pattern containing Skyve bindings.
	 * @return The formatted message text.
	 */
	public static String getMessage(Bean bean, String message) {
		return Binder.formatMessage(message, bean);
	}

	/**
	 * Evaluates a named bean condition for the supplied document row.
	 *
	 * @param moduleName The owning module name.
	 * @param documentName The document name within the module.
	 * @param bizId The business identifier of the row being rendered.
	 * @param conditionName The condition method name to evaluate.
	 * @return {@code true} when the condition evaluates to true for the bean.
	 */
	public static boolean evaluateCondition(String moduleName, String documentName, String bizId, String conditionName) {
		Bean bean = getBean(moduleName, documentName, bizId);
		return bean.evaluateCondition(conditionName);
	}
}
