package org.skyve.impl.bind;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.Role;
import org.skyve.util.ExpressionEvaluator;

/**
 * Resolves {@code {role:module.role}} expressions to booleans based on user membership.
 */
public class RoleExpressionEvaluator extends ExpressionEvaluator {
	public static final String PREFIX = "role";

	/**
	 * Creates a role membership expression evaluator.
	 */
	public RoleExpressionEvaluator() {
		// default constructor
	}
	
	/**
	 * Evaluates whether the current user belongs to a module role.
	 *
	 * @param expression the role expression in the format {@code module.role}
	 * @param bean ignored for role expressions
	 * @return {@link Boolean#TRUE} when the user is in the role, otherwise {@link Boolean#FALSE}
	 */
	@Override
	public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
		int dotIndex = expression.indexOf('.');
		return CORE.getUser().isInRole(expression.substring(0, dotIndex),
										expression.substring(dotIndex + 1)) ?
				Boolean.TRUE :
				Boolean.FALSE;
	}

	/**
	 * Formats role membership evaluation for display.
	 *
	 * @param expression the role expression in the format {@code module.role}
	 * @param bean ignored for role expressions
	 * @return the display representation of the membership boolean
	 */
	@Override
	public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), evaluateWithoutPrefixOrSuffix(expression, bean));
	}
	
	/**
	 * Validates role expression syntax.
	 *
	 * @param expression the role expression being validated
	 * @param returnType the expected return type
	 * @param customer the customer context
	 * @param module the module context
	 * @param document the document context
	 * @return a validation error message when malformed, otherwise {@code null}
	 */
	@Override
	public String validateWithoutPrefixOrSuffix(String expression,
													Class<?> returnType,
													Customer customer,
													Module module,
													Document document) {
		if (expression.indexOf('.') <= 0) {
			return "Role " + expression + " needs to be in the format <module>.<role>";
		}
		return null;
	}
	
	/**
	 * Lists available module-role names for completion.
	 *
	 * @param fragment the partial expression fragment
	 * @param customer the customer metadata source
	 * @param module the module context
	 * @param document the document context
	 * @return matching role names in {@code module.role} format
	 */
	@Override
	public List<String> completeWithoutPrefixOrSuffix(String fragment,
														Customer customer,
														Module module,
														Document document) {
		List<String> result = new ArrayList<>();
		
		for (Module m : customer.getModules()) {
			String moduleName = m.getName();
			for (Role r : m.getRoles()) {
				String roleName = r.getName();
				String moduleRoleName = moduleName + '.' + roleName;
				if ((fragment == null) || moduleRoleName.startsWith(fragment)) {
					result.add(moduleRoleName);
				}
			}
		}
		
		return result;
	}
	
	/**
	 * Leaves role expressions unchanged because they are not binding-relative.
	 *
	 * @param expression the expression buffer
	 * @param binding ignored for role expressions
	 */
	@Override
	public void prefixBindingWithoutPrefixOrSuffix(StringBuilder expression, String binding) {
		// nothing to do here as the binding has nothing to do with role expressions
	}
}
