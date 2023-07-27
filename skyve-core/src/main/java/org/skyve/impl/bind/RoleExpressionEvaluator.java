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

public class RoleExpressionEvaluator extends ExpressionEvaluator {
	public static final String PREFIX = "role";
	
	@Override
	public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
		int dotIndex = expression.indexOf('.');
		return CORE.getUser().isInRole(expression.substring(0, dotIndex),
										expression.substring(dotIndex + 1)) ?
				Boolean.TRUE :
				Boolean.FALSE;
	}

	@Override
	public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
		return BindUtil.toDisplay(CORE.getCustomer(), null, null, evaluateWithoutPrefixOrSuffix(expression, bean));
	}
	
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
	
	@Override
	public void prefixBindingWithoutPrefixOrSuffix(StringBuilder expression, String binding) {
		// nothing to do here as the binding has nothing to do with role expressions
	}
}
