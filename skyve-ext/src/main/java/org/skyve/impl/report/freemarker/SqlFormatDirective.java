package org.skyve.impl.report.freemarker;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.customer.Customer;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.utility.DeepUnwrap;

/**
 * FreeMarker user-defined directive that performs a Skyve display format on the specified value
 * based on the Java class.
 *
 * <p>
 * <b>Directive info</b>
 * </p>
 *
 * <p>
 * Directive parameters:
 * <ul>
 * <li><code>value</code>: The value to format
 * </ul>
 * <p>
 * Loop variables: None
 * <p>
 * Directive nested content: No
 * 
 * <p>
 * Usage: <code><@sqlformat value=bean.dateCreated /></code>.
 * </p>
 */
public class SqlFormatDirective implements TemplateDirectiveModel {
	private static final String PARAM_NAME_VALUE = "value";

	@Override
	public void execute(Environment env, Map params, TemplateModel[] loopVars, TemplateDirectiveBody body)
	throws TemplateException, IOException {
		if (params.isEmpty()) {
			throw new TemplateModelException("This directive requires parameters.");
		}

		if (loopVars.length != 0) {
			throw new TemplateModelException("This directive doesn't allow loop variables.");
		}

		// If there is non-empty nested content:
		if (body != null) {
			throw new TemplateModelException("This directive doesn't allow nested content.");
		}

		// process the parameters
		Object valueParam = null;

		Iterator<?> paramIter = params.entrySet().iterator();
		while (paramIter.hasNext()) {
			Map.Entry<?, ?> ent = (Map.Entry<?, ?>) paramIter.next();

			String paramName = (String) ent.getKey();
			TemplateModel paramValue = (TemplateModel) ent.getValue();

			if (paramName.equals(PARAM_NAME_VALUE)) {
				// unwrap to try get the skyve object
				valueParam = DeepUnwrap.permissiveUnwrap(paramValue);
			}
			else {
				throw new TemplateModelException("Unsupported parameter: " + paramName);
			}
		}

		// do the actual directive execution
		try (Writer out = env.getOut()) {
			if (valueParam != null) {
				Customer customer = CORE.getCustomer();
				if (customer == null) {
					throw new TemplateModelException("No customer available, unable to format.");
				}
				out.write(BindUtil.toDisplay(customer, null, null, valueParam));
			}
		}
	}
}
