package org.skyve.impl.report.freemarker;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.OWASP;

import freemarker.core.Environment;
import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import freemarker.template.utility.DeepUnwrap;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * FreeMarker user-defined directive that retrieves the description from the metadata for
 * the specified Skyve attribute.
 *
 * <p>
 * <b>Directive info</b>
 * </p>
 *
 * <p>
 * Directive parameters:
 * <ul>
 * <li><code>bean</code>: The bean which the attribute to get the description belongs to
 * <li><code>binding</code>: The attribute name to retrieve the description for, can be a compound binding
 * <li><code>escape</code>: Boolean (Optional), set false to explicitly turn off (disable) HTML escaping (on by default)
 * </ul>
 * <p>
 * Loop variables: None
 * <p>
 * Directive nested content: No
 * 
 * <p>
 * Usage: <code><@description bean=contact binding="contactType" /></code>.
 * </p>
 */
public class DescriptionDirective implements TemplateDirectiveModel {

	private static final String PARAM_NAME_BEAN = "bean";
	private static final String PARAM_NAME_BINDING = "binding";
	private static final String PARAM_ESCAPE = "escape";

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
		Bean beanParam = null;
		String bindingParam = null;
		boolean escapeParam = true;

		Iterator<?> paramIter = params.entrySet().iterator();
		while (paramIter.hasNext()) {
			Map.Entry<?, ?> ent = (Map.Entry<?, ?>) paramIter.next();

			String paramName = (String) ent.getKey();
			TemplateModel paramValue = (TemplateModel) ent.getValue();

			if (paramName.equals(PARAM_NAME_BEAN)) {
				// unwrap to try get the skyve object
				Object beanObj = DeepUnwrap.permissiveUnwrap(paramValue);
				if (!(beanObj instanceof Bean)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a Skyve bean.", PARAM_NAME_BEAN));
				}
				beanParam = (Bean) beanObj;
			} else if (paramName.equals(PARAM_NAME_BINDING)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_NAME_BINDING));
				}
				bindingParam = ((TemplateScalarModel) paramValue)
						.getAsString();
			} else if (paramName.equals(PARAM_ESCAPE)) {
				if (paramValue instanceof TemplateBooleanModel) {
					escapeParam = ((TemplateBooleanModel) paramValue).getAsBoolean();
				} else if (paramValue instanceof TemplateScalarModel) {
					escapeParam = Boolean.parseBoolean(((TemplateScalarModel) paramValue).getAsString());
				} else {
					throw new TemplateModelException(String.format("The '%s' parameter must be a boolean.", PARAM_ESCAPE));
				}
			} else {
				throw new TemplateModelException(
						"Unsupported parameter: " + paramName);
			}
		}

		// do the actual directive execution
		try (Writer out = env.getOut()) {
		if (beanParam != null && bindingParam != null) {
			String description = getDescription(beanParam, bindingParam);
			if (escapeParam) {
				description = OWASP.escapeHtml(description);
			}
			if (description != null) {
				out.write(description);
			}
		}
	}
	}

	/**
	 * Get the attribute description from the metadata
	 * 
	 * @param bean The bean
	 * @param fullyQualifiedPropertyName The compound binding to the required attribute
	 * @return The description of the attribute
	 */
	private static @Nullable String getDescription(@Nonnull final Bean bean,
														@Nonnull final String fullyQualifiedPropertyName) {
		final Customer customer = CORE.getCustomer();
		final Module module = customer.getModule(bean.getBizModule());
		final Document document = module.getDocument(customer, bean.getBizDocument());
		final TargetMetaData tmd = Binder.getMetaDataForBinding(customer, module, document, fullyQualifiedPropertyName);
		final Attribute ta = tmd.getAttribute();
		return (ta == null) ? null : ta.getDescription();
	}
}
