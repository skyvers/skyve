package org.skyve.impl.report.freemarker;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.lang3.StringEscapeUtils;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.util.Binder;

import freemarker.core.Environment;
import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import freemarker.template.utility.DeepUnwrap;

/**
 * FreeMarker user-defined directive that performs a Skyve display format on the specified value.
 *
 * <p>
 * <b>Directive info</b>
 * </p>
 *
 * <p>
 * Directive parameters:
 * <ul>
 * <li><code>bean</code>: The bean which the attribute to format belongs to
 * <li><code>binding</code>: The attribute name to format, can be a compound binding
 * </ul>
 * <p>
 * Loop variables: None
 * <p>
 * Directive nested content: No
 * 
 * <p>
 * Usage: <code><@format bean=fund binding="dateEff" /></code>.
 * </p>
 */
public class FormatDirective implements TemplateDirectiveModel {
	private static final String PARAM_NAME_BEAN = "bean";
	private static final String PARAM_NAME_BINDING = "binding";
	private static final String PARAM_ESCAPE = "escape";
	
	@SuppressWarnings("rawtypes")
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
				if (! (beanObj instanceof Bean)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a Skyve bean.", PARAM_NAME_BEAN));
				}
				beanParam = (Bean) beanObj;
			}
			else if (paramName.equals(PARAM_NAME_BINDING)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_NAME_BINDING));
				}
				bindingParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_ESCAPE)) {
				if (paramValue instanceof TemplateBooleanModel) {
					escapeParam = ((TemplateBooleanModel) paramValue).getAsBoolean();	
				} else if(paramValue instanceof TemplateScalarModel) {
					escapeParam = Boolean.parseBoolean(((TemplateScalarModel) paramValue).getAsString());
				} else {
					throw new TemplateModelException(String.format("The '%s' parameter must be a boolean.", PARAM_ESCAPE));
				}
			} else {
				throw new TemplateModelException("Unsupported parameter: " + paramName);
			}
		}

		// do the actual directive execution
		Writer out = env.getOut();
		if (beanParam != null && bindingParam != null) {
			String display = Binder.getDisplay(CORE.getCustomer(), beanParam, bindingParam);
			if (escapeParam) {
				display = StringEscapeUtils.escapeHtml4(display);
			}

			// replace /n with <br/>
			display = display.replace("\n", "<br/>");

			out.write(display);
		}
	}
}
