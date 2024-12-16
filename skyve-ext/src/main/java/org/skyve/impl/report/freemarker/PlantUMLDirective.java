package org.skyve.impl.report.freemarker;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Writer;
import java.util.Base64;
import java.util.Iterator;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.util.Binder;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import freemarker.template.utility.DeepUnwrap;
import net.sourceforge.plantuml.SourceStringReader;

/**
 * FreeMarker user-defined directive which generates a PlantUML image for the specified PlantUML
 * markup and outputs a HTML img with a data URL set to the generated diagram image.
 *
 * <p>
 * <b>Directive info</b>
 * </p>
 *
 * <p>
 * Directive parameters:
 * <ul>
 * <li><code>bean</code>: The bean which the attribute to get the PlantUML markup belongs to
 * <li><code>binding</code>: The attribute name to retrieve the markup for, can be a compound binding
 * <li><code>markup</code>: String markup to generate the diagram if not using a bean and a binding
 * </ul>
 * <p>
 * Loop variables: None
 * <p>
 * Directive nested content: No
 * 
 * <p>
 * Usage: <code><@plantUmlImage bean=contact binding="markup" /></code>,
 * 
 * <pre>
 * <@plantUmlImage markup="
 * &#64;startuml
 * object object01
 * object object02
 * object01 -- object02
 * @enduml" />
 * </pre>
 * </p>
 */
public class PlantUMLDirective implements TemplateDirectiveModel {
	private static final String PARAM_NAME_BEAN = "bean";
	private static final String PARAM_NAME_BINDING = "binding";
	private static final String PARAM_NAME_MARKUP = "markup";

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
		String markupParam = null;

		Iterator<?> paramIter = params.entrySet().iterator();
		while (paramIter.hasNext()) {
			Map.Entry<?, ?> entry = (Map.Entry<?, ?>) paramIter.next();

			String paramName = (String) entry.getKey();
			TemplateModel paramValue = (TemplateModel) entry.getValue();

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
			} else if (paramName.equals(PARAM_NAME_MARKUP)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_NAME_MARKUP));
				}
				markupParam = ((TemplateScalarModel) paramValue)
						.getAsString();
			} else {
				throw new TemplateModelException(
						"Unsupported parameter: " + paramName);
			}

			// check we have a bean and a binding, or a markup
			if ((beanParam == null || bindingParam == null) && markupParam == null) {
				throw new TemplateModelException(
						"Expected bean and binding, or markup parameter to be supplied.");
			}
		}

		// do the actual directive execution
		try (Writer out = env.getOut()) {
			if (beanParam != null && bindingParam != null) {
				String uml = Binder.getDisplay(CORE.getCustomer(), beanParam, bindingParam);
				out.write(generateImage(uml));
			} else if (markupParam != null) {
				out.write(generateImage(markupParam));
			}
		}
	}

	/**
	 * Converts PlantUML markup into a base64 encoded data url inside a HTML image tag.
	 * 
	 * @param uml The PlantUML markup
	 * @return A HTML image tag representing the diagram
	 */
	private static String generateImage(final String uml) {
		try {
			SourceStringReader reader = new SourceStringReader(uml);
			// System.out.println("Converting markup into image: " + uml);

			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			// Generate the image into a ByteArrayOutputStream
			reader.outputImage(baos);
			
			// Get the image as byte[]
			byte[] imageBytes = baos.toByteArray();
			
			// Convert to Base64 so that it can be embedded as a Data URL
			String base64 = Base64.getEncoder().encodeToString(imageBytes);
			String dataUrl = "data:image/png;base64," + base64 + "\'";
			
			StringBuilder s = new StringBuilder();
			s.append("<img src='")
					.append(dataUrl)
					.append(" />");
			
			return s.toString();
		} catch (IOException ioe) {
			throw new RuntimeException(ioe);
		}
	}
}
