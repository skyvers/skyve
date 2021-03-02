package services.report.freemarker;

import java.io.IOException;
import java.io.Writer;
import java.util.Base64;
import java.util.Iterator;
import java.util.Map;

import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.util.Binder;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateNumberModel;
import freemarker.template.TemplateScalarModel;
import freemarker.template.utility.DeepUnwrap;

/**
 * FreeMarker user-defined directive which loads a specified content image from Skyve's content
 * repository and outputs a HTML img with a data URL set to the loaded image.
 *
 * <p>
 * <b>Directive info</b>
 * </p>
 *
 * <p>
 * Directive parameters:
 * <ul>
 * <li><code>bean</code>: The bean which contains the content attribute, can be a compound binding
 * <li><code>module</code>: The name of the module where the content attribute is located
 * <li><code>document</code>: The name of the document where the content attribute is located
 * <li><code>attribute</code>: The attribute binding for the content
 * <li><code>height</code>: Height of the image element, can be null
 * <li><code>width</code>: Width of the image element, can be null
 * <li><code>class</code>: Class(es) for the image element, can be null
 * <li><code>style</code>: Style(s) for the image element, can be null
 * </ul>
 * <p>
 * Loop variables: None
 * <p>
 * Directive nested content: No
 * 
 * <p>
 * Usage: <code><@content content=bean module="admin" document="Contact" attribute="image" /></code>.
 * </p>
 */
public class ContentDirective implements TemplateDirectiveModel {

	private static final String PARAM_BEAN = "bean";
	private static final String PARAM_MODULE = "module";
	private static final String PARAM_DOCUMENT = "document";
	private static final String PARAM_ATTRIBUTE = "attribute";
	private static final String PARAM_HEIGHT = "height";
	private static final String PARAM_WIDTH = "width";
	private static final String PARAM_CLASS = "class";
	private static final String PARAM_STYLE = "style";

	@Override
	@SuppressWarnings("rawtypes")
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
		String 	moduleParam = null,
				documentParam = null,
				attributeParam = null,
				heightParam = null,
				widthParam = null,
				classParam = null,
				styleParam = null;

		Iterator paramIter = params.entrySet().iterator();
		while (paramIter.hasNext()) {
			Map.Entry ent = (Map.Entry) paramIter.next();

			String paramName = (String) ent.getKey();
			TemplateModel paramValue = (TemplateModel) ent.getValue();

			if (paramName.equals(PARAM_BEAN)) {
				// unwrap to try get the skyve object
				Object beanObj = DeepUnwrap.permissiveUnwrap(paramValue);
				if (!(beanObj instanceof Bean)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a Skyve bean.", PARAM_BEAN));
				}
				beanParam = (Bean) beanObj;
			} else if (paramName.equals(PARAM_MODULE)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_MODULE));
				}
				moduleParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_DOCUMENT)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_DOCUMENT));
				}
				documentParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_ATTRIBUTE)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_ATTRIBUTE));
				}
				attributeParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_HEIGHT)) {
				if (paramValue instanceof TemplateScalarModel) {
					heightParam = ((TemplateScalarModel) paramValue).getAsString();
				} else if (paramValue instanceof TemplateNumberModel) {
					heightParam = ((TemplateNumberModel) paramValue).getAsNumber().toString();
				} else {
					throw new TemplateModelException(
							String.format("The '%s' parameter must be a String or an Integer.", PARAM_HEIGHT));
				}
			} else if (paramName.equals(PARAM_WIDTH)) {
				if (paramValue instanceof TemplateScalarModel) {
					widthParam = ((TemplateScalarModel) paramValue).getAsString();
				} else if (paramValue instanceof TemplateNumberModel) {
					widthParam = ((TemplateNumberModel) paramValue).getAsNumber().toString();
				} else {
					throw new TemplateModelException(
							String.format("The '%s' parameter must be a String or an Integer.", PARAM_WIDTH));
				}
			} else if (paramName.equals(PARAM_CLASS)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_CLASS));
				}
				classParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_STYLE)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_STYLE));
				}
				styleParam = ((TemplateScalarModel) paramValue).getAsString();
			} else {
				throw new TemplateModelException(
						"Unsupported parameter: " + paramName);
			}
		}

		if (attributeParam == null) {
			throw new TemplateModelException("Parameter '" + PARAM_ATTRIBUTE + "' is required");
		}
		if (documentParam == null) {
			throw new TemplateModelException("Parameter '" + PARAM_DOCUMENT + "' is required");
		}
		if (moduleParam == null) {
			throw new TemplateModelException("Parameter '" + PARAM_MODULE + "' is required");
		}
		if (beanParam == null) {
			throw new TemplateModelException("Parameter '" + PARAM_BEAN + "' is required");
		}

		// do the actual directive execution
		Writer out = env.getOut();
		if (beanParam != null && moduleParam != null && documentParam != null && attributeParam != null) {
			try (ContentManager cm = EXT.newContentManager()) {
				String content = (String) Binder.get(beanParam, attributeParam);
				if (content != null) {
					AttachmentContent ac = cm.get(content);
					byte[] fileBytes = ac.getContentBytes();

					StringBuilder s = new StringBuilder();
					s.append(String.format("<img src='data:%s;base64,%s'",
							ac.getMimeType().toString(),
							Base64.getEncoder().encodeToString(fileBytes)));

					if (attributeParam != null) {
						s.append(" alt='").append(attributeParam).append("'");
					}

					if (heightParam != null) {
						s.append(" height='").append(heightParam).append("'");
					}

					if (widthParam != null) {
						s.append(" width='").append(widthParam).append("'");
					}

					if (classParam != null) {
						s.append(" class='").append(classParam).append("'");
					}

					if (styleParam != null) {
						s.append(" style='").append(styleParam).append("'");
					}

					s.append(" />");

					// System.out.println("@content output: " + s.toString());
					out.write(s.toString());
				}
			} catch (Exception e) {
				throw new TemplateModelException("Error retrieving content: " + attributeParam, e);
			}
		}
	}
}
