package services.report.freemarker;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Writer;
import java.util.Base64;
import java.util.Iterator;
import java.util.Map;

import javax.imageio.ImageIO;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.model.document.DynamicImage.ImageFormat;
import org.skyve.metadata.module.Module;

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
 * FreeMarker user-defined directive that loads a specified Skyve {@link DynamicImage}.
 *
 * <p>
 * <b>Directive info</b>
 * </p>
 *
 * <p>
 * Directive parameters:
 * <ul>
 * <li><code>image</code>: The name of the image
 * <li><code>module</code>: The name of the module which contains the DynamicImage
 * <li><code>document</code>: The name of the document which contains the DynamicImage
 * <li><code>bean</code>: The bean which the image belongs to
 * <li><code>height</code>: Height of the image element, can be null
 * <li><code>width</code>: Width of the image element, can be null
 * </ul>
 * <p>
 * Loop variables: None
 * <p>
 * Directive nested content: No
 * 
 * <p>
 * Usage: <code><@dynamicImage image="Chart" module="admin" document="User" bean=user /></code>.
 * </p>
 */
public class DynamicImageDirective implements TemplateDirectiveModel {

	private static final String PARAM_NAME_BEAN = "bean";
	private static final String PARAM_NAME_IMAGE = "image";
	private static final String PARAM_NAME_DOCUMENT = "document";
	private static final String PARAM_NAME_MODULE = "module";
	private static final String PARAM_HEIGHT = "height";
	private static final String PARAM_WIDTH = "width";

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
		String imageParam = null,
				documentParam = null,
				moduleParam = null,
				heightParam = null,
				widthParam = null;
		Bean beanParam = null;

		Integer width = Integer.valueOf(300),
				height = Integer.valueOf(300);

		Iterator paramIter = params.entrySet().iterator();
		while (paramIter.hasNext()) {
			Map.Entry ent = (Map.Entry) paramIter.next();

			String paramName = (String) ent.getKey();
			TemplateModel paramValue = (TemplateModel) ent.getValue();

			if (paramName.equals(PARAM_NAME_IMAGE)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_NAME_IMAGE));
				}
				imageParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_NAME_DOCUMENT)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_NAME_DOCUMENT));
				}
				documentParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_NAME_MODULE)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_NAME_MODULE));
				}
				moduleParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_NAME_BEAN)) {
				// unwrap to try get the skyve object
				Object beanObj = DeepUnwrap.permissiveUnwrap(paramValue);
				if (!(beanObj instanceof Bean)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a Skyve bean.", PARAM_NAME_BEAN));
				}
				beanParam = (Bean) beanObj;
			} else if (paramName.equals(PARAM_HEIGHT)) {
				if (paramValue instanceof TemplateScalarModel) {
					heightParam = ((TemplateScalarModel) paramValue).getAsString();
				} else if (paramValue instanceof TemplateNumberModel) {
					heightParam = ((TemplateNumberModel) paramValue).getAsNumber().toString();
				} else {
					throw new TemplateModelException(
							String.format("The '%s' parameter must be a String or an Integer.", PARAM_HEIGHT));
				}

				try {
					height = Integer.parseInt(heightParam);
				} catch (NumberFormatException nfe) {
					throw new TemplateModelException(
							String.format("The '%s' parameter (%s) is not a valid height integer value", PARAM_HEIGHT,
									heightParam));
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

				try {
					width = Integer.parseInt(widthParam);
				} catch (NumberFormatException nfe) {
					throw new TemplateModelException(
							String.format("The '%s' parameter (%s) is not a valid width integer value", PARAM_WIDTH,
									widthParam));
				}
			} else {
				throw new TemplateModelException(
						"Unsupported parameter: " + paramName);
			}
		}

		if (imageParam == null) {
			throw new TemplateModelException("Parameter '" + PARAM_NAME_IMAGE + "' is required");
		}
		if (documentParam == null) {
			throw new TemplateModelException("Parameter '" + PARAM_NAME_DOCUMENT + "' is required");
		}
		if (moduleParam == null) {
			throw new TemplateModelException("Parameter '" + PARAM_NAME_MODULE + "' is required");
		}
		if (beanParam == null) {
			throw new TemplateModelException("Parameter '" + PARAM_NAME_BEAN + "' is required");
		}

		// get the Skyve Document for the image
		Customer customer = CORE.getCustomer();
		Module module = customer.getModule(moduleParam);

		if (module == null) {
			throw new TemplateModelException(
					"Module '" + moduleParam + "' could not be found. Please check the spelling and update the markup.");
		}
		Document document = module.getDocument(customer, documentParam);
		if (document == null) {
			throw new TemplateModelException(
					"Document '" + documentParam + "' could not be found. Please check the spelling and update the markup.");
		}

		DynamicImage<Bean> dynamicImage = document.getDynamicImage(customer, imageParam);
		try {
			BufferedImage image = dynamicImage.getImage(beanParam, width.intValue(), height.intValue(), CORE.getUser());

			try {
				ImageFormat format = dynamicImage.getFormat();
				if (format == null) {
					format = ImageFormat.png;
				}

				// convert the dynamic image to a byte array
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ImageIO.write(image, format.toString(), baos);
				byte[] bytes = baos.toByteArray();

				// do the actual directive execution
				Writer out = env.getOut();
				if (bytes.length > 0) {
					// encode file to base64
					StringBuilder s = new StringBuilder();
					s.append(String.format("<img src='data:%s;base64,%s'",
							format.getMimeType().toString(),
							Base64.getEncoder().encodeToString(bytes)));

					if (heightParam != null) {
						s.append(" height='").append(heightParam).append("'");
					}

					if (widthParam != null) {
						s.append(" width='").append(widthParam).append("'");
					}

					s.append(" />");

					System.out.println("@dynamicImage output: " + s.toString());

					out.write(s.toString());
				}

			} catch (Exception e) {
				image.flush();
			}
		} catch (Exception e) {
			throw new TemplateModelException("Error retrieving the dynamic image", e);
		}
	}
}
