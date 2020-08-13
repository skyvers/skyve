package services.report.freemarker;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.util.Base64;
import java.util.Iterator;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.content.MimeType;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateNumberModel;
import freemarker.template.TemplateScalarModel;

/**
 * FreeMarker user-defined directive which loads a specified resource using Skyve's fall-through resource handling
 * and outputs a HTML img with a data URL set to the loaded image.
 *
 * <p>
 * <b>Directive info</b>
 * </p>
 *
 * <p>
 * Directive parameters:
 * <ul>
 * <li><code>filename</code>: The name of the resource
 * <li><code>module</code>: The name of the module where the resource is located, can be null
 * <li><code>alt</code>: Alt text for the image element, can be null
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
 * Usage: <code><@image filename="logo.png" /></code>,
 * <code><@image filename="logo.png" module="admin" alt="Corporate logo" height="60" class="logo" /></code>.
 * </p>
 */
public class ImageDirective implements TemplateDirectiveModel {

	private static final String PARAM_FILENAME = "filename";
	private static final String PARAM_MODULE = "module";
	private static final String PARAM_ALT = "alt";
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
		String filenameParam = null,
				moduleParam = null,
				altParam = null,
				heightParam = null,
				widthParam = null,
				classParam = null,
				styleParam = null;

		Iterator paramIter = params.entrySet().iterator();
		while (paramIter.hasNext()) {
			Map.Entry ent = (Map.Entry) paramIter.next();

			String paramName = (String) ent.getKey();
			TemplateModel paramValue = (TemplateModel) ent.getValue();

			if (paramName.equals(PARAM_FILENAME)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_FILENAME));
				}
				filenameParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_MODULE)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_MODULE));
				}
				moduleParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_ALT)) {
				if (!(paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_ALT));
				}
				altParam = ((TemplateScalarModel) paramValue).getAsString();
			} else if (paramName.equals(PARAM_HEIGHT)) {
				if (paramValue instanceof TemplateScalarModel) {
					heightParam = ((TemplateScalarModel) paramValue).getAsString();
				} else if (paramValue instanceof TemplateNumberModel) {
					heightParam = ((TemplateNumberModel) paramValue).getAsNumber().toString();
				} else {
					throw new TemplateModelException(
							String.format("The '%s' parameter must be a String or an Integer.", PARAM_ALT));
				}
			} else if (paramName.equals(PARAM_WIDTH)) {
				if (paramValue instanceof TemplateScalarModel) {
					widthParam = ((TemplateScalarModel) paramValue).getAsString();
				} else if (paramValue instanceof TemplateNumberModel) {
					widthParam = ((TemplateNumberModel) paramValue).getAsNumber().toString();
				} else {
					throw new TemplateModelException(
							String.format("The '%s' parameter must be a String or an Integer.", PARAM_ALT));
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

		// do the actual directive execution
		Writer out = env.getOut();
		if (filenameParam != null) {
			File resourceFile = CORE.getRepository().findResourceFile(filenameParam, CORE.getCustomer().getName(), moduleParam);
			if (resourceFile != null && resourceFile.exists()) {
				// encode file to base64
				byte[] fileContent = Files.readAllBytes(resourceFile.toPath());

				String mimeType = parseMimeTypeFromFilename(filenameParam);

				StringBuilder s = new StringBuilder();
				s.append(String.format("<img src='data:%s;base64,%s'",
						mimeType,
						Base64.getEncoder().encodeToString(fileContent)));

				if (altParam != null) {
					s.append(" alt='").append(altParam).append("'");
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

				// System.out.println("@img output: " + s.toString());

				out.write(s.toString());
			} else {
				throw new TemplateModelException("File not found: " + filenameParam);
			}
		}
	}

	/**
	 * Guesses the mime type from the specified filename's extension.
	 * 
	 * @param filename The filename supplied to the directive
	 * @return The mime type of the file
	 * @throws TemplateModelException
	 */
	private String parseMimeTypeFromFilename(String filename) throws TemplateModelException {
		String ext = filename.substring(filename.lastIndexOf(".") + 1);

		if (ext.contentEquals("png")) {
			return MimeType.png.toString();
		} else if (ext.contentEquals("jpg") || ext.contentEquals("jpeg")) {
			return MimeType.jpeg.toString();
		} else if (ext.contentEquals("gif")) {
			return MimeType.gif.toString();
		} else if (ext.contentEquals(MimeType.bitmap.getStandardFileSuffix())) {
			return MimeType.bitmap.toString();
		} else if (ext.contentEquals(MimeType.tiff.getStandardFileSuffix())) {
			return MimeType.tiff.toString();
		}

		throw new TemplateModelException("Unsupported file type: " + ext);
	}
}
