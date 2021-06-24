package org.skyve.impl.report.freemarker;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.util.Base64;
import java.util.Iterator;
import java.util.Map;

import org.skyve.CORE;

import freemarker.core.Environment;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;

/**
 * FreeMarker user-defined directive that loads a specified resource using Skyve's fall-through resource handling.
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
 * </ul>
 * <p>
 * Loop variables: None
 * <p>
 * Directive nested content: No
 * 
 * <p>
 * Usage: <code><@resource filename="logo.png" module="admin" /></code>.
 * </p>
 */
public class ResourceDirective implements TemplateDirectiveModel {
	private static final String PARAM_NAME_FILENAME = "filename";
	private static final String PARAM_NAME_MODULE = "module";

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
		String filenameParam = null,
				moduleParam = null;

		Iterator<?> paramIter = params.entrySet().iterator();
		while (paramIter.hasNext()) {
			Map.Entry<?, ?> ent = (Map.Entry<?, ?>) paramIter.next();

			String paramName = (String) ent.getKey();
			TemplateModel paramValue = (TemplateModel) ent.getValue();

			if (paramName.equals(PARAM_NAME_FILENAME)) {
				if (! (paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_NAME_FILENAME));
				}
				filenameParam = ((TemplateScalarModel) paramValue).getAsString();
			}
			else if (paramName.equals(PARAM_NAME_MODULE)) {
				if (! (paramValue instanceof TemplateScalarModel)) {
					throw new TemplateModelException(String.format("The '%s' parameter must be a String.", PARAM_NAME_MODULE));
				}
				moduleParam = ((TemplateScalarModel) paramValue).getAsString();
			}
			else {
				throw new TemplateModelException("Unsupported parameter: " + paramName);
			}
		}

		// do the actual directive execution
		try (Writer out = env.getOut()) {
			if (filenameParam != null) {
				File resourceFile = CORE.getRepository().findResourceFile(filenameParam, CORE.getCustomer().getName(), moduleParam);
				if (resourceFile != null && resourceFile.exists()) {
					// encode file to base64
					byte[] fileContent = Files.readAllBytes(resourceFile.toPath());
					out.write(Base64.getEncoder().encodeToString(fileContent));
				}
			}
		}
	}
}
