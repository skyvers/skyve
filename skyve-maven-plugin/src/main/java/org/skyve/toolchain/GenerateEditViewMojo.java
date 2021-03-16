package org.skyve.toolchain;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Execute;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.skyve.impl.generate.ViewGenerator;
import org.skyve.toolchain.config.GenerateEditViewConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Generates a Skyve edit view.
 */
@Mojo(name = "generateEditView", requiresDependencyResolution = ResolutionScope.TEST)
@Execute(phase = LifecyclePhase.PROCESS_RESOURCES)
public class GenerateEditViewMojo extends AbstractSkyveMojo {
	private static final Logger LOGGER = LoggerFactory.getLogger(GenerateEditViewMojo.class);

	/**
	 * Relative source directory.
	 */
	@Parameter(required = true, defaultValue = "src/main/java/")
	private String srcDir;

	/**
	 * Customer name.
	 */
	@Parameter()
	private String customer;

	@Parameter()
	private GenerateEditViewConfig generateEditViewConfig;

	@Override
	public void execute() throws MojoExecutionException {
		try {
			final String configCustomerName = (generateEditViewConfig != null) ? generateEditViewConfig.getCustomer() : customer;
			final String customerName = getDefaultOrPromptCustomer(configCustomerName);

			final String configModuleName = (generateEditViewConfig != null) ? generateEditViewConfig.getModule() : null;
			final String moduleName = getDefaultOrPrompt(configModuleName, "Please enter a module name");

			final String configDocumentName = (generateEditViewConfig != null) ? generateEditViewConfig.getDocument() : null;
			final String documentName = getDefaultOrPrompt(configDocumentName, "Please enter a document name");

			final boolean isCustomerOverriden = (generateEditViewConfig != null) ? generateEditViewConfig.isCustomerOverriden() : false;

			final String overridenViewName = (generateEditViewConfig != null) ? generateEditViewConfig.getOverridenViewName() : null;

			configureClasspath(srcDir);
			ViewGenerator.main(new String[] {srcDir,
												customerName,
												moduleName,
												documentName,
												Boolean.toString(isCustomerOverriden),
												overridenViewName });
		}
		catch (Exception e) {
			LOGGER.error("Failed to generate edit view.", e);
			throw new MojoExecutionException("Failed to generate edit view.", e);
		}
	}
}
