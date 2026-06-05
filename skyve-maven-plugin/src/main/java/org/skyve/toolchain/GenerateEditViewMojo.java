package org.skyve.toolchain;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Execute;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.skyve.impl.generate.ViewGenerator;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.toolchain.config.GenerateEditViewConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Generates the XML edit view for a Skyve document.
 *
 * <p>Threading: this mojo mutates generated source trees and repository wiring and should be treated as
 * thread-confined.
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
	@Parameter(property = "customer")
	private String customer;

	@Parameter()
	private GenerateEditViewConfig generateEditViewConfig;

	/**
	 * Resolves the customer, module, and document names and generates the edit view XML.
	 *
	 * @throws MojoExecutionException if generation fails
	 */
	@Override
	public void execute() throws MojoExecutionException {
		try {
			final String configCustomerName = (generateEditViewConfig != null) ? generateEditViewConfig.getCustomer() : customer;
			final String customerName = getDefaultOrPromptCustomer(configCustomerName);

			final String configModuleName = (generateEditViewConfig != null) ? generateEditViewConfig.getModule() : null;
			final String moduleName = getDefaultOrPrompt(configModuleName, "Please enter a module name");

			final String configDocumentName = (generateEditViewConfig != null) ? generateEditViewConfig.getDocument() : null;
			final String documentName = getDefaultOrPrompt(configDocumentName, "Please enter a document name");

			final boolean isCustomerOverriden = (generateEditViewConfig != null) && generateEditViewConfig.isCustomerOverriden();

			final String overridenViewName = (generateEditViewConfig != null) ? generateEditViewConfig.getOverridenViewName() : null;

			configureClasspath(srcDir);
			setRepository();
			generateEditView(new String[] {srcDir,
											customerName,
											moduleName,
											documentName,
											Boolean.toString(isCustomerOverriden),
											overridenViewName });
		}
		catch (Exception e) {
			LOGGER.error("Failed to generate edit view.");
			throw new MojoExecutionException("Failed to generate edit view.", e);
		}
	}

	// test seam
	void setRepository() {
		ProvidedRepositoryFactory.set(new LocalDesignRepository(srcDir, false));
	}

	@SuppressWarnings("static-method") // test seam
	void generateEditView(String[] arguments) throws Exception {
		ViewGenerator.main(arguments);
	}
}
