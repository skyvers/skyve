package org.skyve.toolchain;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Execute;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.skyve.impl.generate.QueryGenerator;
import org.skyve.toolchain.config.GenerateDefaultQueriesConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Generates the default query metadata for a Skyve customer and module.
 *
 * <p>Threading: this mojo reads repository metadata and should be treated as thread-confined.
 */
@Mojo(name = "generateDefaultQueries", requiresDependencyResolution = ResolutionScope.TEST)
@Execute(phase = LifecyclePhase.PROCESS_RESOURCES)
public class GenerateDefaultQueriesMojo extends AbstractSkyveMojo {
	private static final Logger LOGGER = LoggerFactory.getLogger(GenerateDefaultQueriesMojo.class);

	@Parameter
	private GenerateDefaultQueriesConfig generateDefaultQueriesConfig;

	/**
	 * Resolves the customer and module names, then invokes the Skyve query generator.
	 *
	 * @throws MojoExecutionException if query generation fails
	 */
	@Override
	public void execute() throws MojoExecutionException {
		try {
			final String configCustomerName = (generateDefaultQueriesConfig != null) ? 
													generateDefaultQueriesConfig.getCustomer() :
													null;
			final String customerName = getDefaultOrPromptCustomer(configCustomerName);
			final String configModuleName = (generateDefaultQueriesConfig != null) ?
												generateDefaultQueriesConfig.getModule() :
												null;
			final String moduleName = getDefaultOrPrompt(configModuleName, "Please enter a module name");
			final boolean includeAssociationBizKeys = (generateDefaultQueriesConfig != null) && generateDefaultQueriesConfig.isIncludeAssociationBizKeys();

			configureClasspath();
			generateDefaultQueries(new String[] {customerName, moduleName, String.valueOf(includeAssociationBizKeys)});
		}
		catch (Exception e) {
			LOGGER.error("Failed to generated default queries.");
			throw new MojoExecutionException("Failed to generate default queries.", e);
		}
	}

	@SuppressWarnings("static-method") // test seam
	void generateDefaultQueries(String[] arguments) throws Exception {
		QueryGenerator.main(arguments);
	}
}
