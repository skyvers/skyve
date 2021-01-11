package org.skyve.toolchain;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.*;
import org.skyve.impl.generate.QueryGenerator;
import org.skyve.toolchain.config.GenerateDefaultQueriesConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Generates default queries.
 */
@Mojo(name = "generateDefaultQueries", requiresDependencyResolution = ResolutionScope.TEST)
@Execute(phase = LifecyclePhase.PROCESS_RESOURCES)
public class GenerateDefaultQueriesMojo extends AbstractSkyveMojo {
    private static final Logger LOGGER = LoggerFactory.getLogger(GenerateDefaultQueriesMojo.class);

    @Parameter()
    private GenerateDefaultQueriesConfig generateDefaultQueriesConfig;

    public void execute() throws MojoExecutionException {
        try {
            final String configCustomerName = generateDefaultQueriesConfig != null ? generateDefaultQueriesConfig.getCustomer() : null;
            final String customerName = getDefaultOrPromptCustomer(configCustomerName);

            final String configModuleName = generateDefaultQueriesConfig != null ? generateDefaultQueriesConfig.getModule() : null;
            final String moduleName = getDefaultOrPrompt(configModuleName, "Please enter a module name");

            final Boolean includeAssociationBizKeys = generateDefaultQueriesConfig != null ? generateDefaultQueriesConfig.getIncludeAssociationBizKeys() : false;

            configureClasspath();
            QueryGenerator.main(new String[] {
                    customerName,
                    moduleName,
                    String.valueOf(Boolean.TRUE.equals(includeAssociationBizKeys))
            });
        } catch (Exception e) {
            LOGGER.error("Failed to generated default queries.", e);
            throw new MojoExecutionException("Failed to generate default queries.", e);
        }
    }
}
