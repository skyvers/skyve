package org.skyve.toolchain;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Execute;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.skyve.impl.generate.DialectOptions;
import org.skyve.impl.generate.DomainGenerator;
import org.skyve.toolchain.config.GenerateDomainConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Generates Skyve domain and generated-test sources from the repository metadata.
 *
 * <p>Threading: this mojo mutates generated source trees and should be treated as thread-confined.
 */
@Mojo(name = "generateDomain", requiresDependencyResolution = ResolutionScope.TEST)
@Execute(phase = LifecyclePhase.PROCESS_RESOURCES)
public class GenerateDomainMojo extends AbstractSkyveMojo {
	private static final Logger LOGGER = LoggerFactory.getLogger(GenerateDomainMojo.class);

	/**
	 * Relative source directory.
	 */
	@Parameter(required = true, defaultValue = "src/main/java/")
	private String srcDir;

	/**
	 * Relative generated directory.
	 */
	@Parameter(required = true, defaultValue = "src/generated/java/")
	private String generatedDir;

	/**
	 * Relative test directory.
	 */
	@Parameter(required = true, defaultValue = "src/test/java/")
	private String testDir;

	/**
	 * Relative generated test directory.
	 */
	@Parameter(required = true, defaultValue = "src/generatedTest/java/")
	private String generatedTestDir;

	@Parameter()
	private GenerateDomainConfig generateDomainConfig;

	/**
	 * Runs the domain generator with the configured dialect, source roots, and excluded modules.
	 *
	 * @throws MojoExecutionException if generation fails or the configuration is missing
	 */
	@Override
	public void execute() throws MojoExecutionException {
		if (generateDomainConfig == null) {
			throw new MojoExecutionException("Generate domain configuration not specified.");
		}

		try {
			configureClasspath(srcDir);
			DomainGenerator.registerCustomisations(generateDomainConfig.getCustomisationsClass());
			DomainGenerator.generate(generateDomainConfig.isDebug(),
										generateDomainConfig.isMultiTenant(),
										DialectOptions.valueOf(generateDomainConfig.getDialect()),
										srcDir,
										generatedDir,
										testDir,
										generatedTestDir,
										generateDomainConfig.getExcludedModules().split(","));
		}
		catch (Exception e) {
			LOGGER.error("Failed to generated domain.", e);
			throw new MojoExecutionException("Failed to generate domain.", e);
		}
	}
}
