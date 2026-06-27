package org.skyve.toolchain;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.skyve.impl.create.MavenSkyveProject;
import org.skyve.impl.create.MavenSkyveProject.MavenSkyveProjectCreator;

/**
 * Removes any previously generated assemble output for the selected customer.
 *
 * <p>Threading: this mojo mutates filesystem state and should be treated as thread-confined.
 */
@Mojo(name = "clearBeforeAssemble")
public class ClearBeforeAssembleMojo extends AbstractMojo {
	@Parameter(defaultValue = "${project}", readonly = true)
	protected MavenProject project;

	/**
	 * Customer name.
	 */
	@Parameter(required = true, property = "customer")
	private String customer;

	/**
	 * Loads the project context and clears assembled artefacts for the configured customer.
	 *
	 * @throws MojoExecutionException if the clean-up fails
	 */
	@Override
	public void execute() throws MojoExecutionException {
		try {
			final MavenSkyveProject me = new MavenSkyveProjectCreator()
											.projectName(project.getName())
											.projectDirectory(project.getBasedir().getAbsolutePath())
											.customerName(customer)
											.initialise();
			me.clearBeforeAssemble();
		}
		catch (Exception e) {
			throw new MojoExecutionException("Failed to clear before assemble.", e);
		}
	}
}
