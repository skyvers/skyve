package org.skyve.toolchain;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.skyve.create.SkyveProject;
import org.skyve.create.SkyveProject.SkyveProjectCreator;

@Mojo(name = "clearBeforeAssemble")
public class ClearBeforeAssembleMojo extends AbstractMojo {
	@Parameter(defaultValue = "${project}", readonly = true)
	protected MavenProject project;

	/**
	 * Customer name.
	 */
	@Parameter(required = true)
	private String customer;

	@Override
	public void execute() throws MojoExecutionException {
		try {
			final SkyveProject me = new SkyveProjectCreator()
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
