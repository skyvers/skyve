package org.skyve.toolchain;

import java.io.File;
import java.io.IOException;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.skyve.impl.create.MavenSkyveProject;
import org.skyve.impl.create.MavenSkyveProject.MavenSkyveProjectCreator;

@Mojo(name = "assemble")
public class AssembleMojo extends AbstractMojo {

	@Parameter(defaultValue = "${project}", readonly = true)
	protected MavenProject project;

	/**
	 * Skyve directory (absolute or relative).
	 */
	@Parameter(required = true)
	private String skyveDir;

	/**
	 * Template project directory (absolute or relative).
	 */
	@Parameter
	private String templateDir;

	/**
	 * Customer name.
	 */
	@Parameter(required = true)
	private String customer;

	@Override
	public void execute() throws MojoExecutionException {
		try {
			final File skyveFile = new File(skyveDir);
			// if relative convert to absolute.
			if (! skyveFile.isAbsolute()) {
				skyveDir = project.getBasedir().toPath().resolve(skyveDir).toString();
			}

			if (! new File(skyveDir).exists()) {
				throw new IOException(String.format("Skyve directory %s does not exist.", skyveDir));
			}

			if (templateDir != null) {
				final File templateFile = new File(templateDir);
				// if relative convert to absolute.
				if (! templateFile.isAbsolute()) {
					templateDir = project.getBasedir().toPath().resolve(templateDir).toString();
				}
			}

			final MavenSkyveProjectCreator creator = new MavenSkyveProjectCreator()
														.projectName(project.getName())
														.projectDirectory(project.getBasedir().getAbsolutePath())
														.customerName(customer)
														.skyveDirectory(skyveDir);
			if (templateDir != null) {
				// Assemble from a project instead of Skyve.
				if (new File(templateDir).exists()) {
					creator.skyveDirectory(templateDir);
					creator.copyFromProject(true);
				}
			}

			final MavenSkyveProject me = creator.initialise();
			me.clearBeforeAssemble();
			me.assemble();
		}
		catch (Exception e) {
			throw new MojoExecutionException("Failed to assemble.", e);
		}
	}
}
