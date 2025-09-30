package org.skyve.toolchain;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.skyve.impl.create.MavenSkyveProject;
import org.skyve.impl.create.MavenSkyveProject.MavenSkyveProjectCreator;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;

@Mojo(name = "script", requiresDependencyResolution = ResolutionScope.TEST)
public class ScriptMojo extends AbstractSkyveMojo {

	/**
	 * Skyve directory (absolute or relative).
	 */
	@Parameter(required = true, property = "skyveDir")
	private String skyveDir;

	/**
	 * Customer name.
	 */
	@Parameter(required = true, property = "customer")
	private String customer;

	/**
	 * Path to the Skyve script to apply.
	 */
	@Parameter(required = true, defaultValue = "script/skyve.md", property = "scriptPath")
	private String scriptPath;

	@Override
	public void execute() throws MojoExecutionException {
		try {
			configureClasspath();
			final File scriptFile = new File(scriptPath);
			// if relative convert to absolute.
			if (! scriptFile.isAbsolute()) {
				scriptPath = project.getBasedir().toPath().resolve(scriptPath).toString();
			}

			final MavenSkyveProject me = new MavenSkyveProjectCreator().projectName(project.getName())
																		.projectDirectory(project.getBasedir().getAbsolutePath()).customerName(customer)
																		.skyveDirectory(skyveDir)
																		.initialise();

			final String script = new String(Files.readAllBytes(Paths.get(scriptPath)));
			me.applyScript(script, false);
		}
		catch (Exception e) {
			throw new MojoExecutionException("Failed to apply Skyve script.", e);
		}
	}
}
