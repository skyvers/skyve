package org.skyve.toolchain;

import org.apache.commons.io.FileUtils;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.StringUtils;

import java.io.File;

/**
 * Updates the deploy marker file for the current Skyve build output.
 *
 * <p>Threading: this mojo mutates filesystem state and should be treated as thread-confined.
 */
@Mojo(name = "touch")
public class TouchMojo extends AbstractSkyveMojo {
	/**
	 * Skyve directory (absolute or relative).
	 */
	@Parameter()
	private String touchFile;

	/**
	 * Touches the configured marker file, or the default deployment marker when none is supplied.
	 *
	 * @throws MojoExecutionException if the marker file cannot be updated
	 */
	@Override
	public void execute() throws MojoExecutionException {
		try {
			File fileToTouch = null;
			// Set default if not set by the user.
			if (StringUtils.isBlank(touchFile)) {
				fileToTouch = project.getBasedir().toPath().resolve("deployments").resolve(String.format("%s.war.dodeploy", project.getArtifactId())).toFile();
			}
			else {
				fileToTouch = new File(touchFile);
				// if relative convert to absolute.
				if (! fileToTouch.isAbsolute()) {
					fileToTouch = project.getBasedir().toPath().resolve(touchFile).toFile();
				}
			}

			FileUtils.touch(fileToTouch);
		}
		catch (Exception e) {
			throw new MojoExecutionException(String.format("Failed to touch: %s.", touchFile), e);
		}
	}
}
