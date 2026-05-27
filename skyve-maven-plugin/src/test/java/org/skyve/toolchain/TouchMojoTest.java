package org.skyve.toolchain;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.test.util.ReflectionTestUtils;

class TouchMojoTest {

	@TempDir
	Path tempDir;

	private TouchMojo mojo;
	private MavenProject project;

	@BeforeEach
	void setUp() {
		mojo = new TouchMojo();
		project = new MavenProject();
		project.setArtifactId("skyve-war");
		// MavenProject.getBasedir() returns the project base directory file
		// We use ReflectionTestUtils to set the basedir
		ReflectionTestUtils.setField(project, "basedir", tempDir.toFile());
		ReflectionTestUtils.setField(mojo, "project", project);
	}

	@Test
	void executeCreatesDefaultDeploymentFile() throws Exception {
		Files.createDirectories(tempDir.resolve("deployments"));
		mojo.execute();
		assertTrue(tempDir.resolve("deployments/skyve-war.war.dodeploy").toFile().exists());
	}

	@Test
	void executeCreatesAbsolutePathFile() throws Exception {
		Path target = tempDir.resolve("customfile.txt");
		ReflectionTestUtils.setField(mojo, "touchFile", target.toString());
		mojo.execute();
		assertTrue(target.toFile().exists());
	}

	@Test
	void executeCreatesRelativePathFile() throws Exception {
		ReflectionTestUtils.setField(mojo, "touchFile", "deployments/relative.txt");
		Files.createDirectories(tempDir.resolve("deployments"));
		mojo.execute();
		assertTrue(tempDir.resolve("deployments/relative.txt").toFile().exists());
	}
}
