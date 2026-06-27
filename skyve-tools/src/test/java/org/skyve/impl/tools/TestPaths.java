package org.skyve.impl.tools;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Resolves repository-relative paths for tests that need Skyve application metadata.
 */
public final class TestPaths {
	private TestPaths() {
		// Utility class.
	}

	public static Path skyveWarMainJava() {
		return repositoryRoot().resolve("skyve-war").resolve("src").resolve("main").resolve("java");
	}

	private static Path repositoryRoot() {
		Path root = Path.of(System.getProperty("maven.multiModuleProjectDirectory", System.getProperty("user.dir")));
		if (Files.exists(root.resolve("skyve-war"))) {
			return root;
		}
		Path parent = root.getParent();
		if ((parent != null) && Files.exists(parent.resolve("skyve-war"))) {
			return parent;
		}
		return root;
	}
}
