package org.skyve.impl.generate;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Stream;

import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.metadata.repository.ProvidedRepository;

/**
 * Runs {@link OverridableDomainGenerator} over a mocked repository of the sales module into a
 * directory, and reads back what it wrote, for the tests of its customer output.
 */
final class CustomerGenerationTestSupport {
	/** The module the tests generate, which is excluded from domain test generation unless a test asks for tests. */
	static final String MODULE = "sales";

	private CustomerGenerationTestSupport() {
		// static helpers only
	}

	/**
	 * Runs a writing generator over the repository.
	 *
	 * @param srcPath	The source path, where hand-written extensions are, ending with '/', or "".
	 * @param validate	Whether to validate the metadata.
	 */
	static void generate(String srcPath, Path generatedSrc, Path generatedTest, ProvidedRepository repo, boolean validate) throws Exception {
		generate(srcPath, generatedSrc, generatedTest, repo, validate, new String[] {MODULE});
	}

	/**
	 * Runs a writing generator over the repository, generating domain and action tests for the modules not excluded.
	 *
	 * @param srcPath	The source path, where hand-written extensions are, ending with '/', or "".
	 * @param validate	Whether to validate the metadata.
	 * @param excludedModules	The modules to generate no tests for, or null for none.
	 */
	static void generate(String srcPath, Path generatedSrc, Path generatedTest, ProvidedRepository repo, boolean validate, String[] excludedModules)
	throws Exception {
		OverridableDomainGenerator gen = new OverridableDomainGenerator(true,
				false,
				false,
				DialectOptions.H2_NO_INDEXES,
				srcPath,
				generatedSrc.toString() + '/',
				"",
				generatedTest.toString() + '/',
				excludedModules);
		gen.repository = repo;
		gen.validate = validate;
		gen.generate();
	}

	/**
	 * Returns every file below the directory, by relative path, with its contents.
	 */
	static Map<String, String> snapshot(Path directory) throws Exception {
		Map<String, String> result = new TreeMap<>();
		try (Stream<Path> paths = Files.walk(directory)) {
			for (Path path : paths.filter(Files::isRegularFile).toList()) {
				result.put(directory.relativize(path).toString().replace(java.io.File.separatorChar, '/'), Files.readString(path));
			}
		}
		return result;
	}

	/**
	 * Creates a text attribute displayed by its name.
	 */
	static Text text(String name) {
		Text result = new Text();
		result.setName(name);
		result.setDisplayName(name);
		return result;
	}
}
