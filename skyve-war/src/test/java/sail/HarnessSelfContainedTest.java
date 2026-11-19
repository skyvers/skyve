package sail;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

/**
 * Guards that the SAIL harness can be assembled into an application. {@code skyve:assemble} copies only
 * {@code src/test/java/util/**} and {@code src/test/resources/util/**} (see {@code MavenSkyveProject}: its
 * {@code getSkyveTestFiles} and {@code getSkyveTestResourceFiles}); the rest of Skyve's tests, its other modules and its
 * test fixtures stay behind. A file under {@code util} that uses any of them compiles in Skyve and fails in an application.
 *
 * <p>The references that this test refuses are derived from the folders, not listed by hand:
 * <ul>
 * <li>every class under {@code src/test/java} outside {@code util}, by its qualified name;</li>
 * <li>every folder of {@code src/test/java} and {@code src/test/resources} outside {@code util} that no module's
 * {@code src/main} or {@code src/generated} has (such as {@code sail/kitchenSink} and {@code sail/admin}), as a
 * package or a path; a top level folder is replaced by its sub-folders, as {@code sail} is also the first word of the
 * settings' names;</li>
 * <li>every module of Skyve's other than {@code admin}, as a package or a path ({@code modules.test.}, {@code modules/test/}).</li>
 * </ul>
 * Comments are not code: a mention in a comment or Javadoc passes. A reference in a string does not, because a path or a
 * class name in a string is a reference at run time.
 */
class HarnessSelfContainedTest {
	private static final Path SRC = Paths.get("src");
	private static final Path TEST_JAVA = SRC.resolve("test/java");
	private static final Path TEST_RESOURCES = SRC.resolve("test/resources");
	/** The sources that an application has beside its own tests: this module's, and those of the Skyve modules it depends on. */
	private static final List<Path> SHARED = sharedSources();
	private static final String HARNESS = "util";
	/** The one module that assembly provides to every application. */
	private static final String ASSEMBLED_MODULE = "admin";
	/** Files with {@code //} and {@code /* *}{@code /} comments. */
	private static final List<String> C_LIKE = List.of(".java", ".js");

	@Test
	@SuppressWarnings("static-method")
	void theAssembledHarnessUsesNothingThatAssemblyDoesNotCopy() throws IOException {
		Map<String, Pattern> refused = refusedReferences();
		List<Path> harness = new ArrayList<>();
		harness.addAll(files(TEST_JAVA.resolve(HARNESS)));
		harness.addAll(files(TEST_RESOURCES.resolve(HARNESS)));
		assertFalse(harness.isEmpty(), "No harness files found under " + TEST_JAVA.resolve(HARNESS).toAbsolutePath());
		assertFalse(refused.isEmpty(), "No references derived from " + TEST_JAVA.toAbsolutePath());

		List<Executable> checks = new ArrayList<>();
		for (Path file : harness) {
			String text = code(file);
			String[] lines = text.split("\n", -1);
			for (int i = 0; i < lines.length; i++) {
				String line = lines[i];
				int number = i + 1;
				for (Map.Entry<String, Pattern> reference : refused.entrySet()) {
					Matcher matcher = reference.getValue().matcher(line);
					if (matcher.find()) {
						checks.add(() -> org.junit.jupiter.api.Assertions.fail(String.format(
								"%s:%d refers to %s (%s), which skyve:assemble does not copy: %s",
								SRC.relativize(file), Integer.valueOf(number), matcher.group(), reference.getKey(), line.trim())));
					}
				}
			}
		}
		assertAll("The harness (src/test/java/util and src/test/resources/util) uses what assembly leaves behind", checks);
	}

	/**
	 * Derives what the harness must not refer to, from what is in the source folders but not in {@code util}.
	 *
	 * @return a pattern for each, by what it is
	 */
	private static Map<String, Pattern> refusedReferences() throws IOException {
		Map<String, Pattern> result = new LinkedHashMap<>();

		// every class outside util, by its qualified name
		for (Path java : files(TEST_JAVA)) {
			Path relative = TEST_JAVA.relativize(java);
			if (java.toString().endsWith(".java") && (! relative.startsWith(HARNESS))) {
				String name = relative.toString().replace('/', '.');
				name = name.substring(0, name.length() - ".java".length());
				result.put("test class " + name, Pattern.compile("(?<![\\w.])" + Pattern.quote(name) + "(?![\\w])"));
			}
		}

		// every folder outside util that the application's own source does not share, as a package and as a path
		TreeSet<String> folders = new TreeSet<>();
		for (Path root : List.of(TEST_JAVA, TEST_RESOURCES)) {
			folders.addAll(unsharedFolders(root));
		}
		for (String folder : folders) {
			result.put("test folder " + folder, folderPattern(folder));
		}

		// Skyve's own modules, but the one that every application has
		TreeSet<String> modules = new TreeSet<>();
		for (Path root : List.of(TEST_JAVA, SRC.resolve("main/java"), SRC.resolve("generated/java"), SRC.resolve("generatedTest/java"))) {
			Path modulesFolder = root.resolve("modules");
			if (Files.isDirectory(modulesFolder)) {
				try (Stream<Path> children = Files.list(modulesFolder)) {
					children.filter(Files::isDirectory).map(p -> p.getFileName().toString())
							.filter(name -> ! ASSEMBLED_MODULE.equals(name)).forEach(modules::add);
				}
			}
		}
		for (String module : modules) {
			result.put("Skyve module " + module, folderPattern("modules/" + module));
		}
		return result;
	}

	/**
	 * The shallowest folders of the root, outside {@code util}, with no folder of the same path in the application's source.
	 * A top level folder with a folder of its own in the application's source is searched; one that is not shared is
	 * replaced by its sub-folders (its classes are refused by name).
	 */
	private static List<String> unsharedFolders(Path root) throws IOException {
		List<String> result = new ArrayList<>();
		if (! Files.isDirectory(root)) {
			return result;
		}
		try (Stream<Path> top = Files.list(root)) {
			for (Path folder : (Iterable<Path>) top.filter(Files::isDirectory).sorted()::iterator) {
				String name = folder.getFileName().toString();
				if (HARNESS.equals(name)) {
					continue;
				}
				if (shared(name)) {
					collect(folder, root, result);
				}
				else {
					try (Stream<Path> children = Files.list(folder)) {
						children.filter(Files::isDirectory).sorted()
								.forEach(child -> result.add(root.relativize(child).toString()));
					}
				}
			}
		}
		return result;
	}

	/** Adds the shallowest sub-folders of a shared folder that are not shared. */
	private static void collect(Path folder, Path root, List<String> result) throws IOException {
		try (Stream<Path> children = Files.list(folder)) {
			for (Path child : (Iterable<Path>) children.filter(Files::isDirectory).sorted()::iterator) {
				String relative = root.relativize(child).toString();
				if (shared(relative)) {
					collect(child, root, result);
				}
				else {
					result.add(relative);
				}
			}
		}
	}

	private static List<Path> sharedSources() {
		List<Path> result = new ArrayList<>();
		Path repository = SRC.toAbsolutePath().getParent().getParent();
		try (Stream<Path> modules = Files.list(repository)) {
			modules.filter(Files::isDirectory).sorted().forEach(module -> {
				for (String source : List.of("src/main/java", "src/main/resources", "src/generated/java")) {
					result.add(module.resolve(source));
				}
			});
		}
		catch (IOException e) {
			throw new UncheckedIOException(e);
		}
		return result;
	}

	private static boolean shared(String relativeFolder) {
		return SHARED.stream().anyMatch(root -> Files.isDirectory(root.resolve(relativeFolder)));
	}

	/**
	 * A folder as a path ({@code a/b}), or as a package ({@code a.b}) of a class: followed by any sub-packages and a class
	 * name, which starts with a capital. A dotted name that no class follows is not refused, because it can be a setting
	 * such as {@code sail.load.users}, whose first words are a folder's.
	 */
	private static Pattern folderPattern(String folder) {
		String path = Pattern.quote(folder);
		String pkg = Pattern.quote(folder.replace('/', '.'));
		return Pattern.compile("(?<![\\w./])" + path + "(?![\\w])|(?<![\\w.])" + pkg + "(?:\\.[a-z]\\w*)*\\.(?=[A-Z])");
	}

	private static List<Path> files(Path root) throws IOException {
		if (! Files.isDirectory(root)) {
			return List.of();
		}
		try (Stream<Path> walk = Files.walk(root)) {
			return walk.filter(Files::isRegularFile).sorted().toList();
		}
	}

	/**
	 * Reads a file as code: the comments of a Java or JavaScript file are blanked (keeping the lines), so that only code and
	 * strings remain. Other files are read as they are.
	 */
	private static String code(Path file) throws IOException {
		String text = new String(Files.readAllBytes(file), StandardCharsets.ISO_8859_1);
		String name = file.getFileName().toString();
		return C_LIKE.stream().anyMatch(name::endsWith) ? withoutComments(text) : text;
	}

	/** Blanks {@code //} and block comments, leaving string, character and text block literals alone. */
	static String withoutComments(String text) {
		StringBuilder result = new StringBuilder(text.length());
		int length = text.length();
		int i = 0;
		while (i < length) {
			char c = text.charAt(i);
			char next = (i + 1 < length) ? text.charAt(i + 1) : '\0';
			if ((c == '/') && (next == '/')) {
				while ((i < length) && (text.charAt(i) != '\n')) {
					i++;
				}
			}
			else if ((c == '/') && (next == '*')) {
				i += 2;
				while ((i < length) && (! ((text.charAt(i) == '*') && (i + 1 < length) && (text.charAt(i + 1) == '/')))) {
					if (text.charAt(i) == '\n') {
						result.append('\n');
					}
					i++;
				}
				i += 2;
			}
			else if ((c == '"') && text.startsWith("\"\"\"", i)) {
				int end = text.indexOf("\"\"\"", i + 3);
				end = (end < 0) ? length : end + 3;
				result.append(text, i, end);
				i = end;
			}
			else if ((c == '"') || (c == '\'')) {
				result.append(c);
				i++;
				while ((i < length) && (text.charAt(i) != c) && (text.charAt(i) != '\n')) {
					if (text.charAt(i) == '\\') {
						result.append(text.charAt(i));
						i++;
					}
					if (i < length) {
						result.append(text.charAt(i));
						i++;
					}
				}
				if (i < length) {
					result.append(text.charAt(i));
					i++;
				}
			}
			else {
				result.append(c);
				i++;
			}
		}
		return result.toString();
	}
}
