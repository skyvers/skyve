package org.skyve.impl.create;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.script.SkyveScriptInterpreter;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

/**
 * Utility class for creating a new Skyve Maven project from template artefacts.
 *
 * <p>Copies and customises the project scaffold (directories, POM, configuration
 * files) for the specified project name and customer, ready for import into an IDE.
 *
 * <p>Threading: not thread-safe. Instances are mutable during setup and assembly.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate Maven project scaffolding path fragments.
public class MavenSkyveProject {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(MavenSkyveProject.class);

	private static final Path SKYVE_WAR_PATH = Paths.get("skyve-war");

	/**
	 * Relative source path in the Skyve template WAR.
	 */
	public static final Path SKYVE_SRC_PATH = SKYVE_WAR_PATH.resolve("src").resolve("main").resolve("java");
	/**
	 * Relative source-resources path in the Skyve template WAR.
	 */
	public static final Path SKYVE_SRC_RESOURCES_PATH = SKYVE_WAR_PATH.resolve("src").resolve("main").resolve("resources");
	/**
	 * Relative web Java path in the Skyve template WAR.
	 */
	public static final Path SKYVE_WEB_PATH = Paths.get("skyve-war").resolve("src").resolve("main").resolve("java");
	/**
	 * Relative generated-source path in the Skyve template WAR.
	 */
	public static final Path SKYVE_GENERATED_PATH = SKYVE_WAR_PATH.resolve("src").resolve("generated").resolve("java");
	/**
	 * Relative test-source path in the Skyve template WAR.
	 */
	public static final Path SKYVE_TEST_PATH = SKYVE_WAR_PATH.resolve("src").resolve("test").resolve("java");
	/**
	 * Relative generated-test path in the Skyve template WAR.
	 */
	public static final Path SKYVE_GENERATED_TEST_PATH = SKYVE_WAR_PATH.resolve("src").resolve("generatedTest").resolve("java");
	/**
	 * Relative webapp path in the Skyve template WAR.
	 */
	public static final Path SKYVE_WEBAPP_PATH = Paths.get("skyve-war").resolve("src").resolve("main").resolve("webapp");

	private final String projectName;
	private final String projectDescription;
	private final Path projectDirectory;
	private final String customerName;
	private final Path skyveDirectory;
	private final Path srcDirectory;
	private final Path generatedDirectory;
	private final Path resourceDirectory;
	private final Path webDirectory;
	private final Path testDirectory;
	private final Path generatedTestDirectory;
	private final String skyveScript;
	private final Map<String, Pair<ModuleMetaData, List<DocumentMetaData>>> metaData = new HashMap<>();
	private boolean copyFromProject;

	/**
	 * Creates a project definition from the supplied builder values.
	 *
	 * @param builder contains resolved paths, project identity, and assembly options.
	 */
	protected MavenSkyveProject(MavenSkyveProjectCreator builder) {
		this.projectName = builder.projectName;
		this.projectDescription = (builder.projectDescription != null) ? builder.projectDescription : builder.projectName;
		this.projectDirectory = Paths.get(builder.projectDirectory);
		this.customerName = builder.customerName;
		this.skyveDirectory = (builder.skyveDirectory != null) ? Paths.get(builder.skyveDirectory) : null;
		this.srcDirectory = Paths.get(builder.srcDirectory);
		this.generatedDirectory = Paths.get(builder.generatedDirectory);
		this.resourceDirectory = Paths.get(builder.resourceDirectory);
		this.webDirectory = Paths.get(builder.webDirectory);
		this.testDirectory = Paths.get(builder.testDirectory);
		this.generatedTestDirectory = Paths.get(builder.generatedTestDirectory);
		this.skyveScript = builder.skyveScript;
		this.copyFromProject = builder.copyFromProject;
	}

	/**
	 * Returns the project description.
	 *
	 * @return the project description.
	 */
	public String getProjectDescription() {
		return projectDescription;
	}

	/**
	 * Returns the project name.
	 *
	 * @return the project name.
	 */
	public String getProjectName() {
		return projectName;
	}

	/**
	 * Returns the absolute project directory.
	 *
	 * @return the absolute project directory.
	 */
	public Path getAbsoluteProjectPath() {
		return projectDirectory;
	}

	/**
	 * Returns the configured customer name.
	 *
	 * @return the customer name.
	 */
	public String getCustomerName() {
		return customerName;
	}

	/**
	 * Returns the absolute Skyve directory.
	 *
	 * @return the absolute Skyve directory.
	 */
	public Path getAbsoluteSkyvePath() {
		return skyveDirectory;
	}

	/**
	 * Returns the relative source directory.
	 *
	 * @return the relative source directory.
	 */
	public Path getRelativeSrcPath() {
		return srcDirectory;
	}

	/**
	 * Returns the absolute source directory.
	 *
	 * @return the absolute source directory.
	 */
	public Path getAbsoluteSrcPath() {
		return projectDirectory.resolve(srcDirectory);
	}

	/**
	 * Returns the absolute customer source directory.
	 *
	 * @return the absolute customer source directory.
	 */
	public Path getAbsoluteCustomerPath() {
		return getAbsoluteSrcPath().resolve("customers").resolve(customerName);
	}

	/**
	 * Returns the absolute customer resources directory.
	 *
	 * @return the absolute customer resources directory.
	 */
	public Path getAbsoluteCustomerResourcesPath() {
		return getAbsoluteCustomerPath().resolve("resources");
	}

	/**
	 * Returns the relative generated-source directory.
	 *
	 * @return the relative generated-source directory.
	 */
	public Path getRelativeGeneratedPath() {
		return generatedDirectory;
	}

	/**
	 * Returns the absolute generated-source directory.
	 *
	 * @return the absolute generated-source directory.
	 */
	public Path getAbsoluteGeneratedPath() {
		return projectDirectory.resolve(generatedDirectory);
	}

	/**
	 * Returns the relative resources directory.
	 *
	 * @return the relative resources directory.
	 */
	public Path getRelativeResourcePath() {
		return resourceDirectory;
	}

	/**
	 * Returns the absolute resources directory.
	 *
	 * @return the absolute resources directory.
	 */
	public Path getAbsoluteResourcePath() {
		return projectDirectory.resolve(resourceDirectory);
	}

	/**
	 * Returns the relative web directory.
	 *
	 * @return the relative web directory.
	 */
	public Path getRelativeWebPath() {
		return webDirectory;
	}

	/**
	 * Returns the absolute web directory.
	 *
	 * @return the absolute web directory.
	 */
	public Path getAbsoluteWebPath() {
		return projectDirectory.resolve(webDirectory);
	}

	/**
	 * Returns the relative test directory.
	 *
	 * @return the relative test directory.
	 */

	public Path getRelativeTestPath() {
		return testDirectory;
	}

	/**
	 * Returns the absolute test directory.
	 *
	 * @return the absolute test directory.
	 */
	public Path getAbsoluteTestPath() {
		return projectDirectory.resolve(testDirectory);
	}

	/**
	 * Returns the relative generated-test directory.
	 *
	 * @return the relative generated-test directory.
	 */
	public Path getRelativeGeneratedTestPath() {
		return generatedTestDirectory;
	}

	/**
	 * Returns the absolute generated-test directory.
	 *
	 * @return the absolute generated-test directory.
	 */
	public Path getAbsoluteGeneratedTestPath() {
		return projectDirectory.resolve(generatedTestDirectory);
	}

	/**
	 * Returns the absolute docker directory.
	 *
	 * @return the absolute docker directory.
	 */
	public Path getAbsoluteDockerPath() {
		return projectDirectory.resolve("docker");
	}

	/**
	 * Returns the absolute deployments directory.
	 *
	 * @return the absolute deployments directory.
	 */
	public Path getAbsoluteDeploymentsPath() {
		return projectDirectory.resolve("deployments");
	}

	/**
	 * Returns the absolute Skyve base path.
	 *
	 * @return the absolute Skyve base path.
	 */
	public Path getSkyveBasePath() {
		return skyveDirectory.toAbsolutePath();
	}

	/**
	 * Returns the absolute Skyve WAR webapp path.
	 *
	 * @return the absolute Skyve WAR webapp path.
	 */
	public Path getSkyveAbsoluteWarPath() {
		return skyveDirectory.resolve(SKYVE_WEBAPP_PATH);
	}

	/**
	 * Returns the configured Skyve script content.
	 *
	 * @return the configured Skyve script content.
	 */
	public String getSkyveScript() {
		return skyveScript;
	}

	/**
	 * Returns the target path for the persisted Skyve script file.
	 *
	 * @return the target script path.
	 */
	public Path getSkyveScriptPath() {
		return projectDirectory.resolve("scripts").resolve("skyve.md");
	}

	/**
	 * Returns module and document metadata resolved from script processing.
	 *
	 * @return module/document metadata map.
	 */
	protected Map<String, Pair<ModuleMetaData, List<DocumentMetaData>>> getMetaData() {
		return metaData;
	}
	
	/**
	 * Indicates whether files should be copied from a project layout.
	 *
	 * @return true if copying from project layout; otherwise false.
	 */
	public boolean isCopyFromProject() {
		return copyFromProject;
	}

	/**
	 * Copies template files into the destination project while preserving relative paths.
	 *
	 * @param projectDirectory the destination project directory.
	 * @param skyveBasePath the template base path used for relativizing file locations.
	 * @param files the files to copy.
	 * @throws IOException if directory creation or file copy fails.
	 */
	private static void copyFiles(File projectDirectory, Path skyveBasePath, List<File> files) throws IOException {
		for (File file : files) {
			final File target = new File(projectDirectory, skyveBasePath.relativize(file.toPath()).toString());
			if (! target.getParentFile().exists()) {
				if (! target.getParentFile().mkdirs()) {
					throw new IOException("Failed to create project sub-directory " + target.toPath());
				}
			}

			Files.copy(file.toPath(), target.toPath(), StandardCopyOption.REPLACE_EXISTING);
		}
	}

	/**
	 * Deletes previously assembled scaffold files from all managed target directories.
	 *
	 * <p>Side effects: removes files under source, generated, web, test, and historic
	 * locations in the target project structure.
	 */
	public void clearBeforeAssemble() {
		getSkyveAppFiles(getAbsoluteSrcPath()).forEach(File::delete);
		getSkyveGeneratedFiles(getAbsoluteGeneratedPath()).forEach(File::delete);
		getSkyveWebAppFiles(getAbsoluteWebPath()).forEach(File::delete);
		getSkyveTestFiles(getAbsoluteTestPath()).forEach(File::delete);
		getSkyveGeneratedTestFiles(getAbsoluteGeneratedTestPath()).forEach(File::delete);
		getHistoricFiles(getAbsoluteProjectPath()).forEach(File::delete);
	}

	/**
	 * Copies all configured Skyve scaffold groups into target project directories.
	 *
	 * @throws IOException if any required file cannot be copied.
	 */
	public void assemble() throws IOException {
		assemble(true, true, true, true);
	}

	/**
	 * Copies selected scaffold groups into the target project.
	 *
	 * @param includeAppFiles include application metadata and source assets.
	 * @param includeGeneratedFiles include generated source artefacts.
	 * @param includeWebAppFiles include web application assets.
	 * @param includeTestFiles include handwritten and generated test assets.
	 * @throws IOException if any selected copy operation fails.
	 */
	public void assemble(boolean includeAppFiles,
							boolean includeGeneratedFiles,
							boolean includeWebAppFiles,
							boolean includeTestFiles)
	throws IOException {
		if (includeAppFiles) {
			copySkyveAppFiles();
		}
		if (includeGeneratedFiles) {
			copySkyveGeneratedFiles();
		}
		if (includeWebAppFiles) {
			copySkyveWebAppFiles();
		}
		if (includeTestFiles) {
			copySkyveTestFiles();
			copySkyveGeneratedTestFiles();
		}
	}

	/**
	 * Applies and validates a Skyve script, persisting it to disk when valid.
	 *
	 * @param script the script content to apply.
	 * @throws SkyveProjectCreationException if preprocessing or validation reports errors.
	 */
	public void applyScript(String script) throws SkyveProjectCreationException {
		applyScript(script, true);
	}

	/**
	 * Applies and validates a Skyve script and optionally writes it to the project.
	 *
	 * <p>Side effects: populates in-memory module/document metadata and may write
	 * {@code scripts/skyve.md} when {@code writeToFile} is {@code true}.
	 *
	 * @param script the script content to parse and process.
	 * @param writeToFile whether to write the script into the project directory.
	 * @throws SkyveProjectCreationException if the script contains processing errors.
	 */
	public void applyScript(String script, boolean writeToFile) throws SkyveProjectCreationException {
		final SkyveScriptInterpreter i = new SkyveScriptInterpreter(skyveScript);
		i.preProcess();
		i.process();

		if (! i.getErrors().isEmpty()) {
			final StringBuilder sb = new StringBuilder(128);
			i.getErrors().forEach(
				error -> sb.append("Error ").append(error.getMessage()).append(" on line ").append(error.getLineNumber())
			);

			throw new SkyveProjectCreationException(sb.toString());
		}

		if (i.getModules().size() > 0) {
			for (ModuleMetaData m : i.getModules()) {
				// NOTE This line will need to be updated when Skyve script supports multiple modules.
				metaData.put(m.getName(), new ImmutablePair<>(m, i.getDocuments()));
			}

			if (writeToFile) {
				writeScriptToFile(script);
			}
		}
		else {
			LOGGER.warn("No modules were detected, please check your script.");
		}
	}

	/**
	 * Returns legacy scaffold files that should be removed from the target project.
	 *
	 * @param basePath the project base path to inspect.
	 * @return existing historic files targeted for cleanup.
	 */
	private List<File> getHistoricFiles(Path basePath) {
		final List<File> historicFiles = new ArrayList<>();
		final List<File> historicFolders = new ArrayList<>();

		historicFolders.add(basePath.resolve(webDirectory).resolve(AppConstants.ADMIN_MODULE_NAME).toFile());
		historicFolders.add(basePath.resolve(webDirectory).resolve("ecuador").toFile());
		historicFolders.add(basePath.resolve(webDirectory).resolve("editorial").toFile());
		historicFolders.add(basePath.resolve(webDirectory).resolve("ultima").toFile());
		historicFolders.add(basePath.resolve(webDirectory).resolve("mobile").toFile());
		historicFolders.add(basePath.resolve(webDirectory).resolve("web").toFile());
		for (File historicFolder : historicFolders) {
			if (historicFolder.exists()) {
				historicFiles.addAll(FileUtils.listFiles(historicFolder, null, true));
			}
		}

		historicFiles.add(basePath.resolve(webDirectory).resolve("admin-config.properties").toFile());
		historicFiles.add(basePath.resolve(srcDirectory).resolve("org/primefaces/util/FileUploadUtils.java").toFile());
		
		return historicFiles.stream().filter(File::exists).collect(Collectors.toList());
	}

	/**
	 * Returns webapp files to copy from the Skyve template.
	 *
	 * @param basePath the template webapp base path.
	 * @return webapp files to copy.
	 */
	@SuppressWarnings("static-method")
	private List<File> getSkyveWebAppFiles(Path basePath) {
		final List<File> skyveWebAppFiles = new ArrayList<>();

		skyveWebAppFiles.addAll(FileUtils.listFiles(basePath.toFile(), new String[] {"xhtml", "jsp"}, false));
		skyveWebAppFiles.addAll(FileUtils.listFiles(basePath.resolve("desktop").toFile(), null, true));
		skyveWebAppFiles.addAll(FileUtils.listFiles(basePath.resolve("external").toFile(), null, true));
		skyveWebAppFiles.addAll(FileUtils.listFiles(basePath.resolve("pages").toFile(), null, true));
		final File webInfPages = basePath.resolve("WEB-INF").resolve("pages").toFile();
		if (webInfPages.exists()) {
			skyveWebAppFiles.addAll(FileUtils.listFiles(webInfPages, null, true));
		}
		skyveWebAppFiles.add(basePath.resolve("WEB-INF").resolve("beans.xml").toFile());
		skyveWebAppFiles.add(basePath.resolve("WEB-INF").resolve("faces-config.xml").toFile());
		skyveWebAppFiles.add(basePath.resolve("WEB-INF").resolve("jboss-classloading.xml").toFile());
		skyveWebAppFiles.add(basePath.resolve("WEB-INF").resolve("jboss-deployment-structure.xml").toFile());
		skyveWebAppFiles.add(basePath.resolve("WEB-INF").resolve("undertow-handlers.conf").toFile());
		skyveWebAppFiles.add(basePath.resolve("WEB-INF").resolve("web.xml").toFile());
		skyveWebAppFiles.addAll(FileUtils.listFiles(basePath.resolve("WEB-INF").resolve("resources").resolve("skyve").toFile(), null, true));

		return skyveWebAppFiles;
	}

	/**
	 * Copies webapp scaffold files into the project web directory.
	 *
	 * @throws IOException if any copy operation fails.
	 */
	private void copySkyveWebAppFiles() throws IOException {
		final Path relativeWarPath = copyFromProject ? Paths.get("src").resolve("main").resolve("webapp") : SKYVE_WEBAPP_PATH;
		final Path absoluteWarPath = skyveDirectory.resolve(relativeWarPath);
		final File targetDir = projectDirectory.resolve(webDirectory).toFile();

		copyFiles(targetDir, absoluteWarPath, getSkyveWebAppFiles(absoluteWarPath));
	}

	/**
	 * Returns application files to copy from template source/resources paths.
	 *
	 * @param basePath the template source base path.
	 * @return application files to copy.
	 */
	private List<File> getSkyveAppFiles(Path basePath) {
		final List<File> skyveAppFiles = new ArrayList<>();

		// If we are copying from a project, we should copy the entire modules directory
		// as there will be an arbitrary number of modules.
		if (copyFromProject) {
			skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").toFile(), null, true));
		}
		else {
			skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").toFile(), new String[] {"java"}, false));
			skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").resolve(AppConstants.ADMIN_MODULE_NAME).toFile(), null, true));
		}
		skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("resources").toFile(), null, true));
		skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("schemas").toFile(), null, true));
		skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("router").toFile(), null, true));
		final File services = basePath.resolve("services").toFile();
		if (services.exists()) {
			skyveAppFiles.addAll(FileUtils.listFiles(services, null, true));
		}

		return skyveAppFiles;
	}

	/**
	 * Copies application scaffold files into source and resources directories.
	 *
	 * @throws IOException if any copy operation fails.
	 */
	private void copySkyveAppFiles() throws IOException {
		final Path relativeSrcPath = copyFromProject ? Paths.get("src").resolve("main").resolve("java") : SKYVE_SRC_PATH;
		final Path relativeResourcesPath = copyFromProject ? Paths.get("src").resolve("main").resolve("resources") : SKYVE_SRC_RESOURCES_PATH;
		final Path relativeWebPath = copyFromProject ? Paths.get("src").resolve("main").resolve("webapp") : SKYVE_WEB_PATH;

		final Path absoluteSrcPath = skyveDirectory.resolve(relativeSrcPath);
		final Path absoluteResourcesPath = skyveDirectory.resolve(relativeResourcesPath);
		final Path absoluteWebPath = skyveDirectory.resolve(relativeWebPath);
		final File targetSrcDir = projectDirectory.resolve(srcDirectory).toFile();
		final File targetResourcesDir = projectDirectory.resolve(resourceDirectory).toFile();

		copyFiles(targetSrcDir, absoluteSrcPath, getSkyveAppFiles(absoluteSrcPath));
		copyFiles(targetSrcDir,
					absoluteWebPath,
					new ArrayList<>(FileUtils.listFiles(
										(copyFromProject ? absoluteSrcPath : absoluteWebPath).resolve("org").resolve("skyve").toFile(),
										null,
										true)));
		copyFiles(targetResourcesDir,
					absoluteResourcesPath,
					new ArrayList<>(FileUtils.listFiles(absoluteResourcesPath.toFile(), null, true)));
	}

	/**
	 * Returns generated source files to copy from the template.
	 *
	 * @param basePath the template generated-source base path.
	 * @return generated source files to copy.
	 */
	private List<File> getSkyveGeneratedFiles(Path basePath) {
		final List<File> skyveGeneratedFiles = new ArrayList<>();

		// If we are copying from a project, we should copy the entire modules directory
		// as there will be an arbitrary number of modules.
		// skyve-war should only provide the admin module.
		if (copyFromProject) {
			skyveGeneratedFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").toFile(), null, true));
		}
		else {
			skyveGeneratedFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").resolve(AppConstants.ADMIN_MODULE_NAME).toFile(), null, true));
		}

		return skyveGeneratedFiles;
	}

	/**
	 * Copies generated source files into the generated-source directory.
	 *
	 * @throws IOException if any copy operation fails.
	 */
	private void copySkyveGeneratedFiles() throws IOException {
		final Path relativeGeneratedPath = copyFromProject ? Paths.get("src").resolve("generated").resolve("java") : SKYVE_GENERATED_PATH;
		final Path absoluteGeneratedPath = skyveDirectory.resolve(relativeGeneratedPath);
		final File targetDir = getAbsoluteGeneratedPath().toFile();

		copyFiles(targetDir, absoluteGeneratedPath, getSkyveGeneratedFiles(absoluteGeneratedPath));
	}

	/**
	 * Returns test files to copy from the template.
	 *
	 * @param basePath the template test-source base path.
	 * @return test files to copy.
	 */
	private static List<File> getSkyveTestFiles(Path basePath) {
		final List<File> skyveTestFiles = new ArrayList<>();

		skyveTestFiles.addAll(FileUtils.listFiles(basePath.resolve("util").toFile(), null, true));

		return skyveTestFiles;
	}

	/**
	 * Copies test scaffold files and rewrites customer placeholders in base tests.
	 *
	 * @throws IOException if any copy operation fails.
	 */
	private void copySkyveTestFiles() throws IOException {
		final Path relativeTestPath = copyFromProject ? Paths.get("src").resolve("test").resolve("java") : SKYVE_TEST_PATH;
		final Path absoluteTestPath = skyveDirectory.resolve(relativeTestPath);
		final File targetDir = projectDirectory.resolve(testDirectory).toFile();

		copyFiles(targetDir, absoluteTestPath, getSkyveTestFiles(absoluteTestPath));

		try {
			// update the internal base test to the appropriate customer name
			final Path abstractTestFilePath = projectDirectory.resolve(testDirectory).resolve("util").resolve("InternalBaseH2Test.java");
			String testContent = new String(Files.readAllBytes(abstractTestFilePath));
			testContent = testContent.replaceAll("bizhub", customerName);
			Files.write(abstractTestFilePath, testContent.getBytes());
		}
		catch (@SuppressWarnings("unused") IOException e) {
			LOGGER.warn("Failed to update customer, tests will likely fail.");
		}
	}

	/**
	 * Returns generated test files to copy from the template.
	 *
	 * @param basePath the template generated-test base path.
	 * @return generated test files to copy.
	 */
    private List<File> getSkyveGeneratedTestFiles(Path basePath) {
        final List<File> skyveGeneratedTestFiles = new ArrayList<>();

        // If we are copying from a project, we should copy the entire modules directory
        // as there will be an arbitrary number of modules.
        // skyve-war should only provide the admin module.

        File dir;

        if (copyFromProject) {
            dir = basePath.resolve("modules")
                          .toFile();
        } else {
            dir = basePath.resolve("modules")
                          .resolve(AppConstants.ADMIN_MODULE_NAME)
                          .toFile();
        }

        if (dir.isDirectory()) {
            skyveGeneratedTestFiles.addAll(FileUtils.listFiles(dir, null, true));
        }

        return skyveGeneratedTestFiles;
    }

	/**
	 * Copies generated test files into the generated-test directory.
	 *
	 * @throws IOException if any copy operation fails.
	 */
	private void copySkyveGeneratedTestFiles() throws IOException {
		final Path relativeGeneratedTestPath = copyFromProject ? Paths.get("src").resolve("generatedTest").resolve("java") : SKYVE_GENERATED_TEST_PATH;
		final Path absoluteGeneratedTestPath = skyveDirectory.resolve(relativeGeneratedTestPath);
		final File targetDir = projectDirectory.resolve(generatedTestDirectory).toFile();

		copyFiles(targetDir, absoluteGeneratedTestPath, getSkyveGeneratedTestFiles(absoluteGeneratedTestPath));
	}

	/**
	 * Writes script content to the default script file in the project directory.
	 *
	 * @param script the script content to write.
	 */
	private void writeScriptToFile(String script) {
		// write the script to file.
		final File scriptFile = getSkyveScriptPath().toFile();
		if (scriptFile.getParentFile().mkdirs()) {
			try {
				Files.write(scriptFile.toPath(), script.getBytes());
			}
			catch (IOException e) {
				LOGGER.warn("Failed to write Skyve script to file.", e);
			}
		}
		else {
			LOGGER.warn("Failed to create directories for the Skyve script.");
		}
	}

	/**
	 * Builder used to configure and initialise a MavenSkyveProject instance.
	 */
	public static class MavenSkyveProjectCreator {
		/**
		 * Default relative source directory used by new project scaffolds.
		 */
		static final String DEFAULT_SRC_DIRECTORY = "src/main/java/";
		/**
		 * Default relative generated-source directory used by new project scaffolds.
		 */
		static final String DEFAULT_GENERATED_DIRECTORY = "src/generated/java/";
		/**
		 * Default relative resources directory used by new project scaffolds.
		 */
		static final String DEFAULT_RESOURCES_DIRECTORY = "src/main/resources/";
		/**
		 * Default relative web directory used by new project scaffolds.
		 */
		static final String DEFAULT_WEB_DIRECTORY = "src/main/webapp/";
		/**
		 * Default relative test directory used by new project scaffolds.
		 */
		static final String DEFAULT_TEST_DIRECTORY = "src/test/java/";
		/**
		 * Default relative generated-test directory used by new project scaffolds.
		 */
		static final String DEFAULT_GENERATED_TEST_DIRECTORY = "src/generatedTest/java/";

		private String projectName;
		private String projectDescription;
		private String projectDirectory;
		private String customerName;
		private String skyveDirectory;
		private String srcDirectory = DEFAULT_SRC_DIRECTORY;
		private String generatedDirectory = DEFAULT_GENERATED_DIRECTORY;
		private String resourceDirectory = DEFAULT_RESOURCES_DIRECTORY;
		private String webDirectory = DEFAULT_WEB_DIRECTORY;
		private String testDirectory = DEFAULT_TEST_DIRECTORY;
		private String generatedTestDirectory = DEFAULT_GENERATED_TEST_DIRECTORY;
		private String skyveScript;
		private boolean copyFromProject = false;

		/**
		 * Creates an empty builder with default folder values.
		 */
		public MavenSkyveProjectCreator() {
			// nothing to do here
		}

		/**
		 * Creates a builder with an initial project name.
		 *
		 * @param projectName the project name.
		 */
		public MavenSkyveProjectCreator(String projectName) {
			this.projectName = projectName;
		}

		/**
		 * Sets the project name.
		 *
		 * @param projectName the project name.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator projectName(final String projectName) {
			this.projectName = projectName;
			return this;
		}

		/**
		 * Sets the project description.
		 *
		 * @param projectDescription the project description.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator projectDescription(final String projectDescription) {
			this.projectDescription = projectDescription;
			return this;
		}

		/**
		 * Sets the project directory.
		 *
		 * @param projectDirectory the target project directory.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator projectDirectory(final String projectDirectory) {
			this.projectDirectory = projectDirectory;
			return this;
		}

		/**
		 * Sets the customer name.
		 *
		 * @param customerName the customer name.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator customerName(final String customerName) {
			this.customerName = customerName;
			return this;
		}

		/**
		 * Sets the Skyve source directory.
		 *
		 * @param skyveDirectory the Skyve source directory.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator skyveDirectory(final String skyveDirectory) {
			this.skyveDirectory = skyveDirectory;
			return this;
		}

		/**
		 * Sets the relative source directory.
		 *
		 * @param srcDirectory the relative source directory.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator srcDirectory(final String srcDirectory) {
			this.srcDirectory = srcDirectory;
			return this;
		}

		/**
		 * Sets the relative generated-source directory.
		 *
		 * @param generatedDirectory the relative generated-source directory.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator generatedDirectory(final String generatedDirectory) {
			this.generatedDirectory = generatedDirectory;
			return this;
		}

		/**
		 * Sets the relative resources directory.
		 *
		 * @param resourceDirectory the relative resources directory.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator resourceDirectory(final String resourceDirectory) {
			this.resourceDirectory = resourceDirectory;
			return this;
		}

		/**
		 * Sets the relative web directory.
		 *
		 * @param webDirectory the relative web directory.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator webDirectory(final String webDirectory) {
			this.webDirectory = webDirectory;
			return this;
		}

		/**
		 * Sets the relative test directory.
		 *
		 * @param testDirectory the relative test directory.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator testDirectory(final String testDirectory) {
			this.testDirectory = testDirectory;
			return this;
		}

		/**
		 * Sets the relative generated-test directory.
		 *
		 * @param generatedTestDirectory the relative generated-test directory.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator generatedTestDirectory(final String generatedTestDirectory) {
			this.generatedTestDirectory = generatedTestDirectory;
			return this;
		}

		/**
		 * Sets the Skyve script content.
		 *
		 * @param skyveScript the Skyve script content.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator skyveScript(final String skyveScript) {
			this.skyveScript = skyveScript;
			return this;
		}

		/**
		 * Sets whether scaffold files should be copied from an existing project.
		 *
		 * @param copyFromProject true to copy from a project; otherwise false.
		 * @return this builder.
		 */
		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator copyFromProject(final boolean copyFromProject) {
			this.copyFromProject = copyFromProject;
			return this;
		}

		/**
		 * Builds a configured MavenSkyveProject from this builder state.
		 *
		 * @return a configured MavenSkyveProject.
		 */
		public MavenSkyveProject initialise() {
			return new MavenSkyveProject(this);
		}
	}
}
