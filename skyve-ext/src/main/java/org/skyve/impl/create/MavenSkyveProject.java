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
import org.skyve.impl.util.UtilImpl;

public class MavenSkyveProject {
	private static final Path SKYVE_WAR_PATH = Paths.get("skyve-war");
	public static final Path SKYVE_SRC_PATH = SKYVE_WAR_PATH.resolve("src").resolve("main").resolve("java");
	public static final Path SKYVE_SRC_RESOURCES_PATH = SKYVE_WAR_PATH.resolve("src").resolve("main").resolve("resources");
	public static final Path SKYVE_WEB_PATH = Paths.get("skyve-war").resolve("src").resolve("main").resolve("java");
	public static final Path SKYVE_GENERATED_PATH = SKYVE_WAR_PATH.resolve("src").resolve("generated").resolve("java");
	public static final Path SKYVE_TEST_PATH = SKYVE_WAR_PATH.resolve("src").resolve("test").resolve("java");
	public static final Path SKYVE_GENERATED_TEST_PATH = SKYVE_WAR_PATH.resolve("src").resolve("generatedTest").resolve("java");
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

	public String getProjectDescription() {
		return projectDescription;
	}

	public String getProjectName() {
		return projectName;
	}

	public Path getAbsoluteProjectPath() {
		return projectDirectory;
	}

	public String getCustomerName() {
		return customerName;
	}

	public Path getAbsoluteSkyvePath() {
		return skyveDirectory;
	}

	public Path getRelativeSrcPath() {
		return srcDirectory;
	}

	public Path getAbsoluteSrcPath() {
		return projectDirectory.resolve(srcDirectory);
	}

	public Path getAbsoluteCustomerPath() {
		return getAbsoluteSrcPath().resolve("customers").resolve(customerName);
	}

	public Path getAbsoluteCustomerResourcesPath() {
		return getAbsoluteCustomerPath().resolve("resources");
	}

	public Path getRelativeGeneratedPath() {
		return generatedDirectory;
	}

	public Path getAbsoluteGeneratedPath() {
		return projectDirectory.resolve(generatedDirectory);
	}

	public Path getRelativeResourcePath() {
		return resourceDirectory;
	}

	public Path getAbsoluteResourcePath() {
		return projectDirectory.resolve(resourceDirectory);
	}

	public Path getRelativeWebPath() {
		return webDirectory;
	}

	public Path getAbsoluteWebPath() {
		return projectDirectory.resolve(webDirectory);
	}

	public Path getRelativeTestPath() {
		return testDirectory;
	}

	public Path getAbsoluteTestPath() {
		return projectDirectory.resolve(testDirectory);
	}

	public Path getRelativeGeneratedTestPath() {
		return generatedTestDirectory;
	}

	public Path getAbsoluteGeneratedTestPath() {
		return projectDirectory.resolve(generatedTestDirectory);
	}

	public Path getAbsoluteDockerPath() {
		return projectDirectory.resolve("docker");
	}

	public Path getAbsoluteDeploymentsPath() {
		return projectDirectory.resolve("deployments");
	}

	public Path getSkyveBasePath() {
		return skyveDirectory.toAbsolutePath();
	}

	public Path getSkyveAbsoluteWarPath() {
		return skyveDirectory.resolve(SKYVE_WEBAPP_PATH);
	}

	public String getSkyveScript() {
		return skyveScript;
	}

	public Path getSkyveScriptPath() {
		return projectDirectory.resolve("scripts").resolve("skyve.md");
	}

	protected Map<String, Pair<ModuleMetaData, List<DocumentMetaData>>> getMetaData() {
		return metaData;
	}
	
	public boolean isCopyFromProject() {
		return copyFromProject;
	}

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
	 * Deletes any files that were copied via assemble.
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
	 * Copies all skyve-ee files into the relevant project directories.
	 */
	public void assemble() throws IOException {
		assemble(true, true, true, true);
	}

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

	public void applyScript(String script) throws SkyveProjectCreationException {
		applyScript(script, true);
	}

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
			UtilImpl.LOGGER.warning("No modules were detected, please check your script.");
		}
	}

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
		skyveWebAppFiles.add(basePath.resolve("WEB-INF").resolve("faces-config.xml").toFile());
		skyveWebAppFiles.add(basePath.resolve("WEB-INF").resolve("jboss-classloading.xml").toFile());
		skyveWebAppFiles.add(basePath.resolve("WEB-INF").resolve("undertow-handlers.conf").toFile());
		skyveWebAppFiles.add(basePath.resolve("WEB-INF").resolve("web.xml").toFile());
		skyveWebAppFiles.addAll(FileUtils.listFiles(basePath.resolve("WEB-INF").resolve("resources").resolve("skyve").toFile(), null, true));

		return skyveWebAppFiles;
	}

	private void copySkyveWebAppFiles() throws IOException {
		final Path relativeWarPath = copyFromProject ? Paths.get("src").resolve("main").resolve("webapp") : SKYVE_WEBAPP_PATH;
		final Path absoluteWarPath = skyveDirectory.resolve(relativeWarPath);
		final File targetDir = projectDirectory.resolve(webDirectory).toFile();

		copyFiles(targetDir, absoluteWarPath, getSkyveWebAppFiles(absoluteWarPath));
	}

	private List<File> getSkyveAppFiles(Path basePath) {
		final List<File> skyveAppFiles = new ArrayList<>();

		// If we are copying from a project, we should copy the entire modules directory
		// as there will be an arbitrary number of modules.
		// skyve-ee only provides the admin module.
		if (copyFromProject) {
			skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").toFile(), null, true));
		}
		else {
			skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").toFile(), new String[] {"java"}, false));
			skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").resolve(AppConstants.ADMIN_MODULE_NAME).toFile(), null, true));
		}
		skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("resources").toFile(), null, true));
		skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("schemas").toFile(), null, true));
		skyveAppFiles.addAll(FileUtils.listFiles(basePath.resolve("router").toFile(), new String[] {"java"}, true));
		final File services = basePath.resolve("services").toFile();
		if (services.exists()) {
			skyveAppFiles.addAll(FileUtils.listFiles(services, null, true));
		}

		return skyveAppFiles;
	}

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

	private List<File> getSkyveGeneratedFiles(Path basePath) {
		final List<File> skyveGeneratedFiles = new ArrayList<>();

		// If we are copying from a project, we should copy the entire modules directory
		// as there will be an arbitrary number of modules.
		// skyve-ee only provides the admin module.
		if (copyFromProject) {
			skyveGeneratedFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").toFile(), null, true));
		}
		else {
			skyveGeneratedFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").resolve(AppConstants.ADMIN_MODULE_NAME).toFile(), null, true));
		}

		return skyveGeneratedFiles;
	}

	private void copySkyveGeneratedFiles() throws IOException {
		final Path relativeGeneratedPath = copyFromProject ? Paths.get("src").resolve("generated").resolve("java") : SKYVE_GENERATED_PATH;
		final Path absoluteGeneratedPath = skyveDirectory.resolve(relativeGeneratedPath);
		final File targetDir = getAbsoluteGeneratedPath().toFile();

		copyFiles(targetDir, absoluteGeneratedPath, getSkyveGeneratedFiles(absoluteGeneratedPath));
	}

	private static List<File> getSkyveTestFiles(Path basePath) {
		final List<File> skyveTestFiles = new ArrayList<>();

		skyveTestFiles.addAll(FileUtils.listFiles(basePath.resolve("util").toFile(), null, true));

		return skyveTestFiles;
	}

	private void copySkyveTestFiles() throws IOException {
		final Path relativeTestPath = copyFromProject ? Paths.get("src").resolve("test").resolve("java") : SKYVE_TEST_PATH;
		final Path absoluteTestPath = skyveDirectory.resolve(relativeTestPath);
		final File targetDir = projectDirectory.resolve(testDirectory).toFile();

		copyFiles(targetDir, absoluteTestPath, getSkyveTestFiles(absoluteTestPath));

		try {
			// update the junit4 base test
			final Path abstractTestFilePath = projectDirectory.resolve(testDirectory).resolve("util").resolve("AbstractH2Test.java");
			String testContent = new String(Files.readAllBytes(abstractTestFilePath));
			testContent = testContent.replaceAll("bizhub", customerName);
			Files.write(abstractTestFilePath, testContent.getBytes());

			// update the junit5 base test
			final Path abstractTestFilePath2 = projectDirectory.resolve(testDirectory).resolve("util").resolve("AbstractH2TestForJUnit5.java");
			String testContent2 = new String(Files.readAllBytes(abstractTestFilePath2));
			testContent2 = testContent2.replaceAll("bizhub", customerName);
			Files.write(abstractTestFilePath2, testContent2.getBytes());
		}
		catch (@SuppressWarnings("unused") IOException e) {
			UtilImpl.LOGGER.warning("Failed to update customer, tests will likely fail.");
		}
	}

	private List<File> getSkyveGeneratedTestFiles(Path basePath) {
		final List<File> skyveGeneratedTestFiles = new ArrayList<>();

		// If we are copying from a project, we should copy the entire modules directory
		// as there will be an arbitrary number of modules.
		// skyve-ee only provides the admin module.
		if (copyFromProject) {
			skyveGeneratedTestFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").toFile(), null, true));
		}
		else {
			skyveGeneratedTestFiles.addAll(FileUtils.listFiles(basePath.resolve("modules").resolve(AppConstants.ADMIN_MODULE_NAME).toFile(), null, true));
		}

		return skyveGeneratedTestFiles;
	}

	private void copySkyveGeneratedTestFiles() throws IOException {
		final Path relativeGeneratedTestPath = copyFromProject ? Paths.get("src").resolve("generated").resolve("java") : SKYVE_GENERATED_TEST_PATH;
		final Path absoluteGeneratedTestPath = skyveDirectory.resolve(relativeGeneratedTestPath);
		final File targetDir = projectDirectory.resolve(generatedTestDirectory).toFile();

		copyFiles(targetDir, absoluteGeneratedTestPath, getSkyveGeneratedTestFiles(absoluteGeneratedTestPath));
	}

	private void writeScriptToFile(String script) {
		// write the script to file.
		final File scriptFile = getSkyveScriptPath().toFile();
		if (scriptFile.getParentFile().mkdirs()) {
			try {
				Files.write(scriptFile.toPath(), script.getBytes());
			}
			catch (IOException e) {
				UtilImpl.LOGGER.warning("Failed to write Skyve script to file.");
				e.printStackTrace();
			}
		}
		else {
			UtilImpl.LOGGER.warning("Failed to create directories for the Skyve script.");
		}
	}

	public static class MavenSkyveProjectCreator {
		static final String DEFAULT_SRC_DIRECTORY = "src/main/java/";
		static final String DEFAULT_GENERATED_DIRECTORY = "src/generated/java/";
		static final String DEFAULT_RESOURCES_DIRECTORY = "src/main/resources/";
		static final String DEFAULT_WEB_DIRECTORY = "src/main/webapp/";
		static final String DEFAULT_TEST_DIRECTORY = "src/test/java/";
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

		public MavenSkyveProjectCreator() {
			// nothing to do here
		}

		public MavenSkyveProjectCreator(String projectName) {
			this.projectName = projectName;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator projectName(final String projectName) {
			this.projectName = projectName;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator projectDescription(final String projectDescription) {
			this.projectDescription = projectDescription;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator projectDirectory(final String projectDirectory) {
			this.projectDirectory = projectDirectory;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator customerName(final String customerName) {
			this.customerName = customerName;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator skyveDirectory(final String skyveDirectory) {
			this.skyveDirectory = skyveDirectory;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator srcDirectory(final String srcDirectory) {
			this.srcDirectory = srcDirectory;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator generatedDirectory(final String generatedDirectory) {
			this.generatedDirectory = generatedDirectory;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator resourceDirectory(final String resourceDirectory) {
			this.resourceDirectory = resourceDirectory;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator webDirectory(final String webDirectory) {
			this.webDirectory = webDirectory;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator testDirectory(final String testDirectory) {
			this.testDirectory = testDirectory;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator generatedTestDirectory(final String generatedTestDirectory) {
			this.generatedTestDirectory = generatedTestDirectory;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator skyveScript(final String skyveScript) {
			this.skyveScript = skyveScript;
			return this;
		}

		@SuppressWarnings("hiding")
		public MavenSkyveProjectCreator copyFromProject(final boolean copyFromProject) {
			this.copyFromProject = copyFromProject;
			return this;
		}

		public MavenSkyveProject initialise() {
			return new MavenSkyveProject(this);
		}
	}
}
