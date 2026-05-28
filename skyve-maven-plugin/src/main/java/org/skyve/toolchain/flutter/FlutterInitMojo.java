package org.skyve.toolchain.flutter;

import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.TreeMap;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.components.interactivity.PrompterException;
import org.jboss.weld.bootstrap.events.BeforeShutdownImpl;
import org.jboss.weld.environment.se.Weld;
import org.skyve.impl.cdi.SkyveCDIProducer;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.generate.client.flutter.FlutterGenerator;
import org.skyve.impl.generate.client.flutter.FlutterGenerator.GeneratorConfig;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.DataStore;
import org.skyve.toolchain.AbstractSkyveMojo;

/**
 * Generates a baseline Flutter project from an existing Skyve project.
 *
 * <p>Threading: this mojo bootstraps CDI, persistence, repository, and utility singletons and should be treated
 * as thread-confined.
 */
@Mojo(name = "flutter-init")
public class FlutterInitMojo extends AbstractSkyveMojo {

    @Parameter(property = "targetDir")
    private String targetDir;

    @Parameter(property = "uxui")
    private String uxui;

    @Parameter(property = "projectName")
    private String projectName;

    @Parameter(property = "overwrite", defaultValue = "false")
    private boolean overwrite;

    @Parameter(property = "customer")
    private String customer;

    @Parameter(required = true, defaultValue = "src/main/java/")
    private String srcDir;

    @Parameter(property = "modocWhitelist", required = true, defaultValue = "*.*")
    private List<String> modocWhitelist;

    /**
     * Resolves interactive parameters, prepares the output directory, and runs the Flutter generator.
     *
     * @throws MojoExecutionException if generation fails
     * @throws MojoFailureException if required parameters are missing or the target directory already exists
     */
    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {

        info("Running flutter-init");

        try {
            customer = getDefaultOrPromptCustomer(customer);
            targetDir = getDefaultOrPrompt(targetDir, "Please enter the target output directory");
            uxui = getDefaultOrPrompt(uxui, "Please enter the UXUI");
            projectName = getDefaultOrPrompt(projectName, "Please enter the project name");
        } catch (PrompterException e) {
            throw new MojoFailureException("Error assembling required parameters", e);
        } catch (FileNotFoundException fnfe) {
            getLog().error(fnfe);
            throw new MojoExecutionException("Error loading customer details", fnfe);
        }

        debug("Target directory: " + targetDir);
        Path root = prepareTargetDirectory(targetDir, overwrite);
        String projectPath = root.toAbsolutePath()
                                 .toString();

        debug("Running generator:");
        debugParam("uxui", uxui);
        debugParam("projectName", projectName);
        debugParam("projectPath", projectPath);
        debugParam("customer", customer);
        debugParam("modocWhitelist", modocWhitelist.stream()
                                                   .collect(joining(", ")));

        try {
            Weld weld = bootstrapSkyve();
            try {
	            GeneratorConfig config = new FlutterGenerator.GeneratorConfig();
	            config.setUxui(uxui);
	            config.setProjectName(projectName);
	            config.setProjectPath(projectPath);
	            config.setCustomerName(customer);
	
	            modocWhitelist.forEach(config::addModocWhitelistEntry);
	
	            FlutterGenerator generator = new FlutterGenerator(config);
	            generator.generate();
            }
	        finally {
                weld.shutdown();
            }
        } catch (Exception e) {
            getLog().error(e);
            throw new MojoExecutionException("Error while generating project", e);
        }
    }

    /**
     * Bootstraps the Skyve runtime needed by the Flutter generator.
     *
     * <p>Side effects: initialises CDI, configures persistence, data stores, repository access, and the current
     * thread's Skyve user context.
     *
     * @return the initialised Weld container
     * @throws DependencyResolutionRequiredException if Maven cannot resolve the test classpath
     * @throws MalformedURLException if a classpath element cannot be converted to a URL
     */
    @SuppressWarnings("resource") // NB for weld use
	private Weld bootstrapSkyve() throws DependencyResolutionRequiredException, MalformedURLException {
        configureClasspath(srcDir);

        String output = project.getBuild()
                               .getOutputDirectory();
        UtilImpl.APPS_JAR_DIRECTORY = output + File.separator;

        final String DB_DRIVER = "org.h2.Driver";
        final String DB_URL = "jdbc:h2:mem:test";
        final String DB_UNAME = "user";
        final String DB_PWD = "password";
        final String DB_DIALECT = "org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect";

        Weld weld = new Weld();
        weld.addPackage(true, SkyveCDIProducer.class);
        weld.initialize();

        Class<BeforeShutdownImpl> bsi = BeforeShutdownImpl.class;
        debug(bsi + "");

        AbstractPersistence.IMPLEMENTATION_CLASS = HibernateContentPersistence.class;
        AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS = RDBMSDynamicPersistence.class;
        AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
        UtilImpl.DATA_STORE = new DataStore(DB_DRIVER, DB_URL, DB_UNAME, DB_PWD, DB_DIALECT);
        UtilImpl.DATA_STORES.put("test", UtilImpl.DATA_STORE);
        UtilImpl.DDL_SYNC = false;
        UtilImpl.SQL_TRACE = false;
        UtilImpl.QUERY_TRACE = false;
        UtilImpl.JOB_SCHEDULER = false;
        UtilImpl.CONFIGURATION = new TreeMap<>();

        ProvidedRepositoryFactory.set(new LocalDesignRepository());

        String MOJO_USERNAME = "flutter-gen-user";
        final SuperUser user = new SuperUser();
        user.setCustomerName(customer);
        user.setName(MOJO_USERNAME);
        user.setId(MOJO_USERNAME);

        final AbstractPersistence persistence = AbstractPersistence.get();
        persistence.setUser(user);

        return weld;
    }

    /**
     * Creates or clears the target directory for the generated Flutter project.
     *
     * @param dir the target directory path
     * @param clear whether to delete existing contents instead of failing when the directory exists
     * @return the resolved target path
     * @throws MojoFailureException if the directory already exists and {@code clear} is {@code false}
     * @throws MojoExecutionException if the directory cannot be created or cleared
     */
	private Path prepareTargetDirectory(String dir, boolean clear) throws MojoFailureException, MojoExecutionException {
        Path root = Path.of(dir);
        if (root.toFile()
                .exists()) {
            if (clear) {
                try {
                    // Delete the contents of the output directory,
                    // this should play a bit nicer while having the project
                    // open in another editor.
                    debug("Deleting contents of: " + root);
                    try (Stream<Path> dirContents = Files.list(root)) {
	                    for (Path path : dirContents.collect(toList())) {
	                        debug("Deleting: " + path);
	                        File file = path.toFile();
	                        if (file.isDirectory()) {
	                            FileUtils.deleteDirectory(path.toFile());
	                        } else {
	                            file.delete();
	                        }
	                    }
                    }
                } catch (IOException e) {
                    getLog().error(e);
                    throw new MojoExecutionException("Unable to clear target directory: " + root, e);
                }
            } else {
                throw new MojoFailureException("Target directory '" + root + "' already exists and overwrite=false");
            }
        }

        try {
            debug("Creating " + root);
            Files.createDirectories(root);
        } catch (IOException e) {
            getLog().error(e);
            throw new MojoExecutionException("Unabled to create target directory: " + root, e);
        }

        return root;
    }

    /**
     * Logs a debug message using the mojo prefix.
     *
     * @param msg the message to log
     */
	private void debug(CharSequence msg) {

        getLog().debug("[flutter-init] " + msg);
    }

    /**
     * Logs an informational message using the mojo prefix.
     *
     * @param msg the message to log
     */
	private void info(CharSequence msg) {

        getLog().info("[flutter-init] " + msg);
    }

    /**
     * Logs a named debug parameter value using the mojo prefix.
     *
     * @param name the parameter name
     * @param value the parameter value
     */
	private void debugParam(String name, Object value) {

        debug(name + "=" + value);
    }

}
