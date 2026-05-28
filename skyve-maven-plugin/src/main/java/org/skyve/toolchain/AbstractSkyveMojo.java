package org.skyve.toolchain;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.codehaus.plexus.components.interactivity.PrompterException;
import org.skyve.util.Util;

public abstract class AbstractSkyveMojo extends AbstractMojo {
    @Parameter(defaultValue = "${project}", readonly = true)
    protected MavenProject project;

    @Component
    protected Prompter prompter;

    /**
     * Adds the project's test classpath to the current thread context class loader.
     *
     * <p>Side effects: replaces the context class loader for the current thread with a loader that can see the
     * project's test output, dependencies, and optionally the supplied source directory.
     */
    protected void configureClasspath() throws DependencyResolutionRequiredException, MalformedURLException {
    	configureClasspath(null);
    }
    
	/**
	 * Adds the project's test classpath and the supplied source directory to the current thread context class loader.
	 *
	 * <p>Side effects: replaces the context class loader for the current thread. Callers must ensure the new loader
	 * remains valid for the remainder of the mojo execution.
	 *
	 * @param srcDir optional source directory to add to the classpath; may be {@code null}
	 */
	protected void configureClasspath(String srcDir) throws DependencyResolutionRequiredException, MalformedURLException {
        final Collection<String> elements = project.getTestClasspathElements();
        final List<URL> listUrl = new ArrayList<>(elements.size() + 1);
        for (String artifact : elements) {
            final URL url = new File(artifact).toURI().toURL();
            listUrl.add(url);
        }
        if (srcDir != null) {
        	listUrl.add(new File(srcDir).toURI().toURL());
        }

        @SuppressWarnings("resource")
        final URLClassLoader newClassLoader = new URLClassLoader(listUrl.toArray(new URL[listUrl.size()]), Thread.currentThread().getContextClassLoader());
        Thread.currentThread().setContextClassLoader(newClassLoader);
    }

    /**
     * Locates a Skyve source subdirectory beneath one of the project's compile source roots.
     *
     * @param directoryName the directory name to search for, such as {@code modules} or {@code customers}
     * @return the first matching path
     * @throws FileNotFoundException if the directory cannot be found under any compile source root
     */
    protected Path getSkyveDirectory(String directoryName) throws FileNotFoundException {
        for (final String sourceRoot : project.getCompileSourceRoots()) {
            final Path sourceDirectory = Paths.get(sourceRoot, directoryName);
            if (sourceDirectory.toFile().exists()) {
                return sourceDirectory;
            }
        }

        throw new FileNotFoundException(String.format("Failed to find %s directory.", directoryName));
    }

    /**
     * Locates the project's {@code modules} source directory.
     *
     * @return the modules directory
     * @throws FileNotFoundException if the directory cannot be found
     */
    protected Path getModulesDirectory() throws FileNotFoundException {
        return getSkyveDirectory("modules");
    }

    /**
     * Locates the project's {@code customers} source directory.
     *
     * @return the customers directory
     * @throws FileNotFoundException if the directory cannot be found
     */
    protected Path getCustomersDirectory() throws FileNotFoundException {
        return getSkyveDirectory("customers");
    }

    /**
     * Returns the immediate subdirectories beneath the supplied customers directory.
     *
     * @param customersDirectory the directory that contains customer subdirectories
     * @return the customer directories in filesystem order
     */
    protected static List<File> getCustomerDirectories(Path customersDirectory) {
        return Arrays.stream(customersDirectory.toFile().listFiles())
                .filter(File::isDirectory)
                .toList();
    }

    protected String getDefaultOrPromptCustomer(String defaultCustomer) throws FileNotFoundException, PrompterException {
        String processedDefaultCustomer = Util.processStringValue(defaultCustomer);
    	if (processedDefaultCustomer != null) {
            return processedDefaultCustomer;
        }

        final Path customersDirectory = getCustomersDirectory();
        final List<File> customerDirectories = getCustomerDirectories(customersDirectory);

        final File customerDirectory;
        if (customerDirectories.size() == 1) {
            customerDirectory = customerDirectories.get(0);
        } else if (customerDirectories.size() > 1) {
            final String selectedCustomer = prompter.prompt(String.format("Please enter a customer name (customers available: %s)",
                    customerDirectories.stream().map(File::getName).collect(Collectors.joining(", "))));
            customerDirectory = customerDirectories.stream()
                    .filter(c -> c.getName().equals(selectedCustomer))
                    .findFirst()
                    .orElse(null);

            if (customerDirectory == null) {
                throw new IllegalArgumentException(String.format("Customer %s does not exist.", selectedCustomer));
            }
        } else {
            return prompter.prompt("Please enter a customer name");
        }

        return customerDirectory.getName();
    }

    /**
     * Returns the configured value or prompts the user when the configuration is blank.
     *
     * @param defaultValue the configured value; may be blank
     * @param promptMessage the prompt shown when the configured value is blank
     * @return the resolved value
     * @throws PrompterException if prompting fails
     */
    protected String getDefaultOrPrompt(String defaultValue, String promptMessage) throws PrompterException {
    	String processedDefaultValue = Util.processStringValue(defaultValue);
    	if (processedDefaultValue != null) {
            return processedDefaultValue;
        }

        return prompter.prompt(promptMessage);
    }
}
