package org.skyve.toolchain;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.codehaus.plexus.components.interactivity.PrompterException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerModuleMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

@Mojo(name = "newModule")
public class NewModuleMojo extends AbstractSkyveMojo {
	private static final Logger LOGGER = LoggerFactory.getLogger(NewModuleMojo.class);

	@Override
	public void execute() throws MojoExecutionException {
		try {
			final Path modulesDirectory = getModulesDirectory();

			final String newModuleName = prompter.prompt("Please enter a module name");
			final String properModuleName = BindUtil.toJavaInstanceIdentifier(newModuleName);
			final File newModuleFile = modulesDirectory.resolve(properModuleName).toFile();
			if (newModuleFile.exists()) {
				throw new MojoExecutionException(String.format("Directory %s for new module %s already exists.",
																newModuleFile.getAbsolutePath(),
																newModuleName));
			}

			final Path absoluteSrcPath = modulesDirectory.getParent();

			// The XMLMetaData class requires this path to be set to the src directory.
			UtilImpl.APPS_JAR_DIRECTORY = absoluteSrcPath.toFile().getAbsolutePath() + File.separator;

			if (! newModuleFile.mkdir()) {
				throw new MojoExecutionException(String.format("Failed to create directory %s for new module %s.",
																newModuleFile.getAbsolutePath(),
																newModuleName));
			}
			LOGGER.info("Successfully created directory {} for new module {}.", newModuleFile.getAbsolutePath(), newModuleName);

			final ModuleMetaData newModuleMetaData = new ModuleMetaData();
			newModuleMetaData.setName(properModuleName);
			newModuleMetaData.setTitle(newModuleName);
			XMLMetaData.marshalModule(newModuleMetaData, false, modulesDirectory.toFile().getAbsolutePath());
			LOGGER.info("Successfully created metadata for new module {}.", newModuleName);

			try {
				final Path customersDirectory = getCustomersDirectory();
				final List<File> customerDirectories = getCustomerDirectories(customersDirectory);

				final File customerDirectory;
				if (customerDirectories.size() == 1) {
					customerDirectory = customerDirectories.get(0);
				}
				else if (customerDirectories.size() > 1) {
					final String selectedCustomer = prompter.prompt(String.format("Enter the name of the customer this module belongs to (customers available: %s)",
																					customerDirectories.stream().map(File::getName).collect(Collectors.joining(", "))));
					customerDirectory = customerDirectories.stream()
															.filter(c -> c.getName().equals(selectedCustomer))
															.findFirst()
															.orElse(null);

					if (customerDirectory == null) {
						LOGGER.warn("Customer {} does not exist, module will need to be added to customer metadata manually.", selectedCustomer);
						return;
					}
				}
				else {
					LOGGER.warn("Failed to find customer metadata, module will need to be added to customer metadata manually.");
					return;
				}

				final String customerName = customerDirectory.getName();
				final File customerFile = Paths.get(customerDirectory.getAbsolutePath(),
														String.format("%s.xml", customerName)).toFile();

				if (! customerFile.exists()) {
					LOGGER.warn("Customer metadata {} does not exist, module will need to be added to customer metadata manually.",
									customerFile.getAbsolutePath());
					return;
				}

				final CustomerModuleMetaData newCustomerModuleMetaData = new CustomerModuleMetaData();
				newCustomerModuleMetaData.setName(properModuleName);

				final CustomerMetaData customerMetaData = XMLMetaData.unmarshalCustomerFile(customerFile.getAbsolutePath());
				customerMetaData.getModules().getModules().add(newCustomerModuleMetaData);

				XMLMetaData.marshalCustomer(customerMetaData, absoluteSrcPath.toFile().getAbsolutePath());
				LOGGER.info("Successfully added module {} to customer metadata {}.",
								newModuleName,
								customerFile.getAbsolutePath());
			}
			catch (@SuppressWarnings("unused") FileNotFoundException e) {
				LOGGER.warn("Failed to find customers directory, module will need to be added to customer metadata manually.");
			}
		}
		catch (PrompterException | FileNotFoundException e) {
			throw new MojoExecutionException("Failed to create new module.", e);
		}
	}
}
