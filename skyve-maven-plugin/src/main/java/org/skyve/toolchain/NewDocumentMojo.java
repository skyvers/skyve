package org.skyve.toolchain;

import java.io.File;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.components.interactivity.PrompterException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.document.BizKey;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleDocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.util.PluralUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.Persistent;
import org.skyve.toolchain.config.NewDocumentConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nullable;

@Mojo(name = "newDocument")
public class NewDocumentMojo extends AbstractSkyveMojo {
	private static final Logger LOGGER = LoggerFactory.getLogger(NewDocumentMojo.class);

	@Parameter
	private NewDocumentConfig newDocumentConfig;

	protected String moduleName;
	protected String documentName;
	protected File newDocumentDirectory;

	@Override
	public void execute() throws MojoExecutionException {
		try {
			final Path modulesDirectory = getModulesDirectory(project);

			moduleName = getModuleName();
			final Path moduleDirectory = modulesDirectory.resolve(moduleName);
			if (! moduleDirectory.toFile().exists()) {
				throw new MojoExecutionException(String.format("Directory %s for module %s does not exist.",
																moduleDirectory.toFile().getAbsolutePath(),
																moduleName));
			}

			final Path absoluteSrcPath = modulesDirectory.getParent();

			// The XMLMetaData class requires this path to be set to the src directory.
			UtilImpl.APPS_JAR_DIRECTORY = absoluteSrcPath.toFile().getAbsolutePath() + File.separator;

			final String newDocumentName = prompter.prompt("Please enter a document name");
			documentName = BindUtil.toJavaTypeIdentifier(newDocumentName);
			newDocumentDirectory = moduleDirectory.resolve(documentName).toFile();
			if (newDocumentDirectory.exists()) {
				throw new MojoExecutionException(String.format("Directory %s for new document %s already exists.",
																newDocumentDirectory.getAbsolutePath(),
																documentName));
			}

			if (! newDocumentDirectory.mkdir()) {
				throw new MojoExecutionException(String.format("Failed to create directory %s for new document %s.",
																newDocumentDirectory.getAbsolutePath(),
																documentName));
			}
			LOGGER.info("Successfully created directory {} for new document {}.", newDocumentDirectory.getAbsolutePath(), documentName);

			final DocumentMetaData documentMetaData = new DocumentMetaData();
			documentMetaData.setName(documentName);
			documentMetaData.setSingularAlias(documentName);
			documentMetaData.setPluralAlias(PluralUtil.pluralise(documentName));

			String persistentName = generatePersistentName(moduleName, documentName);
			if (persistentName != null) {
				Persistent persistent = new Persistent();
				persistent.setName(persistentName);
				documentMetaData.setPersistent(persistent);

				BizKey bizKey = new BizKey();
				bizKey.setExpression(documentName);
				documentMetaData.setBizKey(bizKey);
			}

			XMLMetaData.marshalDocument(documentMetaData, false, moduleDirectory.toFile().getAbsolutePath());
			LOGGER.info("Successfully created metadata for new document {}.", documentName);

			final Path moduleMetaDataFile = moduleDirectory.resolve(String.format("%s.xml", moduleName));
			if (! moduleMetaDataFile.toFile().exists()) {
				LOGGER.warn("Module metadata {} does not exist, document will need to be added to module metadata manually.",
								moduleMetaDataFile.toFile().getAbsolutePath());
				return;
			}

			final ModuleMetaData moduleMetaData = XMLMetaData.unmarshalModuleFile(moduleMetaDataFile.toFile().getAbsolutePath());
			final ModuleDocumentMetaData moduleDocument = new ModuleDocumentMetaData();
			moduleDocument.setRef(documentName);
			moduleMetaData.getDocuments().add(moduleDocument);
			moduleMetaData.getDocuments().sort(Comparator.comparing(ModuleDocumentMetaData::getRef));
			XMLMetaData.marshalModule(moduleMetaData, false, modulesDirectory.toFile().getAbsolutePath());
			LOGGER.info("Successfully added document {} to module metadata {}.", documentName, moduleName);

		}
		catch (PrompterException | FileNotFoundException e) {
			throw new MojoExecutionException("Failed to create new document.", e);
		}
	}

	private static Path getModulesDirectory(MavenProject project) throws FileNotFoundException {
		for (final String sourceRoot : project.getCompileSourceRoots()) {
			final Path modules = Paths.get(sourceRoot, "modules");
			if (modules.toFile().exists()) {
				return modules;
			}
		}

		throw new FileNotFoundException("Failed to find modules directory.");
	}

	private String getModuleName() throws PrompterException {
		if ((newDocumentConfig != null) && StringUtils.isNotBlank(newDocumentConfig.getDefaultModule())) {
			return newDocumentConfig.getDefaultModule();
		}
		return prompter.prompt("Please enter a module name");
	}

	/**
	 * Uses the module and document name to guess the persistent name
	 */
	private static @Nullable String generatePersistentName(String moduleName, final String documentName) {
		if ((moduleName != null) && (documentName != null)) {
			// remove any non-word characters and whitespace from the module name
			String prefix = StringUtils.deleteWhitespace(moduleName.replaceAll("\\W", ""));
			if (prefix.length() > 3) {
				prefix = StringUtils.left(moduleName, 3);

				// cleanse the document name of any invalid characters
				return prefix.toUpperCase() + "_" + documentName;
			}
		}
		return null;
	}
}
