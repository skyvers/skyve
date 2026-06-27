package org.skyve.toolchain;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import javax.lang.model.element.Modifier;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.squareup.javapoet.AnnotationSpec;
import com.squareup.javapoet.ClassName;
import com.squareup.javapoet.CodeBlock;
import com.squareup.javapoet.FieldSpec;
import com.squareup.javapoet.JavaFile;
import com.squareup.javapoet.MethodSpec;
import com.squareup.javapoet.ParameterizedTypeName;
import com.squareup.javapoet.TypeSpec;

import jakarta.enterprise.inject.Default;
import jakarta.inject.Inject;

/**
 * Creates a new document service class with standard retrieval helpers.
 *
 * <p>Threading: this mojo mutates project sources and should be treated as thread-confined.
 */
@Mojo(name = "newService")
public class NewServiceMojo extends NewDocumentMojo {
	private static final Logger LOGGER = LoggerFactory.getLogger(NewServiceMojo.class);

	/**
	 * Relative source directory.
	 */
	@Parameter(required = true, defaultValue = "src/main/java/")
	protected String srcDir;

	/**
	 * Creates the document first when required, then writes the service class.
	 *
	 * @throws MojoExecutionException if the service cannot be created
	 */
	@Override
	public void execute() throws MojoExecutionException {
		try {
			super.execute();
		}
		catch (DocumentDirectoryAlreadyExistsException e) {
			LOGGER.info("Document '{}' in module '{}' already exists. Skipping document creation and creating service only.",
						documentName,
						moduleName);
		}

		final Path serviceClassPath = getServiceClassPath();
		if (Files.exists(serviceClassPath)) {
			throw new MojoExecutionException(String.format("Service class %s for document %s in module %s already exists.",
															serviceClassPath.toAbsolutePath(),
															documentName,
															moduleName));
		}

		createServiceClass();
	}

	/**
	 * Creates the document service class.
	 *
	 * <p>The generated service injects {@link Persistence} and exposes simple document lookup helpers.
	 */
	void createServiceClass() {
		final String serviceName = documentName + "Service";

		final ClassName extensionClassName = ClassName.get("modules." + moduleName + "." + documentName, getExtensionName());
		final MethodSpec get = MethodSpec.methodBuilder("get")
				.addJavadoc(CodeBlock.builder()
						.add("Return the $L with the specified bizId.\n\n", documentName)
						.add("@param bizId The bizId of the $L to retrieve\n", documentName)
						.add("@return The $L, or null if one does not exist with the specified bizId", documentName)
						.build())
											.addModifiers(Modifier.PUBLIC)
											.returns(extensionClassName)
											.addParameter(String.class, "bizId")
											.addStatement("final $T query = persistence.newDocumentQuery($T.MODULE_NAME, $T.DOCUMENT_NAME)",
															DocumentQuery.class,
															extensionClassName,
															extensionClassName)
											.addStatement("query.getFilter().addEquals($T.DOCUMENT_ID, bizId)", extensionClassName)
											.addStatement("return query.beanResult()")
											.build();

		final MethodSpec getAll = MethodSpec.methodBuilder("getAll")
				.addJavadoc(CodeBlock.builder()
						.add("Retrieves all $Ls in the datastore.\n\n", documentName)
						.add("@return All $Ls", documentName)
						.build())
				.addModifiers(Modifier.PUBLIC)
												.returns(ParameterizedTypeName.get(ClassName.get(List.class), extensionClassName))
												.addStatement("final $T query = persistence.newDocumentQuery($T.MODULE_NAME, $T.DOCUMENT_NAME)",
																DocumentQuery.class,
																extensionClassName,
																extensionClassName)
												.addStatement("return query.beanResults()")
												.build();

		final TypeSpec serviceClass = TypeSpec.classBuilder(serviceName)
												.addJavadoc(CodeBlock.builder().add("This class acts as a service layer to encapsulate domain logic.\n\n")
																				.add("Add this line to classes that wish to use it: @Inject private transient " + serviceName + " service;")
																				.build())
												.addModifiers(Modifier.PUBLIC)
												.addAnnotation(AnnotationSpec.builder(ClassName.get(Default.class)).build())
												.addField(FieldSpec.builder(Persistence.class, "persistence")
																		.addAnnotation(Inject.class)
																		.addModifiers(Modifier.PRIVATE)
																		.build())
												.addMethod(get).addMethod(getAll).build();

		final JavaFile javaFile = JavaFile.builder("modules." + moduleName + "." + documentName, serviceClass).indent("\t").build();

		try {
			javaFile.writeTo(Paths.get(srcDir));
		} catch (IOException e) {
			LOGGER.warn("Failed to scaffold document service for document '{}' in module '{}'.", documentName, moduleName, e);
		}
	}

	/**
	 * Returns the generated extension class name for the current document.
	 *
	 * @return the extension class name
	 */
	private String getExtensionName() {
		return documentName + "Extension";
	}

	/**
	 * Returns the output path for the generated service class.
	 *
	 * @return the service source file path
	 */
	private Path getServiceClassPath() {
		return Paths.get(srcDir, "modules", moduleName, documentName, documentName + "Service.java");
	}
}
