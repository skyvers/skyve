package org.skyve.toolchain;

import java.io.IOException;
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
 * <p>
 * This mojo creates a new service class for a document.
 * </p>
 * <p>
 * It creates a service class for the specified Document with helper methods
 * for retrieving single and multiple instances.
 * </p>
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
	 * Executes the mojo to create a new service class for a document.
	 * This will create the service class with basic CRUD operations.
	 * 
	 * @throws MojoExecutionException if there is an error during execution
	 */
	@Override
	public void execute() throws MojoExecutionException {
		super.execute();
		createServiceClass();
	}

	/**
	 * Creates the document service class.
	 * This class provides service layer methods for working with the document.
	 * It includes methods for retrieving single and multiple instances.
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
			LOGGER.warn("Failed to scaffold document service.", e);
		}
	}

	/**
	 * Gets the name of the extension class for the document.
	 * 
	 * @return The extension class name (document name + "Extension")
	 */
	private String getExtensionName() {
		return documentName + "Extension";
	}
} 