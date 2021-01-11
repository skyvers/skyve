package org.skyve.toolchain;

import com.squareup.javapoet.*;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.components.interactivity.PrompterException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import javax.lang.model.element.Modifier;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

@Mojo(name = "newAction")
public class NewActionMojo extends AbstractSkyveMojo {

    /**
     * Relative source directory.
     */
    @Parameter(required = true, defaultValue = "src/main/java/")
    private String srcDir;

    protected String moduleName;
    protected String documentName;
    protected String actionName;
    protected File documentDirectory;
    protected File actionsDirectory;

    @Override
    public void execute() throws MojoExecutionException {
        try {
            final Path modulesDirectory = getModulesDirectory(project);

            moduleName = prompter.prompt("Please enter a module name");
            final Path moduleDirectory = modulesDirectory.resolve(moduleName);
            if (!moduleDirectory.toFile().exists()) {
                throw new MojoExecutionException(String.format("Directory %s for module %s does not exist.",
                        moduleDirectory.toFile().getAbsolutePath(), moduleName));
            }

            final Path absoluteSrcPath =  modulesDirectory.getParent();

            // The XMLMetaData class requires this path to be set to the src directory.
            UtilImpl.APPS_JAR_DIRECTORY = absoluteSrcPath.toFile().getAbsolutePath() + File.separator;

            documentName = prompter.prompt("Please enter a document name");
            documentDirectory = moduleDirectory.resolve(documentName).toFile();
            if (!documentDirectory.exists()) {
                throw new MojoExecutionException(String.format("Directory %s for new document %s does not exist.",
                        documentDirectory.getAbsolutePath(), documentName));
            }

            actionName = prompter.prompt("Please enter an action name");
            actionsDirectory = moduleDirectory.resolve(documentName).resolve("actions").toFile();
            if (!actionsDirectory.exists()) {
                if (!actionsDirectory.mkdir()) {
                    throw new MojoExecutionException(String.format("Failed to create actions directory %s for new action %s.",
                            actionsDirectory.getAbsolutePath(), actionName));
                }
            }
            createActionClass();
        } catch (PrompterException | IOException e) {
            throw new MojoExecutionException("Failed to create new action.", e);
        }
    }

    private Path getModulesDirectory(MavenProject project) throws FileNotFoundException {
        for (final String sourceRoot : project.getCompileSourceRoots()) {
            final Path modules = Paths.get(sourceRoot, "modules");
            if (modules.toFile().exists()) {
                return modules;
            }
        }

        throw new FileNotFoundException("Failed to find modules directory.");
    }

    private void createActionClass() throws IOException {
        final String extensionName = documentName + "Extension";
        final Path extensionClassPath = documentDirectory.toPath().resolve(extensionName + ".java");
        final boolean extensionClassExists = extensionClassPath.toFile().exists();

        final TypeName documentClass = extensionClassExists ? ClassName.get("modules." + moduleName + "." + documentName, extensionName) : ClassName.get("modules." + moduleName + ".domain", documentName);
        final MethodSpec execute = MethodSpec.methodBuilder("execute")
                .addModifiers(Modifier.PUBLIC)
                .returns(ParameterizedTypeName.get(ClassName.get(ServerSideActionResult.class), documentClass))
                .addParameter(documentClass, "bean")
                .addParameter(WebContext.class, "webContext")
                .addStatement("return new $T<>(bean)", ServerSideActionResult.class)
                .addAnnotation(AnnotationSpec.builder(ClassName.get(Override.class)).build())
                .build();

        final TypeSpec actionClass = TypeSpec.classBuilder(actionName)
                .addModifiers(Modifier.PUBLIC)
                .addSuperinterface(ParameterizedTypeName.get(ClassName.get(ServerSideAction.class), documentClass))
                .addMethod(execute)
                .build();

        final JavaFile javaFile = JavaFile.builder("modules." + moduleName + "." + documentName + ".actions", actionClass)
                .indent("\t")
                .build();

        javaFile.writeTo(Paths.get(srcDir));
    }
}
