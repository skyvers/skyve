package org.skyve.toolchain.flutter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.FileNotFoundException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.components.interactivity.Prompter;
import org.codehaus.plexus.components.interactivity.PrompterException;
import org.jboss.weld.environment.se.Weld;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.generate.client.flutter.FlutterGenerator.GeneratorConfig;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.RDBMSDynamicPersistence;
import org.skyve.impl.persistence.hibernate.HibernateContentPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;
import org.springframework.test.util.ReflectionTestUtils;

@SuppressWarnings("static-method")
class FlutterInitMojoTest {
	@TempDir
	Path tempDir;

	@Test
	void prepareTargetDirectoryCreatesMissingDirectory() throws Exception {
		FlutterInitMojo mojo = newMojo();
		Path target = tempDir.resolve("generated");

		Path result = prepareTargetDirectory(mojo, target, false);

		assertEquals(target, result);
		assertTrue(Files.isDirectory(target));
	}

	@Test
	void executeBuildsGeneratorConfigAndShutsDownWeld() throws Exception {
		TestFlutterInitMojo mojo = new TestFlutterInitMojo();
		Path target = tempDir.resolve("flutter");
		configureRequiredFields(mojo, target);

		mojo.execute();

		GeneratorConfig config = mojo.generatedConfig;
		assertEquals("edit", config.getUxui());
		assertEquals("demo_app", config.getProjectName());
		assertEquals(target.toAbsolutePath().toString(), config.getProjectPath());
		assertEquals("demo", config.getCustomerName());
		assertTrue(config.allowsMoDoc("admin", "User"));
		assertTrue(config.allowsMoDoc("sales", "Order"));
		verify(mojo.weld).shutdown();
	}

	@Test
	void executeWrapsGeneratorFailureAndShutsDownWeld() {
		TestFlutterInitMojo mojo = new TestFlutterInitMojo();
		configureRequiredFields(mojo, tempDir.resolve("flutter"));
		mojo.generatorFailure = new IllegalStateException("boom");

		MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);

		assertTrue(exception.getMessage().contains("Error while generating project"));
		verify(mojo.weld).shutdown();
	}

	@Test
	void executeWrapsPromptFailure() throws Exception {
		TestFlutterInitMojo mojo = new TestFlutterInitMojo();
		Prompter prompter = mock(Prompter.class);
		when(prompter.prompt("Please enter the target output directory")).thenThrow(new PrompterException("cancelled"));
		ReflectionTestUtils.setField(mojo, "customer", "demo");
		ReflectionTestUtils.setField(mojo, "prompter", prompter);

		MojoFailureException exception = assertThrows(MojoFailureException.class, mojo::execute);

		assertTrue(exception.getMessage().contains("Error assembling required parameters"));
	}

	@Test
	void executeWrapsMissingCustomerDetails() {
		MissingCustomerFlutterInitMojo mojo = new MissingCustomerFlutterInitMojo();

		MojoExecutionException exception = assertThrows(MojoExecutionException.class, mojo::execute);

		assertTrue(exception.getMessage().contains("Error loading customer details"));
	}

	@Test
	void bootstrapSkyveConfiguresRuntimeStateAndPersistenceUser() throws Exception {
		BootstrapFlutterInitMojo mojo = new BootstrapFlutterInitMojo();
		ReflectionTestUtils.setField(mojo, "customer", "demo");
		ReflectionTestUtils.setField(mojo, "srcDir", tempDir.toString());
		ReflectionTestUtils.setField(mojo, "project", projectWithOutputDirectory(tempDir.resolve("classes")));

		mojo.bootstrapSkyve();

		assertEquals(HibernateContentPersistence.class, AbstractPersistence.IMPLEMENTATION_CLASS);
		assertEquals(RDBMSDynamicPersistence.class, AbstractPersistence.DYNAMIC_IMPLEMENTATION_CLASS);
		assertEquals(NoOpContentManager.class, AbstractContentManager.IMPLEMENTATION_CLASS);
		assertEquals(tempDir.resolve("classes").toString() + java.io.File.separator, UtilImpl.APPS_JAR_DIRECTORY);
		assertEquals("jdbc:h2:mem:test", UtilImpl.DATA_STORE.getJdbcUrl());
		assertFalse(UtilImpl.DDL_SYNC);
		assertFalse(UtilImpl.SQL_TRACE);
		assertFalse(UtilImpl.QUERY_TRACE);
		assertFalse(UtilImpl.JOB_SCHEDULER);
		assertTrue(UtilImpl.DATA_STORES.containsKey("test"));

		ArgumentCaptor<User> user = ArgumentCaptor.forClass(User.class);
		verify(mojo.persistence).setUser(user.capture());
		assertEquals("demo", user.getValue().getCustomerName());
		assertEquals("flutter-gen-user", user.getValue().getName());
		assertEquals("flutter-gen-user", user.getValue().getId());
	}

	@Test
	void prepareTargetDirectoryFailsWhenDirectoryExistsWithoutOverwrite() throws Exception {
		FlutterInitMojo mojo = newMojo();
		Path target = tempDir.resolve("existing");
		Files.createDirectories(target);

		MojoFailureException exception = assertThrows(MojoFailureException.class,
				() -> prepareTargetDirectory(mojo, target, false));

		assertTrue(exception.getMessage().contains("overwrite=false"));
	}

	@Test
	void prepareTargetDirectoryClearsExistingDirectoryWhenOverwriteEnabled() throws Exception {
		FlutterInitMojo mojo = newMojo();
		Path target = tempDir.resolve("existing");
		Path childFile = target.resolve("file.txt");
		Path childDirectory = target.resolve("nested");
		Files.createDirectories(childDirectory);
		Files.writeString(childFile, "stale");
		Files.writeString(childDirectory.resolve("nested.txt"), "stale");

		Path result = prepareTargetDirectory(mojo, target, true);

		assertEquals(target, result);
		assertTrue(Files.isDirectory(target));
		assertFalse(Files.exists(childFile));
		assertFalse(Files.exists(childDirectory));
	}

	@Test
	void prepareTargetDirectoryWrapsClearFailures() throws Exception {
		FlutterInitMojo mojo = newMojo();
		Path target = tempDir.resolve("existing-file");
		Files.writeString(target, "not a directory");

		MojoExecutionException exception = assertThrows(MojoExecutionException.class,
				() -> prepareTargetDirectory(mojo, target, true));

		assertTrue(exception.getMessage().contains("Unable to clear target directory"));
	}

	@Test
	void prepareTargetDirectoryWrapsCreateFailures() throws Exception {
		FlutterInitMojo mojo = newMojo();
		Path blockedParent = Files.writeString(tempDir.resolve("blocked-parent"), "");

		MojoExecutionException exception = assertThrows(MojoExecutionException.class,
				() -> prepareTargetDirectory(mojo, blockedParent.resolve("child"), false));

		assertTrue(exception.getMessage().contains("Unabled to create target directory"));
	}

	private static FlutterInitMojo newMojo() {
		FlutterInitMojo mojo = new FlutterInitMojo();
		mojo.setLog(mock(Log.class));
		return mojo;
	}

	private static void configureRequiredFields(FlutterInitMojo mojo, Path target) {
		ReflectionTestUtils.setField(mojo, "customer", "demo");
		ReflectionTestUtils.setField(mojo, "targetDir", target.toString());
		ReflectionTestUtils.setField(mojo, "uxui", "edit");
		ReflectionTestUtils.setField(mojo, "projectName", "demo_app");
		ReflectionTestUtils.setField(mojo, "modocWhitelist", List.of("admin.User", "sales.*"));
	}

	private static MavenProject projectWithOutputDirectory(Path outputDirectory) {
		MavenProject project = new MavenProject();
		Build build = new Build();
		build.setOutputDirectory(outputDirectory.toString());
		project.setBuild(build);
		return project;
	}

	private static Path prepareTargetDirectory(FlutterInitMojo mojo, Path target, boolean clear)
	throws MojoExecutionException, MojoFailureException {
		try {
			Method method = FlutterInitMojo.class.getDeclaredMethod("prepareTargetDirectory", String.class, boolean.class);
			method.setAccessible(true);
			return (Path) method.invoke(mojo, target.toString(), Boolean.valueOf(clear));
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof MojoExecutionException mojoExecutionException) {
				throw mojoExecutionException;
			}
			if (cause instanceof MojoFailureException mojoFailureException) {
				throw mojoFailureException;
			}
			throw new AssertionError(cause);
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	private static final class TestFlutterInitMojo extends FlutterInitMojo {
		private final Weld weld = mock(Weld.class);
		private GeneratorConfig generatedConfig;
		private RuntimeException generatorFailure;

		private TestFlutterInitMojo() {
			setLog(mock(Log.class));
		}

		@Override
		Weld bootstrapSkyve() {
			return weld;
		}

		@Override
		void generate(GeneratorConfig config) {
			generatedConfig = config;
			if (generatorFailure != null) {
				throw generatorFailure;
			}
		}
	}

	private static final class BootstrapFlutterInitMojo extends FlutterInitMojo {
		private final AbstractPersistence persistence = mock(AbstractPersistence.class);

		private BootstrapFlutterInitMojo() {
			setLog(mock(Log.class));
		}

		@Override
		protected void configureClasspath(String srcDir) throws DependencyResolutionRequiredException, MalformedURLException {
			// The bootstrap test covers runtime state configuration without mutating the thread context class loader.
		}

		@Override
		void initialize(Weld weld) {
			// Avoid starting Weld; the real method is exercised through execute-level tests via the hook boundary.
		}

		@Override
		void setRepository() {
			// Avoid loading design metadata from disk.
		}

		@Override
		AbstractPersistence getPersistence() {
			return persistence;
		}
	}

	private static final class MissingCustomerFlutterInitMojo extends FlutterInitMojo {
		private MissingCustomerFlutterInitMojo() {
			setLog(mock(Log.class));
		}

		@Override
		protected String getDefaultOrPromptCustomer(String defaultCustomer) throws FileNotFoundException {
			throw new FileNotFoundException("customers");
		}
	}
}
