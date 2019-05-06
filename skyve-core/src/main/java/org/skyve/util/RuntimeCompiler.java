package org.skyve.util;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Arrays;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticListener;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.JavaFileObject.Kind;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

import org.skyve.domain.messages.DomainException;

/**
 * Compile a java class definition String and load the class or write it to the file system.
 */
public class RuntimeCompiler {
	public static String COMPILE_PATH = Util.getContentDirectory() + "compile/";
	
	/**
	 * Diagnostic Listener that will throw an exception when something goes wrong with compilation.
	 */
	private static class ExceptionProducingDiagnosticListener implements DiagnosticListener<JavaFileObject> {
		@Override
		public void report(Diagnostic<? extends JavaFileObject> diagnostic) {
			String message = String.format("%s at line %d, column %d\nCode %s\nMessage %s\nSource %s", 
											diagnostic.getKind(),
											Long.valueOf(diagnostic.getLineNumber()),
											Long.valueOf(diagnostic.getColumnNumber()),
											diagnostic.getCode(),
											diagnostic.getMessage(Locale.ENGLISH),
											diagnostic.getSource());
			if (javax.tools.Diagnostic.Kind.ERROR.equals(diagnostic.getKind())) {
				throw new DomainException(message);
			}
			Util.LOGGER.warning(message);
		}
	}

	/**
	 * In memory Java Source 
	 **/
	private static class InMemoryJavaSourceFileObject extends SimpleJavaFileObject {
		private String contents = null;

		private InMemoryJavaSourceFileObject(String className, String contents) throws Exception {
			super(URI.create("string:///" + className.replace('.', '/') + Kind.SOURCE.extension), Kind.SOURCE);
			this.contents = contents;
		}

		@Override
		public CharSequence getCharContent(boolean ignoreEncodingErrors) throws IOException {
			return contents;
		}
	}

	/**
	 * In memory Java Class 
	 **/
	private static class InMemoryJavaClassFileObject extends SimpleJavaFileObject {
		private ByteArrayOutputStream stream = new ByteArrayOutputStream(2048);

		private InMemoryJavaClassFileObject(String className) {
			super(URI.create("bytes:///" + className.replace('.', '/') + Kind.CLASS.extension), Kind.CLASS);
		}

		@Override
		public OutputStream openOutputStream() throws IOException {
			return stream;
		}
		
		byte[] getByteCode() {
			return stream.toByteArray();
		}
	}

	public static class InMemoryClassLoader extends ClassLoader {
		private Map<String, InMemoryJavaClassFileObject> classes = new TreeMap<>();
		
		public void putClass(String fullyQualifiedClassName, InMemoryJavaClassFileObject javaClass) {
			classes.put(fullyQualifiedClassName, javaClass);
		}
		
		@Override
		protected Class<?> findClass(String fullyQualifiedClassName) throws ClassNotFoundException {
			InMemoryJavaClassFileObject classFile = classes.get(fullyQualifiedClassName);
			if (classFile == null) {
				return super.findClass(fullyQualifiedClassName);
			}
			byte[] byteCode = classFile.getByteCode();
			Class<?> result = defineClass(fullyQualifiedClassName, byteCode, 0, byteCode.length);
			if (result == null) {
				throw new ClassNotFoundException(fullyQualifiedClassName);
			}
			return result;
		}
	}
	
	/**
	 * Create a java source file object from the code ready to compile.
	 */
	@SuppressWarnings("synthetic-access")
	public static JavaFileObject javaSource(String fullyQualifiedClassName, String code) {
		JavaFileObject result = null;
		try {
			result = new InMemoryJavaSourceFileObject(fullyQualifiedClassName, code);
		} catch (Exception exception) {
			exception.printStackTrace();
		}
		return result;
	}

	/**
	 * Create a java class file object to compile to.
	 */
	@SuppressWarnings("synthetic-access")
	public static InMemoryJavaClassFileObject javaClass(String fullyQualifiedClassName) {
		InMemoryJavaClassFileObject result = null;
		try {
			result = new InMemoryJavaClassFileObject(fullyQualifiedClassName);
		} catch (Exception exception) {
			exception.printStackTrace();
		}
		return result;
	}

	/**
	 * Compile the source and return true if successful
	 * @param source	The java source to compile
	 * @return	true if successful, otherwise false
	 */
	public static boolean compile(JavaFileObject source,
									String classesFolder,
									String... classPath) throws IOException {
		return compile(Arrays.asList(source), classesFolder, classPath);
	}
	
	/**
	 * Compile the sources and return true if successful
	 * @param sources	The java sources to compile
	 * @return	true if successful, otherwise false
	 */
	public static boolean compile(Iterable<? extends JavaFileObject> sources,
									String classesFolder,
									String... classPath) throws IOException {
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();

		@SuppressWarnings("synthetic-access")
		ExceptionProducingDiagnosticListener diagnosticListener = new ExceptionProducingDiagnosticListener();
		try (StandardJavaFileManager fileManager = compiler.getStandardFileManager(diagnosticListener, null, null)) {
			// Specify classpath and classes output folder
			Iterable<String> options = Arrays.asList("-d", classesFolder, "-cp", Arrays.asList(classPath).stream().collect(Collectors.joining(File.pathSeparator)));
			JavaCompiler.CompilationTask task = compiler.getTask(null, fileManager, diagnosticListener, options, null, sources);
			return Boolean.TRUE.equals(task.call());
		}
	}
	
	public static boolean compile(String sourceFolder,
									String classesFolder,
									String... classPath) {
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		@SuppressWarnings("synthetic-access")
		ExceptionProducingDiagnosticListener diagnosticListener = new ExceptionProducingDiagnosticListener();
		Iterable<String> options = Arrays.asList("-sourcepath", sourceFolder, "-d", classesFolder, "-cp", Arrays.asList(classPath).stream().collect(Collectors.joining(File.pathSeparator)));
		JavaCompiler.CompilationTask task = compiler.getTask(null, null, diagnosticListener, options, null, null);
		return Boolean.TRUE.equals(task.call());
		
//		fileManager.setLocation(StandardLocation.SOURCE_PATH, inputFolder);
//		fileManager.list(StandardLocation.SOURCE_PATH, "", Collections.singleton(Kind.SOURCE), true);
//		compiler.run(null, out, err, "-sourcepath", inputFolder, "-d", outputFolder, "-cp", Arrays.asList(classPath).stream().collect(Collectors.joining(File.pathSeparator)));
	}

	
	/**
	 * Load a single class in a new class loader that's a child of the context class loader.
	 * @param classesFolder
	 * @param fullyQualifiedClassName
	 * @return
	 */
	public static Class<?> loadClass(String classesFolder, String fullyQualifiedClassName) {
		// Create a File object on the root of the directory containing the class file
		File file = new File(classesFolder);

		try {
			// Create a new class loader with the directory
			@SuppressWarnings("resource")
			ClassLoader loader = new URLClassLoader(new URL[] {file.toURI().toURL()},
														Thread.currentThread().getContextClassLoader());
			return loader.loadClass(fullyQualifiedClassName);
		}
		catch (Exception e) {
			throw new DomainException("Could not load class " + fullyQualifiedClassName + " from classes folder " + classesFolder, e);
		}
	}

	/**
	 * Compile the sources and return true if successful
	 * @param sources	The java sources to compile
	 * @return	true if successful, otherwise false
	 */
	public static InMemoryClassLoader compile(Iterable<? extends JavaFileObject> sources,
												String... classPath) throws Exception {
		InMemoryClassLoader result = new InMemoryClassLoader();
		
		JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
		@SuppressWarnings("synthetic-access")
		ExceptionProducingDiagnosticListener diagnosticListener = new ExceptionProducingDiagnosticListener();
		try (StandardJavaFileManager fileManager = compiler.getStandardFileManager(diagnosticListener, null, null)) {
			try (ForwardingJavaFileManager<JavaFileManager> forwardingFileManager = new ForwardingJavaFileManager<JavaFileManager>(fileManager) {
				@Override
				public JavaFileObject getJavaFileForOutput(Location location,
															String className,
															Kind kind,
															FileObject sibling) throws IOException {
					// Only for default package and module classes
					if ((className.indexOf('.') < 0) || className.startsWith("modules.")) {
						@SuppressWarnings("synthetic-access")
						InMemoryJavaClassFileObject javaClass = new InMemoryJavaClassFileObject(className);
						result.putClass(className, javaClass);
						return javaClass;
					}
					return super.getJavaFileForOutput(location, className, kind, sibling);
				}
			}) {

				// Specify classpath and classes output folder
				Iterable<String> options = Arrays.asList("-cp", Arrays.asList(classPath).stream().collect(Collectors.joining(File.pathSeparator)));
				JavaCompiler.CompilationTask task = compiler.getTask(null, forwardingFileManager, diagnosticListener, options, null, sources);
				if (Boolean.FALSE.equals(task.call())) {
					throw new DomainException("Compilation was not successful");
				}
			}
		}
		
		return result;
	}
}
