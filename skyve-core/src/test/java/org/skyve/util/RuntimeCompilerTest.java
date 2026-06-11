package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.util.Arrays;

import javax.tools.JavaFileObject;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.RuntimeCompiler.InMemoryClassLoader;

@SuppressWarnings("static-method")
class RuntimeCompilerTest {

	private static final String SIMPLE_JAVA =
		"public class SimpleCalc { " +
		"  public int add() { return 1 + 2; } " +
		"} ";

	@Test
	void compilePathIsNotNull() {
		assertNotNull(RuntimeCompiler.COMPILE_PATH);
		assertFalse(RuntimeCompiler.COMPILE_PATH.isEmpty());
	}

	@Test
	void javaSourceReturnsNotNull() {
		JavaFileObject result = RuntimeCompiler.javaSource("SimpleCalc", SIMPLE_JAVA);
		assertNotNull(result);
	}

	@Test
	void javaClassReturnsNotNull() {
		var result = RuntimeCompiler.javaClass("SimpleCalc");
		assertNotNull(result);
	}

	@Test
	void inMemoryClassLoaderThrowsForUnknownClass() {
		InMemoryClassLoader loader = new InMemoryClassLoader();
		assertThrows(ClassNotFoundException.class, () -> loader.loadClass("com.unknown.DoesNotExist"));
	}

	@Test
	void compileAndLoadClassFromFileSystem() throws Exception {
		JavaFileObject file = RuntimeCompiler.javaSource("SimpleCalc", SIMPLE_JAVA);
		try {
			boolean success = RuntimeCompiler.compile(file, UtilImpl.TEMP_DIRECTORY);
			assertTrue(success);
			Class<?> type = RuntimeCompiler.loadClass(UtilImpl.TEMP_DIRECTORY, "SimpleCalc");
			assertNotNull(type);
			Method addMethod = type.getDeclaredMethod("add");
			Object result = addMethod.invoke(type.getDeclaredConstructor().newInstance());
			org.junit.jupiter.api.Assertions.assertEquals(Integer.valueOf(3), result);
		} finally {
			File classFile = new File(UtilImpl.TEMP_DIRECTORY, "SimpleCalc.class");
			if (classFile.exists()) {
				Files.delete(classFile.toPath());
			}
		}
	}

	@Test
	void inMemoryCompileAndLoadClass() throws Exception {
		JavaFileObject file = RuntimeCompiler.javaSource("SimpleCalc", SIMPLE_JAVA);
		InMemoryClassLoader cl = RuntimeCompiler.compile(Arrays.asList(file));
		Class<?> type = cl.loadClass("SimpleCalc");
		assertNotNull(type);
		Method addMethod = type.getDeclaredMethod("add");
		Object result = addMethod.invoke(type.getDeclaredConstructor().newInstance());
		org.junit.jupiter.api.Assertions.assertEquals(Integer.valueOf(3), result);
	}

	@Test
	void compilingInvalidJavaThrowsRuntimeException() {
		JavaFileObject badFile = RuntimeCompiler.javaSource("BadClass", "public class BadClass { INVALID CODE }");
		RuntimeException e = assertThrows(RuntimeException.class, () -> RuntimeCompiler.compile(badFile, UtilImpl.TEMP_DIRECTORY));
		assertTrue(e.getCause() instanceof DomainException);
	}

	@Test
	void loadClassThrowsDomainExceptionForMissingClassFile() {
		assertThrows(DomainException.class, () -> RuntimeCompiler.loadClass(UtilImpl.TEMP_DIRECTORY, "com.does.not.Exist"));
	}

	@Test
	void inMemoryClassLoaderDelegatesUnknownClassToParent() throws Exception {
		InMemoryClassLoader loader = new InMemoryClassLoader();
		// String is a well-known JDK class; loadClass should succeed by delegating to the parent ClassLoader
		Class<?> cls = loader.loadClass("java.lang.String");
		assertNotNull(cls);
	}
}
