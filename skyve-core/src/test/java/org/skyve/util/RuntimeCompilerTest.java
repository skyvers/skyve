package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.lang.reflect.Method;
import java.util.Arrays;

import javax.tools.JavaFileObject;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.RuntimeCompiler.InMemoryClassLoader;

@SuppressWarnings("static-method")
public class RuntimeCompilerTest {

	private static final String SIMPLE_JAVA =
		"public class SimpleCalc { " +
		"  public int add() { return 1 + 2; } " +
		"} ";

	@Test
	public void compilePathIsNotNull() {
		assertNotNull(RuntimeCompiler.COMPILE_PATH);
		assertFalse(RuntimeCompiler.COMPILE_PATH.isEmpty());
	}

	@Test
	public void javaSourceReturnsNotNull() {
		JavaFileObject result = RuntimeCompiler.javaSource("SimpleCalc", SIMPLE_JAVA);
		assertNotNull(result);
	}

	@Test
	public void javaClassReturnsNotNull() {
		var result = RuntimeCompiler.javaClass("SimpleCalc");
		assertNotNull(result);
	}

	@Test
	public void inMemoryClassLoaderThrowsForUnknownClass() {
		InMemoryClassLoader loader = new InMemoryClassLoader();
		assertThrows(ClassNotFoundException.class, () -> loader.loadClass("com.unknown.DoesNotExist"));
	}

	@Test
	public void compileAndLoadClassFromFileSystem() throws Exception {
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
				classFile.delete();
			}
		}
	}

	@Test
	public void inMemoryCompileAndLoadClass() throws Exception {
		JavaFileObject file = RuntimeCompiler.javaSource("SimpleCalc", SIMPLE_JAVA);
		InMemoryClassLoader cl = RuntimeCompiler.compile(Arrays.asList(file));
		Class<?> type = cl.loadClass("SimpleCalc");
		assertNotNull(type);
		Method addMethod = type.getDeclaredMethod("add");
		Object result = addMethod.invoke(type.getDeclaredConstructor().newInstance());
		org.junit.jupiter.api.Assertions.assertEquals(Integer.valueOf(3), result);
	}

	@Test
	public void compilingInvalidJavaThrowsRuntimeException() {
		JavaFileObject badFile = RuntimeCompiler.javaSource("BadClass", "public class BadClass { INVALID CODE }");
		RuntimeException e = assertThrows(RuntimeException.class, () -> RuntimeCompiler.compile(badFile, UtilImpl.TEMP_DIRECTORY));
		assertTrue(e.getCause() instanceof DomainException);
	}

	@Test
	public void loadClassThrowsDomainExceptionForMissingClassFile() {
		assertThrows(DomainException.class, () -> RuntimeCompiler.loadClass(UtilImpl.TEMP_DIRECTORY, "com.does.not.Exist"));
	}
}
