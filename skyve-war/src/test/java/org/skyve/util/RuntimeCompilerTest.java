package org.skyve.util;

import java.io.File;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.util.Arrays;

import javax.tools.JavaFileObject;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.RuntimeCompiler.InMemoryClassLoader;

class RuntimeCompilerTest {
	private static final String JAVA =  "public class Calculator { " + 
										"  public int testAdd() { " +
										"    return 200+300; " + 
										"  } " + 
										"} ";

	@Test
	@SuppressWarnings("static-method")
	void testCompileAndLoad() throws Exception {
		JavaFileObject file = RuntimeCompiler.javaSource("Calculator", JAVA);

		try {
			RuntimeCompiler.compile(file, UtilImpl.TEMP_DIRECTORY);
			Class<?> type = RuntimeCompiler.loadClass(UtilImpl.TEMP_DIRECTORY, "Calculator");
			Method testAdd = type.getDeclaredMethod("testAdd");
			Assertions.assertEquals(Integer.valueOf(500), testAdd.invoke(type.getDeclaredConstructor().newInstance()));
		}
		finally {
			File classFile = new File(UtilImpl.TEMP_DIRECTORY, "Calculator.class");
			if (classFile.exists()) {
				Files.delete(classFile.toPath());
			}
		}
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testClassLoader() throws Exception {
		JavaFileObject file = RuntimeCompiler.javaSource("Calculator", JAVA);

		InMemoryClassLoader cl = RuntimeCompiler.compile(Arrays.asList(file));
		Class<?> type = cl.loadClass("Calculator");
		Method testAdd = type.getDeclaredMethod("testAdd");
		Assertions.assertEquals(Integer.valueOf(500), testAdd.invoke(type.getDeclaredConstructor().newInstance()));
	}
}
