package org.skyve.util;

import java.io.File;
import java.lang.reflect.Method;
import java.util.Arrays;

import javax.tools.JavaFileObject;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.RuntimeCompiler.InMemoryClassLoader;

public class RuntimeCompilerTest {
	private static final String JAVA =  "public class Calculator { " + 
										"  public int testAdd() { " +
										"    return 200+300; " + 
										"  } " + 
										"} ";

	@Test
	@SuppressWarnings("static-method")
	public void testCompileAndLoad() throws Exception {
		JavaFileObject file = RuntimeCompiler.javaSource("Calculator", JAVA);

		try {
			RuntimeCompiler.compile(file, UtilImpl.TEMP_DIRECTORY);
			Class<?> type = RuntimeCompiler.loadClass(UtilImpl.TEMP_DIRECTORY, "Calculator");
			Method testAdd = type.getDeclaredMethod("testAdd");
			Assert.assertEquals(Integer.valueOf(500), testAdd.invoke(type.getDeclaredConstructor().newInstance()));
		}
		finally {
			File classFile = new File(UtilImpl.TEMP_DIRECTORY, "Calculator.class");
			if (classFile.exists()) {
				classFile.delete();
			}
		}
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testClassLoader() throws Exception {
		JavaFileObject file = RuntimeCompiler.javaSource("Calculator", JAVA);

		InMemoryClassLoader cl = RuntimeCompiler.compile(Arrays.asList(file));
		Class<?> type = cl.loadClass("Calculator");
		Method testAdd = type.getDeclaredMethod("testAdd");
		Assert.assertEquals(Integer.valueOf(500), testAdd.invoke(type.getDeclaredConstructor().newInstance()));
	}
}
