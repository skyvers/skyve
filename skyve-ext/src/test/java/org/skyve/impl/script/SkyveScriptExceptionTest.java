package org.skyve.impl.script;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.skyve.impl.script.SkyveScriptException.ExceptionType;

@SuppressWarnings("static-method")
public class SkyveScriptExceptionTest {

	@Test
	public void testConstructorMessageAndLineNumber() {
		SkyveScriptException ex = new SkyveScriptException(ExceptionType.error, "Something went wrong", 42);
		assertEquals("Something went wrong", ex.getMessage());
		assertEquals(42, ex.getLineNumber());
		assertEquals(ExceptionType.error, ex.getType());
	}

	@Test
	public void testConstructorWithI18nValues() {
		SkyveScriptException ex = new SkyveScriptException(ExceptionType.warning, "msg.key", 10, "param1");
		assertEquals(10, ex.getLineNumber());
		assertEquals(ExceptionType.warning, ex.getType());
	}

	@Test
	public void testConstructorWithI18nFlag() {
		SkyveScriptException ex = new SkyveScriptException(ExceptionType.info, "raw message", 5, false);
		assertEquals("raw message", ex.getMessage());
		assertEquals(5, ex.getLineNumber());
		assertEquals(ExceptionType.info, ex.getType());
	}

	@Test
	public void testExceptionTypeValues() {
		ExceptionType[] values = ExceptionType.values();
		assertNotNull(values);
		assertEquals(4, values.length);
	}

	@Test
	public void testExceptionTypeInfo() {
		assertEquals(ExceptionType.info, ExceptionType.valueOf("info"));
	}

	@Test
	public void testExceptionTypeWarning() {
		assertEquals(ExceptionType.warning, ExceptionType.valueOf("warning"));
	}

	@Test
	public void testExceptionTypeError() {
		assertEquals(ExceptionType.error, ExceptionType.valueOf("error"));
	}

	@Test
	public void testExceptionTypeCritical() {
		assertEquals(ExceptionType.critical, ExceptionType.valueOf("critical"));
	}

	@Test
	public void testExceptionIsThrowable() {
		SkyveScriptException ex = new SkyveScriptException(ExceptionType.critical, "critical error", 99);
		try {
			throw ex;
		}
		catch (SkyveScriptException caught) {
			assertEquals(99, caught.getLineNumber());
			assertEquals(ExceptionType.critical, caught.getType());
		}
	}
}
