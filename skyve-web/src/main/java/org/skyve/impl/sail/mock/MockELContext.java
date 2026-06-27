package org.skyve.impl.sail.mock;

import jakarta.el.ELContext;
import jakarta.el.ELResolver;
import jakarta.el.FunctionMapper;
import jakarta.el.VariableMapper;

/**
 * Provides a mock implementation used by SAIL execution tests in the web module.
 */
public class MockELContext extends ELContext {
	/**
	 * Returns no resolver because expression resolution is not required in these tests.
	 */
	@Override
	public ELResolver getELResolver() {
		return null;
	}

	/**
	 * Returns no function mapper because function evaluation is not required in these tests.
	 */
	@Override
	public FunctionMapper getFunctionMapper() {
		return null;
	}

	/**
	 * Returns no variable mapper because variable evaluation is not required in these tests.
	 */
	@Override
	public VariableMapper getVariableMapper() {
		return null;
	}
}
