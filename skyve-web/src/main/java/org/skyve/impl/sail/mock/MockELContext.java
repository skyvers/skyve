package org.skyve.impl.sail.mock;

import jakarta.el.ELContext;
import jakarta.el.ELResolver;
import jakarta.el.FunctionMapper;
import jakarta.el.VariableMapper;

public class MockELContext extends ELContext {
	@Override
	public ELResolver getELResolver() {
		return null;
	}

	@Override
	public FunctionMapper getFunctionMapper() {
		return null;
	}

	@Override
	public VariableMapper getVariableMapper() {
		return null;
	}
}
