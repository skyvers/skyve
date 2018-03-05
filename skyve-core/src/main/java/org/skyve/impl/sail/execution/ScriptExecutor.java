package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.Execute;

public abstract class ScriptExecutor extends NavigationExecutor {
	private StringBuilder script = new StringBuilder(4096);
	private int indent = 0;
	
	protected final ScriptExecutor indent() {
		for (int i = 0; i < indent; i++) {
			script.append('\t');
		}
		return this;
	}
	
	protected final ScriptExecutor in() {
		indent++;
		return this;
	}

	protected final ScriptExecutor out() {
		indent--;
		return this;
	}
	
	protected final ScriptExecutor append(String stuff) {
		if (stuff != null) {
			script.append(stuff);
		}
		return this;
	}
	
	protected final ScriptExecutor newline() {
		script.append('\n');
		return this;
	}

	@Override
	public final void execute(Execute execute) {
		indent().append(execute.getScript()).newline();
	}
	
	@Override
	public final String toString() {
		return script.toString();
	}
}
