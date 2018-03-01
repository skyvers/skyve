package org.skyve.impl.tools.test.sail.execution;

import org.skyve.impl.tools.test.sail.language.step.Execute;

public abstract class ScriptExecutor implements Executor {
	private StringBuilder script = new StringBuilder(4096);
	private int indent = 0;
	
	protected ScriptExecutor indent() {
		for (int i = 0; i < indent; i++) {
			script.append('\t');
		}
		return this;
	}
	
	protected ScriptExecutor in() {
		indent++;
		return this;
	}

	protected ScriptExecutor out() {
		indent--;
		return this;
	}
	
	protected ScriptExecutor append(String stuff) {
		if (stuff != null) {
			script.append(stuff);
		}
		return this;
	}
	
	protected ScriptExecutor newline() {
		script.append('\n');
		return this;
	}

	@Override
	public void execute(Execute execute) {
		indent().append(execute.getScript()).newline();
	}
	
	@Override
	public String toString() {
		return script.toString();
	}
}
