package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.step.Execute;

public abstract class ScriptExecutor<T extends AutomationContext> extends ContextualExecutor<T> {
	private StringBuilder script = new StringBuilder(4096);
	private int indent = 0;
	
	public final ScriptExecutor<T> indent() {
		for (int i = 0; i < indent; i++) {
			script.append('\t');
		}
		return this;
	}
	
	public final ScriptExecutor<T> in() {
		indent++;
		return this;
	}

	public final ScriptExecutor<T> out() {
		indent--;
		return this;
	}
	
	public final ScriptExecutor<T> append(String stuff) {
		if (stuff != null) {
			script.append(stuff);
		}
		return this;
	}
	
	public final ScriptExecutor<T> newline() {
		script.append('\n');
		return this;
	}

	@Override
	public final void executeExecute(Execute execute) {
		indent().append(execute.getScript()).newline();
	}
	
	@Override
	public String toString() {
		return script.toString();
	}
}
