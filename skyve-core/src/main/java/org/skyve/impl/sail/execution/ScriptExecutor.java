package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.Execute;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class ScriptExecutor<T extends AutomationContext> extends ContextualExecutor<T> {
	private StringBuilder script = new StringBuilder(4096);
	private int indent = 0;

	// NB An instance member LOGGER is OK here as this is not Serializable
    protected final Logger LOGGER = LoggerFactory.getLogger(getClass());

	@Override
	public void executeAutomation(Automation automation) {
		super.executeAutomation(automation); // set context defaults

		Procedure before = automation.getBefore();
		if (before != null) {
			startTest("Before Automation");
			for (Step step : before.getSteps()) {
				step.execute(this);
			}
			endTest();
		}
		for (Interaction interaction : automation.getInteractions()) {
			executeInteraction(interaction);
		}
		Procedure after = automation.getAfter();
		if (after != null) {
			startTest("After Automation");
			for (Step step : after.getSteps()) {
				step.execute(this);
			}
			endTest();
		}
	}
	
	@Override
	public void executeInteraction(Interaction interaction) {
		LOGGER.info("Execute Interaction {}", interaction.getName());
		startTest(interaction.getName());
		Procedure before = interaction.getBefore();
		if (before != null) {
			indent().append("<!-- Before ").append(interaction.getName()).append(" -->").newline();
			for (Step step : before.getSteps()) {
				step.execute(this);
			}
		}
		for (Step step : interaction.getSteps()) {
			step.execute(this);
		}
		Procedure after = interaction.getAfter();
		if (after != null) {
			indent().append("<!-- After ").append(interaction.getName()).append(" -->").newline();
			for (Step step : after.getSteps()) {
				step.execute(this);
			}
		}
		endTest();
	}

	protected abstract void startTest(String heading);
	protected abstract void endTest();

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
	public void executeExecute(Execute execute) {
		indent().append(execute.getScript()).newline();
	}
	
	@Override
	public String toString() {
		return script.toString();
	}
}
