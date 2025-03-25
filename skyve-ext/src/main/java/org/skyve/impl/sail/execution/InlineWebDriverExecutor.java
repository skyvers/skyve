package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;

public abstract class InlineWebDriverExecutor<T extends AutomationContext> extends WebDriverExecutor<T> {
	@Override
	public void executeAutomation(Automation automation) {
		super.executeAutomation(automation); // set context defaults

		in();
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
		LOGGER.info("Execute Interaction " + interaction.getName());
		startTest(interaction.getName());
		Procedure before = interaction.getBefore();
		if (before != null) {
			indent().append("// Before ").append(interaction.getName()).newline();
			for (Step step : before.getSteps()) {
				step.execute(this);
			}
		}
		for (Step step : interaction.getSteps()) {
			step.execute(this);
		}
		Procedure after = interaction.getAfter();
		if (after != null) {
			indent().append("// After ").append(interaction.getName()).newline();
			for (Step step : after.getSteps()) {
				step.execute(this);
			}
		}
		endTest();
	}
}
