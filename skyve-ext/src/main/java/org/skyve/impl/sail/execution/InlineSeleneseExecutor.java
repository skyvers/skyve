package org.skyve.impl.sail.execution;

import org.skyve.metadata.sail.language.Procedure;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.Interaction;
import org.skyve.metadata.sail.language.Automation;

public abstract class InlineSeleneseExecutor<T extends AutomationContext> extends SeleneseExecutor<T> {
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
			for (Step step : interaction.getSteps()) {
				step.execute(this);
			}
		}
		endTest();
	}
}
