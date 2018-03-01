package org.skyve.impl.tools.test.sail.execution;

import org.skyve.impl.tools.test.sail.language.Procedure;
import org.skyve.impl.tools.test.sail.language.Step;
import org.skyve.impl.tools.test.sail.language.TestCase;
import org.skyve.impl.tools.test.sail.language.TestSuite;

public abstract class InlineSeleneseExecutor extends SeleneseExecutor {
	@Override
	public void execute(TestSuite testSuite) {
		Procedure setup = testSuite.getSetup();
		if (setup != null) {
			startTest("Test Suite Setup");
			for (Step step : setup.getSteps()) {
				step.execute(this);
			}
			endTest();
		}
		for (TestCase testCase : testSuite.getCases()) {
			execute(testCase);
		}
		Procedure tearDown = testSuite.getTearDown();
		if (tearDown != null) {
			startTest("Test Suite Tear Down");
			for (Step step : tearDown.getSteps()) {
				step.execute(this);
			}
			endTest();
		}
	}
	
	@Override
	public void execute(TestCase testCase) {
		
		startTest(testCase.getName());
		Procedure setup = testCase.getSetup();
		if (setup != null) {
			indent().append("<!-- Setup for ").append(testCase.getName()).append(" -->").newline();
			for (Step step : setup.getSteps()) {
				step.execute(this);
			}
		}
		for (Step step : testCase.getSteps()) {
			step.execute(this);
		}
		Procedure tearDown = testCase.getTearDown();
		if (tearDown != null) {
			indent().append("<!-- Tear down for ").append(testCase.getName()).append(" -->").newline();
			for (Step step : tearDown.getSteps()) {
				step.execute(this);
			}
		}
		endTest();
	}
}
