package org.skyve.wildcat.tools.test.wail.language;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlTransient;

@XmlTransient
public abstract class Procedure implements Executable {
	private List<Step> steps = new ArrayList<>();

	public List<Step> getSteps() {
		return steps;
	}

	@Override
	public void execute(StringBuilder script) {
		for (Step step : steps) {
			step.execute(script);
		}
	}
}
