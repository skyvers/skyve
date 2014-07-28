package org.skyve.wildcat.metadata.flow;

import java.util.ArrayList;
import java.util.List;

public class Transition {
	private String stateName;
	private String stateDisplayName;
	private String guardCondition;
	private List<Action> actions = new ArrayList<>();

	public String getStateName() {
		return stateName;
	}

	public void setStateName(String stateName) {
		this.stateName = stateName;
	}

	public String getStateDisplayName() {
		return stateDisplayName;
	}

	public void setStateDisplayName(String stateDisplayName) {
		this.stateDisplayName = stateDisplayName;
	}

	public String getGuardCondition() {
		return guardCondition;
	}

	public void setGuardCondition(String guardCondition) {
		this.guardCondition = guardCondition;
	}

	public List<Action> getActions() {
		return actions;
	}
}
