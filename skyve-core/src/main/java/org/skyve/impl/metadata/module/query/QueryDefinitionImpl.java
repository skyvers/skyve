package org.skyve.impl.metadata.module.query;

import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.QueryDefinition;

public abstract class QueryDefinitionImpl implements QueryDefinition {
	private static final long serialVersionUID = 1867738351262041832L;

	private Module owningModule;

	private String name;

	private String description;

	private String documentation;
	
	private int timeoutInSeconds = 0;

	@Override
	public Module getOwningModule() {
		return owningModule;
	}
	
	public void setOwningModule(Module owningModule) {
		this.owningModule = owningModule;
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	@Override
	public String getDocumentation() {
		return documentation;
	}

	public void setDocumentation(String documentation) {
		this.documentation = documentation;
	}

	@Override
	public int getTimeoutInSeconds() {
		return timeoutInSeconds;
	}

	public void setTimeoutInSeconds(int timeoutInSeconds) {
		this.timeoutInSeconds = timeoutInSeconds;
	}
}
