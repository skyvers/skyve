package org.skyve.impl.metadata.module.query;

import java.util.Map;

import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.QueryDefinition;

import com.google.common.base.MoreObjects;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public abstract class QueryReferenceImpl implements QueryDefinition {
	private static final long serialVersionUID = -8409852834346167656L;

	private Module owningModule;
	private String name;
	protected String moduleRef;
	protected String ref;
	
	protected QueryReferenceImpl(@Nullable String name, @Nonnull String moduleRef, @Nonnull String ref) {
		this.name = name;
		this.moduleRef = moduleRef;
		this.ref = ref;
	}
	
	@Override
	public Module getOwningModule() {
		return owningModule;
	}

	public void setOwningModule(Module owningModule) {
		this.owningModule = owningModule;
	}

	public String getModuleRef() {
		return moduleRef;
	}
	
	public String getRef() {
		return ref;
	}
	
	@Override
	public String getName() {
		return (name == null) ? ref : name;
	}

	@Override
	public String getDescription() {
		return getTarget().getDescription();
	}
	
	@Override
	public String getDocumentation() {
		return getTarget().getDocumentation();
	}

	@Override
	public int getTimeoutInSeconds() {
		return getTarget().getTimeoutInSeconds();
	}
	
	@Override
	public Map<String, String> getProperties() {
		return getTarget().getProperties();
	}
	
	protected abstract @Nonnull <T extends QueryDefinition> T getTarget();
	
	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
							.add("name", name)
							.add("moduleRef", moduleRef)
							.add("ref", ref)
							.toString();
	}
}
