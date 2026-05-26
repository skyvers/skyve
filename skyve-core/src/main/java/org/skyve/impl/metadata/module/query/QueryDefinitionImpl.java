package org.skyve.impl.metadata.module.query;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.QueryDefinition;

import com.google.common.base.MoreObjects;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract base for all query definition implementations.
 *
 * <p>Holds the properties common to every query type: name, description,
 * documentation, and the target module/document context.  Concrete subclasses
 * specialise for MetaData-driven, SQL, and BizQL query types.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see QueryDefinition
 * @see MetaDataQueryDefinitionImpl
 * @see SQLDefinitionImpl
 * @see BizQLDefinitionImpl
 */
public abstract class QueryDefinitionImpl implements QueryDefinition {
	private static final long serialVersionUID = 1867738351262041832L;

	private Module owningModule;
	private String name;
	private String description;
	private String documentation;
	private int timeoutInSeconds = 0;

	private Map<String, String> properties = new TreeMap<>();

	@Override
	public Module getOwningModule() {
		return owningModule;
	}
	
	public void setOwningModule(@Nonnull Module owningModule) {
		this.owningModule = owningModule;
	}

	@Override
	public String getName() {
		return name;
	}

	public void setName(@Nonnull String name) {
		this.name = name;
	}

	@Override
	public String getDescription() {
		return description;
	}

	public void setDescription(@Nonnull String description) {
		this.description = description;
	}

	@Override
	public String getDocumentation() {
		return documentation;
	}

	public void setDocumentation(@Nullable String documentation) {
		this.documentation = documentation;
	}

	@Override
	public int getTimeoutInSeconds() {
		return timeoutInSeconds;
	}

	public void setTimeoutInSeconds(int timeoutInSeconds) {
		this.timeoutInSeconds = timeoutInSeconds;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	@Override
    public String toString() {
		return MoreObjects.toStringHelper(this).add("name", name).toString();
    }
}
