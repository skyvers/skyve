package org.skyve.impl.metadata.module.query;

import java.util.Map;

import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.QueryDefinition;

import com.google.common.base.MoreObjects;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract base for query reference implementations — lightweight query
 * descriptors that name an existing query without re-specifying its column list.
 *
 * <p>A query reference is used in list or lookup widgets to point at a named
 * query defined elsewhere in the module.  Concrete subclasses specialise for
 * {@link MetaDataQueryReferenceImpl}, {@link SQLReferenceImpl}, and
 * {@link BizQLReferenceImpl}.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see QueryDefinition
 */
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
	public @Nonnull Module getOwningModule() {
		return owningModule;
	}

	public void setOwningModule(@Nonnull Module owningModule) {
		this.owningModule = owningModule;
	}

	public @Nonnull String getModuleRef() {
		return moduleRef;
	}
	
	public @Nonnull String getRef() {
		return ref;
	}
	
	@Override
	public @Nonnull String getName() {
		return (name == null) ? ref : name;
	}

	@Override
	public @Nonnull String getDescription() {
		return getTarget().getDescription();
	}
	
	@Override
	public @Nullable String getDocumentation() {
		return getTarget().getDocumentation();
	}

	@Override
	public int getTimeoutInSeconds() {
		return getTarget().getTimeoutInSeconds();
	}
	
	@Override
	public @Nonnull Map<String, String> getProperties() {
		return getTarget().getProperties();
	}
	
	protected abstract @Nonnull <T extends QueryDefinition> T getTarget();
	
	@Override
	public @Nonnull String toString() {
		return MoreObjects.toStringHelper(this)
							.add("name", name)
							.add("moduleRef", moduleRef)
							.add("ref", ref)
							.toString();
	}
}
