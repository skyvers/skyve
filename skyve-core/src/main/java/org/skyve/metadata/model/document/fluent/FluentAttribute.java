package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.AbstractAttribute;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Attribute.UsageType;

abstract class FluentAttribute<T extends FluentAttribute<T>> {
	protected FluentAttribute() {
		// nothing to see
	}

	@SuppressWarnings("unchecked")
	protected T from(Attribute attribute) {
		audited(attribute.isAudited());
		deprecated(attribute.isDeprecated());
// TODO		attribute.setDefaultInputWidget(null);
// TODO		attribute.setDefaultWidgetReference(null);
		description(attribute.getDescription());
		displayName(attribute.getDisplayName());
		documentation(attribute.getDocumentation());
		name(attribute.getName());
		trackChanges(attribute.isTrackChanges());
		transientAttribute(attribute.isTransient());
		usage(attribute.getUsage());
		sensitivity(attribute.getSensitivity());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T audited(boolean audited) {
		get().setAudited(audited);
		return (T) this;
	}
	
// TODO		attribute.setDefaultInputWidget(null);
// TODO		attribute.setDefaultWidgetReference(null);
	
	@SuppressWarnings("unchecked")
	public T deprecated(boolean deprecated) {
		get().setDeprecated(deprecated);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T description(String description) {
		get().setDescription(description);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T displayName(String displayName) {
		get().setDisplayName(displayName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T documentation(String documentation) {
		get().setDocumentation(documentation);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T trackChanges(boolean trackChanges) {
		get().setTrackChanges(trackChanges);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T transientAttribute(boolean transientAttribute) {
		get().setTransient(transientAttribute);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T usage(UsageType usage) {
		get().setUsage(usage);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T sensitivity(Sensitivity sensitivity) {
		get().setSensitivity(sensitivity);
		return (T) this;
	}

	public abstract AbstractAttribute get();
}
