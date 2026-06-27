package org.skyve.impl.domain;

/**
 * Marker interface for generated enumeration types used as domain-value providers
 * in Skyve documents.
 *
 * <p>Each enum constant exposes a {@link #getCode()} (the stored/bound value) and
 * a {@link #getDescription()} (the display label shown in the UI). Generated domain
 * enumerations implement this interface so that the binding and converter layer can
 * treat all domain enums uniformly.
 */
public interface DomainEnum {
	public abstract String getCode();
	public abstract String getDescription();
}
