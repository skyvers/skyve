package org.skyve.metadata;

/*
 * 
 */
/**
 * Marker interface for all Skyve metadata objects.
 *
 * <p>Every artifact that is declared in Skyve XML metadata and loaded into the
 * repository at runtime implements this interface. It serves as a common root
 * type for compile-time type checking and reflection-based tooling.
 *
 * @see SerializableMetaData
 * @see NamedMetaData
 */
public interface MetaData {
	// tagging interface
}
