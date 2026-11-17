package customers.test.modules.test.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.model.document.ModuleDocument;

/**
 * Compile-time references to the documents declared in the test module for customer test.
 * Documents the module references from other modules are included and are addressed through this module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum TestDocument implements ModuleDocument {
	/** The "AllAttributesDynamicEmbedded" document. */
	ALL_ATTRIBUTES_DYNAMIC_EMBEDDED("AllAttributesDynamicEmbedded"),
	/** The "AllAttributesDynamicPersistent" document. */
	ALL_ATTRIBUTES_DYNAMIC_PERSISTENT("AllAttributesDynamicPersistent"),
	/** The "AllAttributesDynamicPersistentDynamicChild" document. */
	ALL_ATTRIBUTES_DYNAMIC_PERSISTENT_DYNAMIC_CHILD("AllAttributesDynamicPersistentDynamicChild"),
	/** The "AllAttributesEmbedded" document. */
	ALL_ATTRIBUTES_EMBEDDED("AllAttributesEmbedded"),
	/** The "AllAttributesPersistent" document. */
	ALL_ATTRIBUTES_PERSISTENT("AllAttributesPersistent"),
	/** The "AllAttributesRequiredPersistent" document. */
	ALL_ATTRIBUTES_REQUIRED_PERSISTENT("AllAttributesRequiredPersistent"),
	/** The "AllDynamicAttributesPersistent" document. */
	ALL_DYNAMIC_ATTRIBUTES_PERSISTENT("AllDynamicAttributesPersistent"),
	/** The "AllDynamicAttributesPersistentDynamicChild" document. */
	ALL_DYNAMIC_ATTRIBUTES_PERSISTENT_DYNAMIC_CHILD("AllDynamicAttributesPersistentDynamicChild"),
	/** The "AnyBase" document. */
	ANY_BASE("AnyBase"),
	/** The "AnyDerived1" document. */
	ANY_DERIVED1("AnyDerived1"),
	/** The "AnyDerived2" document. */
	ANY_DERIVED2("AnyDerived2"),
	/** The "ArcOneToMany" document. */
	ARC_ONE_TO_MANY("ArcOneToMany"),
	/** The "ArcOneToOne" document. */
	ARC_ONE_TO_ONE("ArcOneToOne"),
	/** The "DynamicMappedExtension" document. */
	DYNAMIC_MAPPED_EXTENSION("DynamicMappedExtension"),
	/** The "DynamicMappedSubclassed" document. */
	DYNAMIC_MAPPED_SUBCLASSED("DynamicMappedSubclassed"),
	/** The "Hierarchical" document. */
	HIERARCHICAL("Hierarchical"),
	/** The "InjectedDocument" document. */
	INJECTED_DOCUMENT("InjectedDocument"),
	/** The "InverseManyToManyPersistent" document. */
	INVERSE_MANY_TO_MANY_PERSISTENT("InverseManyToManyPersistent"),
	/** The "InverseOneToManyPersistent" document. */
	INVERSE_ONE_TO_MANY_PERSISTENT("InverseOneToManyPersistent"),
	/** The "InverseOneToOnePersistent" document. */
	INVERSE_ONE_TO_ONE_PERSISTENT("InverseOneToOnePersistent"),
	/** The "MappedBase" document. */
	MAPPED_BASE("MappedBase"),
	/** The "MappedExtensionJoinedStrategy" document. */
	MAPPED_EXTENSION_JOINED_STRATEGY("MappedExtensionJoinedStrategy"),
	/** The "MappedExtensionSingleStrategy" document. */
	MAPPED_EXTENSION_SINGLE_STRATEGY("MappedExtensionSingleStrategy"),
	/** The "MappedSubclassedJoinedStrategy" document. */
	MAPPED_SUBCLASSED_JOINED_STRATEGY("MappedSubclassedJoinedStrategy"),
	/** The "MappedSubclassedSingleStrategy" document. */
	MAPPED_SUBCLASSED_SINGLE_STRATEGY("MappedSubclassedSingleStrategy"),
	/** The "Reachability" document. */
	REACHABILITY("Reachability"),
	/** The "Test" document. */
	TEST("Test"),
	/** The "UniqueConstraintNonNullable" document. */
	UNIQUE_CONSTRAINT_NON_NULLABLE("UniqueConstraintNonNullable"),
	/** The "UniqueConstraintNullable" document. */
	UNIQUE_CONSTRAINT_NULLABLE("UniqueConstraintNullable"),
	/** The "UniqueConstraintOptimisation" document. */
	UNIQUE_CONSTRAINT_OPTIMISATION("UniqueConstraintOptimisation");

	private final String name;

	private TestDocument(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "test";
	}

	@Override
	public String documentName() {
		return name;
	}
}
