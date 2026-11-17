package modules.test.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.model.document.ModuleDocument;

/**
 * Compile-time references to the documents declared in the test module.
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
	/** The "DeleteDuringPostDelete" document. */
	DELETE_DURING_POST_DELETE("DeleteDuringPostDelete"),
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
	/** The "MappedExtensionUniqueJoinedStrategy" document. */
	MAPPED_EXTENSION_UNIQUE_JOINED_STRATEGY("MappedExtensionUniqueJoinedStrategy"),
	/** The "MappedExtensionUniqueSingleStrategy" document. */
	MAPPED_EXTENSION_UNIQUE_SINGLE_STRATEGY("MappedExtensionUniqueSingleStrategy"),
	/** The "MappedSubclassedJoinedStrategy" document. */
	MAPPED_SUBCLASSED_JOINED_STRATEGY("MappedSubclassedJoinedStrategy"),
	/** The "MappedSubclassedSingleStrategy" document. */
	MAPPED_SUBCLASSED_SINGLE_STRATEGY("MappedSubclassedSingleStrategy"),
	/** The "MappedSubclassedUniqueJoinedStrategy" document. */
	MAPPED_SUBCLASSED_UNIQUE_JOINED_STRATEGY("MappedSubclassedUniqueJoinedStrategy"),
	/** The "MappedSubclassedUniqueSingleStrategy" document. */
	MAPPED_SUBCLASSED_UNIQUE_SINGLE_STRATEGY("MappedSubclassedUniqueSingleStrategy"),
	/** The "OverrideAdds" document. */
	OVERRIDE_ADDS("OverrideAdds"),
	/** The "OverrideEnumExtended" document. */
	OVERRIDE_ENUM_EXTENDED("OverrideEnumExtended"),
	/** The "OverrideEnumJoinedBase" document. */
	OVERRIDE_ENUM_JOINED_BASE("OverrideEnumJoinedBase"),
	/** The "OverrideEnumJoinedSub" document. */
	OVERRIDE_ENUM_JOINED_SUB("OverrideEnumJoinedSub"),
	/** The "OverrideEnumReferrer" document. */
	OVERRIDE_ENUM_REFERRER("OverrideEnumReferrer"),
	/** The "OverrideExtended" document. */
	OVERRIDE_EXTENDED("OverrideExtended"),
	/** The "OverrideJoinedBase" document. */
	OVERRIDE_JOINED_BASE("OverrideJoinedBase"),
	/** The "OverrideJoinedSub" document. */
	OVERRIDE_JOINED_SUB("OverrideJoinedSub"),
	/** The "OverrideLength" document. */
	OVERRIDE_LENGTH("OverrideLength"),
	/** The "OverrideLinkTarget" document. */
	OVERRIDE_LINK_TARGET("OverrideLinkTarget"),
	/** The "OverrideMappedBase" document. */
	OVERRIDE_MAPPED_BASE("OverrideMappedBase"),
	/** The "OverrideMappedSub" document. */
	OVERRIDE_MAPPED_SUB("OverrideMappedSub"),
	/** The "OverrideNarrowed" document. */
	OVERRIDE_NARROWED("OverrideNarrowed"),
	/** The "OverrideNarrowedExtended" document. */
	OVERRIDE_NARROWED_EXTENDED("OverrideNarrowedExtended"),
	/** The "OverrideNarrowedLinked" document. */
	OVERRIDE_NARROWED_LINKED("OverrideNarrowedLinked"),
	/** The "OverrideProxied" document. */
	OVERRIDE_PROXIED("OverrideProxied"),
	/** The "OverrideProxy" document. */
	OVERRIDE_PROXY("OverrideProxy"),
	/** The "OverrideReferrer" document. */
	OVERRIDE_REFERRER("OverrideReferrer"),
	/** The "OverrideRemoved" document. */
	OVERRIDE_REMOVED("OverrideRemoved"),
	/** The "OverrideSingleBase" document. */
	OVERRIDE_SINGLE_BASE("OverrideSingleBase"),
	/** The "OverrideSingleSub" document. */
	OVERRIDE_SINGLE_SUB("OverrideSingleSub"),
	/** The "Reachability" document. */
	REACHABILITY("Reachability"),
	/** The "UniqueConstraintMultipleNavigable" document. */
	UNIQUE_CONSTRAINT_MULTIPLE_NAVIGABLE("UniqueConstraintMultipleNavigable"),
	/** The "UniqueConstraintNonNullable" document. */
	UNIQUE_CONSTRAINT_NON_NULLABLE("UniqueConstraintNonNullable"),
	/** The "UniqueConstraintNullable" document. */
	UNIQUE_CONSTRAINT_NULLABLE("UniqueConstraintNullable"),
	/** The "UniqueConstraintOptimisation" document. */
	UNIQUE_CONSTRAINT_OPTIMISATION("UniqueConstraintOptimisation"),
	/** The "UniqueConstraintPersistent" document. */
	UNIQUE_CONSTRAINT_PERSISTENT("UniqueConstraintPersistent");

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
