package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.model.document.ModuleDocument;

/**
 * Compile-time references to the documents declared in the kitchensink module.
 * Documents the module references from other modules are included and are addressed through this module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum KitchensinkDocument implements ModuleDocument {
	/** The "ActionsFixture" document. */
	ACTIONS_FIXTURE("ActionsFixture"),
	/** The "ActionsFixtureRow" document. */
	ACTIONS_FIXTURE_ROW("ActionsFixtureRow"),
	/** The "ConditionsFixture" document. */
	CONDITIONS_FIXTURE("ConditionsFixture"),
	/** The "ConditionsFixtureNode" document. */
	CONDITIONS_FIXTURE_NODE("ConditionsFixtureNode"),
	/** The "ConditionsFixtureRow" document. */
	CONDITIONS_FIXTURE_ROW("ConditionsFixtureRow"),
	/** The "ContainerGrid" document. */
	CONTAINER_GRID("ContainerGrid"),
	/** The "ConvertersDataTables" document. */
	CONVERTERS_DATA_TABLES("ConvertersDataTables"),
	/** The "ConvertersFixture" document. */
	CONVERTERS_FIXTURE("ConvertersFixture"),
	/** The "ConvertersFixtureRow" document. */
	CONVERTERS_FIXTURE_ROW("ConvertersFixtureRow"),
	/** The "DataRepeater" document. */
	DATA_REPEATER("DataRepeater"),
	/** The "DialogsFixture" document. */
	DIALOGS_FIXTURE("DialogsFixture"),
	/** The "DialogsFixtureRow" document. */
	DIALOGS_FIXTURE_ROW("DialogsFixtureRow"),
	/** The "EscapingFixture" document. */
	ESCAPING_FIXTURE("EscapingFixture"),
	/** The "InlineGrid" document. */
	INLINE_GRID("InlineGrid"),
	/** The "KitchenSink" document. */
	KITCHEN_SINK("KitchenSink"),
	/** The "ListAttributes" document. */
	LIST_ATTRIBUTES("ListAttributes"),
	/** The "ListGridFixture" document. */
	LIST_GRID_FIXTURE("ListGridFixture"),
	/** The "LookupDescription" document. */
	LOOKUP_DESCRIPTION("LookupDescription"),
	/** The "ModelLookupFixture" document. */
	MODEL_LOOKUP_FIXTURE("ModelLookupFixture"),
	/** The "ModelLookupRow" document. */
	MODEL_LOOKUP_ROW("ModelLookupRow"),
	/** The "OrderedGrid" document. */
	ORDERED_GRID("OrderedGrid"),
	/** The "OrderedGridFixture" document. */
	ORDERED_GRID_FIXTURE("OrderedGridFixture"),
	/** The "OrderedGridFixtureLine" document. */
	ORDERED_GRID_FIXTURE_LINE("OrderedGridFixtureLine"),
	/** The "OrderedGridFixtureRow" document. */
	ORDERED_GRID_FIXTURE_ROW("OrderedGridFixtureRow"),
	/** The "RerenderFixture" document. */
	RERENDER_FIXTURE("RerenderFixture"),
	/** The "UploadFixture" document. */
	UPLOAD_FIXTURE("UploadFixture"),
	/** The "UploadFixtureGridRow" document. */
	UPLOAD_FIXTURE_GRID_ROW("UploadFixtureGridRow");

	private final String name;

	private KitchensinkDocument(String name) {
		this.name = name;
	}

	@Override
	public String moduleName() {
		return "kitchensink";
	}

	@Override
	public String documentName() {
		return name;
	}
}
