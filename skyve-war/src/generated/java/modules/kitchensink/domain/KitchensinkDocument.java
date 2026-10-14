package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import org.skyve.metadata.model.document.ModuleDocument;

/**
 * Compile-time references to the documents declared in the kitchensink module.
 * Generated - local changes will be overwritten.
 */
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public enum KitchensinkDocument implements ModuleDocument {
	/** The "ContainerGrid" document. */
	CONTAINER_GRID("ContainerGrid"),
	/** The "DataRepeater" document. */
	DATA_REPEATER("DataRepeater"),
	/** The "EscapingFixture" document. */
	ESCAPING_FIXTURE("EscapingFixture"),
	/** The "InlineGrid" document. */
	INLINE_GRID("InlineGrid"),
	/** The "KitchenSink" document. */
	KITCHEN_SINK("KitchenSink"),
	/** The "ListAttributes" document. */
	LIST_ATTRIBUTES("ListAttributes"),
	/** The "LookupDescription" document. */
	LOOKUP_DESCRIPTION("LookupDescription"),
	/** The "OrderedGrid" document. */
	ORDERED_GRID("OrderedGrid"),
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
