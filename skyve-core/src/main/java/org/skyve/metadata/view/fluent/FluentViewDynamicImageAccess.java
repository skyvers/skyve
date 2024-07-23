package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewDynamicImageUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ViewDynamicImageUserAccessMetaData} metadata.
 * 
 * @author mike
 */
public class FluentViewDynamicImageAccess extends FluentViewUserAccess<FluentViewDynamicImageAccess, ViewDynamicImageUserAccessMetaData> {
	/**
	 * Creates a new FluentViewDynamicImageAccess
	 */
	public FluentViewDynamicImageAccess() {
		access = new ViewDynamicImageUserAccessMetaData();
	}

	/**
	 * Creates a new FluentViewDynamicImageAccess from the specified ViewDynamicImageUserAccessMetaData.
	 */
	public FluentViewDynamicImageAccess(ViewDynamicImageUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewDynamicImageAccess from a runtime metadata.
	 */
	protected FluentViewDynamicImageAccess from(String imageName, Set<String> uxuis) {
		imageName(imageName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the image name for this FluentViewDynamicImageAccess.
	 */
	public FluentViewDynamicImageAccess imageName(final String imageName) {
		access.setImageName(imageName);
		return this;
	}

	/**
	 * Returns the underlying view user access metadata from this builder.
	 */
	@Override
	public ViewDynamicImageUserAccessMetaData get() {
		return access;
	}
}
