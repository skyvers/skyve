package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewDynamicImageUserAccessMetaData;

import jakarta.annotation.Nonnull;

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
	public FluentViewDynamicImageAccess(@Nonnull ViewDynamicImageUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewDynamicImageAccess from a runtime metadata.
	 */
	protected @Nonnull FluentViewDynamicImageAccess from(@Nonnull String imageName, @Nonnull Set<String> uxuis) {
		imageName(imageName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the image name for this FluentViewDynamicImageAccess.
	 */
	public @Nonnull FluentViewDynamicImageAccess imageName(final @Nonnull String imageName) {
		access.setImageName(imageName);
		return this;
	}

	/**
	 * Returns the underlying view user access metadata from this builder.
	 */
	@Override
	public @Nonnull ViewDynamicImageUserAccessMetaData get() {
		return access;
	}
}
