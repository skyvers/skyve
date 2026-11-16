package org.skyve.impl.backup;

import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Represents a field in the backup process, storing metadata about an attribute's type and sensitivity level.
 * 
 * @author simeonsolomou
 */
public class BackupField {

	private final AttributeType attributeType;
	private Sensitivity sensitivity;

	/**
	 * Constructs a new {@code BackupField} with the specified attribute type and sensitivity.
	 *
	 * @param attributeType - the type of the attribute.
	 * @param sensitivity - the sensitivity level of the attribute.
	 */
	public BackupField(@Nonnull AttributeType attributeType, @Nullable Sensitivity sensitivity) {
		this.attributeType = attributeType;
		this.sensitivity = sensitivity;
	}

	public @Nonnull AttributeType getAttributeType() {
		return attributeType;
	}

	public @Nullable Sensitivity getSensitivity() {
		return sensitivity;
	}

	public void setSensitivity(@Nullable Sensitivity sensitivity) {
		this.sensitivity = sensitivity;
	}
}
