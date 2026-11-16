package org.skyve.impl.backup;

import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Represents a length field in the backup process, storing metadata about an attribute's type, sensitivity level and maximum length.
 * 
 * @author simeonsolomou
 */
public class BackupLengthField extends BackupField {

	private final Integer maxLength;

	/**
	 * Constructs a new {@code BackupLengthField} with the specified attribute type, sensitivity and maximum length.
	 *
	 * @param attributeType - the type of the attribute.
	 * @param sensitivity - the sensitivity level of the attribute.
	 * @param maxLength - the maximum length of the attribute.
	 */
	public BackupLengthField(@Nonnull AttributeType attributeType, @Nullable Sensitivity sensitivity, @Nullable Integer maxLength) {
		super(attributeType, sensitivity);

		this.maxLength = maxLength;
	}

	public @Nullable Integer getMaxLength() {
		return maxLength;
	}
}