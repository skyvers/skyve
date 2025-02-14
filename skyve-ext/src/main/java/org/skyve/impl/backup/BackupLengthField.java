package org.skyve.impl.backup;

import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;

/**
 * Represents a length field in the backup process, storing metadata about an attribute's type, sensitivity level and maximum length.
 * 
 * @author simeonsolomou
 */
public class BackupLengthField extends BackupField {

	public Integer maxLength;

	/**
	 * Constructs a new {@code BackupField} with the specified attribute type, sensitivity and maximum length.
	 *
	 * @param attributeType - the type of the attribute.
	 * @param sensitivity - the sensitivity level of the attribute.
	 * @param maxLength - the maximum length of the attribute.
	 */
	public BackupLengthField(AttributeType attributeType, Sensitivity sensitivity, Integer maxLength) {
		super(attributeType, sensitivity);

		this.maxLength = maxLength;
	}

	public Integer getMaxLength() {
		return maxLength;
	}

	public void setMaxLength(Integer maxLength) {
		this.maxLength = maxLength;
	}
}