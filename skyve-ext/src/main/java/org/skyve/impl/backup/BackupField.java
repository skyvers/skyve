package org.skyve.impl.backup;

import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;

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
	public BackupField(AttributeType attributeType, Sensitivity sensitivity) {
		this.attributeType = attributeType;
		this.sensitivity = sensitivity;
	}

	public AttributeType getAttributeType() {
		return attributeType;
	}

	public Sensitivity getSensitivity() {
		return sensitivity;
	}

	public void setSensitivity(Sensitivity sensitivity) {
		this.sensitivity = sensitivity;
	}
}
