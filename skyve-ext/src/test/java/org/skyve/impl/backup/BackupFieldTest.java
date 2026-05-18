package org.skyve.impl.backup;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;

@SuppressWarnings("static-method")
public class BackupFieldTest {

	@Test
	public void constructorSetsAttributeTypeAndSensitivity() {
		BackupField field = new BackupField(AttributeType.text, Sensitivity.personal);
		assertEquals(AttributeType.text, field.getAttributeType());
		assertEquals(Sensitivity.personal, field.getSensitivity());
	}

	@Test
	public void constructorAllowsNullSensitivity() {
		BackupField field = new BackupField(AttributeType.integer, null);
		assertEquals(AttributeType.integer, field.getAttributeType());
		assertNull(field.getSensitivity());
	}

	@Test
	public void setSensitivityUpdatesSensitivity() {
		BackupField field = new BackupField(AttributeType.bool, Sensitivity.personal);
		field.setSensitivity(Sensitivity.restricted);
		assertEquals(Sensitivity.restricted, field.getSensitivity());
	}

	@Test
	public void setSensitivityToNullClearsSensitivity() {
		BackupField field = new BackupField(AttributeType.text, Sensitivity.personal);
		field.setSensitivity(null);
		assertNull(field.getSensitivity());
	}
}
