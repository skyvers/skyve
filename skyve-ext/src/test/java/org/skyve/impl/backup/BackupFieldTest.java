package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;

@SuppressWarnings("static-method")
class BackupFieldTest {

	@Test
	void constructorSetsAttributeTypeAndSensitivity() {
		BackupField field = new BackupField(AttributeType.text, Sensitivity.personal);
		assertEquals(AttributeType.text, field.getAttributeType());
		assertEquals(Sensitivity.personal, field.getSensitivity());
	}

	@Test
	void constructorAllowsNullSensitivity() {
		BackupField field = new BackupField(AttributeType.integer, null);
		assertEquals(AttributeType.integer, field.getAttributeType());
		assertNull(field.getSensitivity());
	}

	@Test
	void setSensitivityUpdatesSensitivity() {
		BackupField field = new BackupField(AttributeType.bool, Sensitivity.personal);
		field.setSensitivity(Sensitivity.restricted);
		assertEquals(Sensitivity.restricted, field.getSensitivity());
	}

	@Test
	void setSensitivityToNullClearsSensitivity() {
		BackupField field = new BackupField(AttributeType.text, Sensitivity.personal);
		field.setSensitivity(null);
		assertNull(field.getSensitivity());
	}

        // ---- BackupLengthField ----

        @Test
        void backupLengthFieldConstructorSetsMaxLength() {
                BackupLengthField lf = new BackupLengthField(AttributeType.text, Sensitivity.personal, Integer.valueOf(255));
                assertEquals(AttributeType.text, lf.getAttributeType());
                assertEquals(Sensitivity.personal, lf.getSensitivity());
                assertEquals(Integer.valueOf(255), lf.getMaxLength());
        }

        @Test
        void backupLengthFieldAllowsNullMaxLength() {
                BackupLengthField lf = new BackupLengthField(AttributeType.integer, null, null);
                assertNull(lf.getMaxLength());
        }
}
